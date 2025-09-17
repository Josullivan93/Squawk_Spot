# server.R: Contains the core logic of the Shiny application.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(here, shiny, dplyr, data.table, seewave, tuneR, shinyjs, plotly)
source(here("helper.R"))

options(shiny.maxRequestSize = 30*1024^2)

# --- Server Definition ---
server <- function(input, output, session) {
  
  # reactiveValues object for storing data
  data_storage <- reactiveValues(
    features = NULL,
    current_run = 1,
    files_to_classify = NULL,
    full_wave_object = NULL
  )
  
  # Reactive value for the playhead time
  playhead_time <- reactiveVal(0)
  
  # Reactive to store current start/end time of run
  current_run_times <- reactive({
    req(data_storage$runs_table, data_storage$current_run)
    run_idx <- data_storage$current_run
    if(run_idx > nrow(data_storage$runs_table)) return(NULL)
    list(
      start = data_storage$runs_table$start_time[run_idx],
      end   = data_storage$runs_table$end_time[run_idx]
    )
  })
  
  # --- Combined Reactive: chunk info + audio data ---
  current_chunk_full <- reactive({
    req(data_storage$files_to_classify)
    run_idx <- data_storage$current_run
    chunk_path <- data_storage$files_to_classify[run_idx]
    req(file.exists(here(chunk_path)))
    
    # Read WAV
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    duration <- seewave::duration(chunk_wave)
    
    # Waveform
    osc_data <- seewave::oscillo(chunk_wave@left, f = chunk_wave@samp.rate, plot = FALSE)
    time_vector <- seq(0, duration, length.out = nrow(osc_data))
    
    # Spectrogram
    spec_data <- seewave::spectro(chunk_wave@left, f = chunk_wave@samp.rate,
                                  plot = FALSE, osc = FALSE, tlim = c(0, duration))
    
    # Highlight times relative to sliced chunk
    highlight <- NULL
    times <- current_run_times()
    if(!is.null(times)) {
      # Determine chunk_start from runs table
      chunk_start <- 0
      if(!is.null(data_storage$runs_table)) {
        slice_start <- data_storage$runs_table$start_time[run_idx]
        if(!is.null(slice_start)) chunk_start <- slice_start
      }
      highlight <- list(
        start = times$start - chunk_start,
        end   = times$end - chunk_start
      )
    }
    
    # Return all chunk info
    list(
      chunk_path = chunk_path,
      highlight = highlight,
      wave = chunk_wave,
      duration = duration,
      osc_data = osc_data,
      time_vector = time_vector,
      spec_data = spec_data
    )
  })
  
  # --- Render plots once per chunk ---
  observeEvent(current_chunk_full(), {
    chunk <- current_chunk_full()
    
    # compute waveform & spectrogram ranges
    y_range_wave <- range(chunk$osc_data[,1], na.rm = TRUE)
    y_range_spec <- range(chunk$spec_data$freq, na.rm = TRUE)
    
    # Shapes (highlight + playhead)
    shapes <- generate_shapes(highlight = chunk$highlight)
    
    # Waveform plot
    plotlyProxy("waveform_plot", session) %>%
      plotlyProxyInvoke("restyle", list(x = list(chunk$time_vector), y = list(chunk$osc_data[,1]))) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = shapes,
        xaxis = list(range = c(0, chunk$duration)),
        yaxis = list(range = y_range_wave)
      ))
    
    # Spectrogram plot
    plotlyProxy("spectrogram_plot", session) %>%
      plotlyProxyInvoke("restyle", list(
        z = list(t(chunk$spec_data$amp)),  # transpose
        x = list(chunk$spec_data$time),
        y = list(chunk$spec_data$freq)
      )) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = shapes,
        xaxis = list(range = c(0, chunk$duration)),
        yaxis = list(range = y_range_spec)
      ))
  })
  
  # --- Update only playhead ---
  observeEvent(playhead_time(), {
    chunk <- current_chunk_full()
    shapes <- generate_shapes(highlight = chunk$highlight, playhead = playhead_time())
    
    plotlyProxy("waveform_plot", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = shapes))
    
    plotlyProxy("spectrogram_plot", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = shapes))
  })
  
  # --- Temp directory & resource path ---
  temp_dir <- here("Output", "tmp")
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  addResourcePath("temp_audio", temp_dir)
  
  # --- UI Update Logic ---
  observeEvent(input$upload_file, {
    shinyjs::hide("main_ui")
    temp_dir <- here("Output", "tmp")
    if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
    
    old_files <- list.files(temp_dir, full.names = TRUE)
    if (length(old_files) > 0) {
      features_path <- file.path(temp_dir, "features.csv")
      if (file.exists(features_path)) {
        old_dir <- here("Old")
        if (!dir.exists(old_dir)) dir.create(old_dir, recursive = TRUE)
        old_csv <- file.path(old_dir, paste0("Old_features_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
        old_moved <- file.rename(features_path, old_csv)
        if (!old_moved) {
          copied <- file.copy(features_path, old_csv, overwrite = TRUE)
          if (!copied) stop("Failed to move or copy old features.csv to ", old_csv)
          unlink(features_path)
        }
      }
      unlink(old_files, recursive = TRUE)
    }
  })
  
  observeEvent(data_storage$features, {
    if (!is.null(data_storage$features)) shinyjs::show("main_ui")
  })
  
  # --- File Processing and Chunking ---
  observeEvent(input$process_btn, {
    req(input$upload_file)
    showModal(modalDialog(title = "Processing File", "Please wait while the audio is being processed and sliced.", footer = NULL))
    
    processing_results <- process_wav(
      wav_path = input$upload_file$datapath,
      stationary_filter = input$stationary_filter,
      nonstationary_filter = input$nonstationary_filter,
      low_pass_hz = input$low_pass_hz,
      high_pass_hz = input$high_pass_hz,
      window_size = input$window_size,
      window_overlap = input$window_overlap
    )
    
    data_storage$full_wave_object <- processing_results$proc_wav
    
    chunking_results <- group_and_slice_chunks(
      features_df = processing_results$features_df,
      full_wave = data_storage$full_wave_object,
      positive_class = "Squawk",
      buffer_time = 1.0,
      temp_dir = temp_dir
    )
    
    data_storage$features <- chunking_results$updated_features_df
    data_storage$runs_table <- chunking_results$runs_table
    data_storage$files_to_classify <- chunking_results$file_paths
    data_storage$current_run <- 1
    removeModal()
  })
  
  # --- Resume previous session ---
  observeEvent(input$resume_btn, {
    data_storage$features <- fread(file.path(temp_dir, "features.csv"))
    if (file.exists(file.path(temp_dir, "runs.csv"))) {
      data_storage$runs_table <- fread(file.path(temp_dir, "runs.csv"))
      data_storage$files_to_classify <- data_storage$runs_table$filepath
    } else {
      data_storage$runs_table <- NULL
      data_storage$files_to_classify <- list.files(temp_dir, pattern="\\.wav$", full.names=TRUE)
    }
  })
  
  # --- Navigation & Classification ---
  observeEvent(list(input$btn_squawk, input$hotkey_1), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Squawk",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1
  })
  
  observeEvent(list(input$btn_other, input$hotkey_2), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Other Vocalisations",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1
  })
  
  observeEvent(list(input$btn_unknown, input$hotkey_3), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Unknown",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1
  })
  
  observeEvent(list(input$btn_noise, input$hotkey_4), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Noise",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1
  })
  
  observeEvent(input$btn_next, { data_storage$current_run <- data_storage$current_run + 1 })
  observeEvent(input$btn_prev, { data_storage$current_run <- max(1, data_storage$current_run - 1) })
  
  # --- Audio Player UI ---
  output$audio_player <- renderUI({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_run]
    req(file.exists(here(chunk_path)))
    audio_tag <- tags$audio(
      id = "audio_element",
      src = file.path("temp_audio", basename(chunk_path[[1]])),
      type = "audio/wav",
      autoplay = TRUE,
      controls = TRUE
    )
    js_script <- tags$script(HTML("
      var audio_player = document.getElementById('audio_element');
      if (window.updateTimer) clearInterval(window.updateTimer);
      if (audio_player) {
        const updatePlayhead = function() {
          Shiny.setInputValue('current_time', audio_player.currentTime, {priority: 'event'});
        };
  
        const startTimer = function() {
        if (!window.updateTimer) {
          window.updateTimer = setInterval(updatePlayhead, 50);
        }
      };
  
      const stopTimer = function() {
        if (window.updateTimer) {
          clearInterval(window.updateTimer);
          window.updateTimer = null;
        }
      };
  
    audio_player.addEventListener('play', startTimer);
    audio_player.addEventListener('playing', startTimer);
    audio_player.addEventListener('pause', stopTimer);
    audio_player.addEventListener('ended', stopTimer);
  }
                                  "))
    tagList(audio_tag, js_script)
  })
  
  observeEvent(input$current_time, { playhead_time(input$current_time) })
  
  # --- Initial empty plots ---
  output$waveform_plot <- renderPlotly({
    plot_ly(x = numeric(0), y = numeric(0), type = 'scatter', mode = 'lines', name = "Waveform") %>%
      layout(xaxis = list(title="Time (s)"), yaxis=list(title="Amplitude"), shapes=list())
  })
  output$spectrogram_plot <- renderPlotly({
    plot_ly(z = matrix(0,2,2), x=c(0,1), y=c(0,1), type='heatmap', colors='viridis', showscale=FALSE) %>%
      layout(xaxis=list(title="Time (s)"), yaxis=list(title="Frequency (kHz)"), shapes=list())
  })
  
  # --- Reactive: chunk shapes ---
  chunk_shapes <- reactive({
    chunk <- current_chunk_full()
    list(
      # Highlight
      list(
        type="rect",
        x0 = if(!is.null(chunk$highlight)) chunk$highlight$start else 0,
        x1 = if(!is.null(chunk$highlight)) chunk$highlight$end else 0,
        y0=0, y1=1, yref="paper",
        fillcolor="rgba(255,255,0,0.3)",
        line=list(width=0)
      ),
      # Playhead (x0=x1, updated separately)
      list(
        type="line",
        x0=0, x1=0, y0=0, y1=1, yref="paper",
        line=list(color="red", width=2)
      )
    )
  })
  
  # --- Render plots once per chunk ---
  observeEvent(current_chunk_full(), {
    chunk <- current_chunk_full()
    shapes <- chunk_shapes()
    plotlyProxy("waveform_plot", session) %>%
      plotlyProxyInvoke("restyle", list(x=list(chunk$time_vector), y=list(chunk$osc_data[,1]))) %>%
      plotlyProxyInvoke("relayout", list(shapes=shapes))
    plotlyProxy("spectrogram_plot", session) %>%
      plotlyProxyInvoke("restyle", list(z=list(chunk$spec_data$amp), x=list(chunk$spec_data$time), y=list(chunk$spec_data$freq))) %>%
      plotlyProxyInvoke("relayout", list(shapes=shapes))
  })
  
  # --- Update only playhead ---
  observeEvent(playhead_time(), {
    shapes <- chunk_shapes()
    shapes[[2]]$x0 <- playhead_time()
    shapes[[2]]$x1 <- playhead_time()
    plotlyProxy("waveform_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes=shapes))
    plotlyProxy("spectrogram_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes=shapes))
  })
  
  # --- File info + progress ---
  output$file_info <- renderText({
    req(data_storage$files_to_classify)
    total_runs <- length(data_storage$files_to_classify)
    current_file_name <- basename(here(data_storage$files_to_classify[data_storage$current_run]))
    paste0("Run ", data_storage$current_run, " of ", total_runs, ": ", current_file_name)
  })
  
  session$onSessionEnded(function() { stopApp() })
}