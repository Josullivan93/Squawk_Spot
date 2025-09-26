# server.R: Contains the core logic of the Shiny application.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(here, shiny, dplyr, data.table, seewave, tuneR, shinyjs, plotly)
source(here("helper.R"))

options(shiny.maxRequestSize = 30*1024^2)

# Server Definition
server <- function(input, output, session) {
  
  # reactiveValues object for storing data
  data_storage <- reactiveValues(
    features = NULL,
    current_run = 1,
    files_to_classify = NULL,
    full_wave_object = NULL,
    runs_table = NULL
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
  
  # Combined Reactive: chunk info + audio data
  current_chunk_full <- reactive({
    req(data_storage$files_to_classify, data_storage$current_run)
    run_idx <- data_storage$current_run
    
    # Ensure index is valid before proceeding
    req(run_idx > 0, run_idx <= length(data_storage$files_to_classify))
    
    chunk_path <- data_storage$files_to_classify[run_idx]
    req(file.exists(here(chunk_path)))
    
    # Read WAV
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    duration <- seewave::duration(chunk_wave)
    
    # Sanitize the audio signal
    signal_sanitized <- chunk_wave@left
    signal_sanitized[!is.finite(signal_sanitized)] <- 0
    
    # Waveform (oscillo)
    osc_data <- seewave::oscillo(signal_sanitized, f = chunk_wave@samp.rate, plot = FALSE)
    time_vector <- seq(0, duration, length.out = nrow(osc_data))
    
    # Spectrogram
    spec_data <- seewave::spectro(signal_sanitized, f = chunk_wave@samp.rate,
                                  plot = FALSE, osc = FALSE)
    
    # Determine chunk_start (slice_start)
    chunk_start <- 0
    if (!is.null(data_storage$runs_table) && nrow(data_storage$runs_table) >= run_idx) {
      if ("slice_start" %in% names(data_storage$runs_table)) {
        ss <- data_storage$runs_table$slice_start[run_idx]
        if (!is.na(ss)) chunk_start <- as.numeric(ss)
      } else {
        ss2 <- data_storage$runs_table$start_time[run_idx]
        if (!is.na(ss2)) chunk_start <- as.numeric(ss2)
      }
    }
    
    # Highlight times relative to chunk_start
    highlight <- NULL
    times <- current_run_times()
    if (!is.null(times)) {
      start_rel <- max(0, min(duration, times$start - chunk_start))
      end_rel   <- max(0, min(duration, times$end   - chunk_start))
      highlight <- if (end_rel > start_rel) list(start = start_rel, end = end_rel) else list(start = start_rel, end = start_rel)
    }
    
    list(
      chunk_path  = chunk_path,
      chunk_start = chunk_start,
      highlight   = highlight,
      wave        = chunk_wave,
      duration    = duration,
      osc_data    = osc_data,
      time_vector = time_vector,
      spec_data   = spec_data
    )
  })
  
# DEFINE PLOT OUTPUTS
  
  output$waveform_plot <- renderPlotly({
    req(current_chunk_full())
    chunk <- current_chunk_full()
    
    y_range_wave <- range(chunk$osc_data[,1], na.rm = TRUE)
    if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)
    
    shapes_base <- list()
    if (!is.null(chunk$highlight)) {
      shapes_base[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(255, 255, 0, 0.3)", line = list(width = 0)
      )
    }
    
    plot_ly(x = chunk$time_vector, y = chunk$osc_data[,1], type = 'scatter', mode = 'lines', hoverinfo = 'x+y', name = "Waveform") %>%
      layout(
        xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
        yaxis = list(range = y_range_wave, fixedrange = TRUE, title = "Amplitude"),
        shapes = shapes_base
      )
  })
  
  output$spectrogram_plot <- renderPlotly({
    req(current_chunk_full())
    chunk <- current_chunk_full()
    
    amp_db <- chunk$spec_data$amp
    finite_vals <- amp_db[is.finite(amp_db)]
    min_db <- if(length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100 # Safe default
    amp_db[!is.finite(amp_db)] <- min_db
    
    shapes_base <- list()
    if (!is.null(chunk$highlight)) {
      shapes_base[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(255, 255, 0, 0.3)", line = list(width = 0)
      )
    }
    
    plot_ly(z = amp_db, x = chunk$spec_data$time, y = chunk$spec_data$freq, type = 'heatmap', colors = 'viridis', hoverinfo = 'x+y+z', showscale = FALSE) %>%
      layout(
        xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
        yaxis = list(title = "Frequency (kHz)"),
        shapes = shapes_base
      )
  })
  
  # Update only playhead (fast)
  observeEvent(playhead_time(), {
    req(current_chunk_full()) # Ensure chunk data exists before updating
    chunk <- current_chunk_full()
    
    shapes <- list()
    if (!is.null(chunk$highlight)) {
      shapes[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(255, 255, 0, 0.3)", line = list(width = 0)
      )
    }
    
    ph <- max(0, min(chunk$duration, as.numeric(playhead_time())))
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = ph, x1 = ph, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = 'red', width = 2)
    )
    
    plotlyProxy("waveform_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes = shapes))
    plotlyProxy("spectrogram_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes = shapes))
  })
  
  # Update progress bar
  observeEvent(data_storage$current_run, {
    req(data_storage$files_to_classify)
    
    progress <- (data_storage$current_run - 1) / length(data_storage$files_to_classify) * 100
    
    # Use shinyjs to update the style of the progress bar
    shinyjs::runjs(paste0(
      "document.getElementById('progress_bar').style.width = '", progress, "%';"
    ))
  })
  
  # Temp directory & resource path
  temp_dir <- here("Output", "tmp")
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  addResourcePath("temp_audio", temp_dir)
  
  # UI Update Logic
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
  
  observeEvent(data_storage$features, {
    if (!is.null(data_storage$features)) shinyjs::show("main_ui")
  })
  
  # File Processing and Chunking
  observeEvent(input$process_btn, {
    req(input$upload_file)
    showModal(modalDialog(
      div(
        style = "text-align: center;",
        
        # Add the 'class' argument here
        tags$img(src = "loader.gif", height = "140px", width = "140px", class = "circular-image"),
        
        p("Processing File...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
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
    
    showNotification("Processing complete!", type = "message")
    
    shinyjs::hide(id = "pre_process_sidebar")
    shinyjs::show(id = "post_process_sidebar")
    shinyjs::hide(id = "main_placeholder")
    shinyjs::show(id = "main_ui")
  })
  
  # Resume previous session
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
  
  # Navigation & Classification
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
  
  # Navigation & Classification
  
  observeEvent(input$btn_next, { data_storage$current_run <- data_storage$current_run + 1 })
  observeEvent(input$btn_prev, { data_storage$current_run <- max(1, data_storage$current_run - 1) })
  
  # Audio Player UI
  output$audio_player <- renderUI({
    req(current_chunk_full())
    chunk <- current_chunk_full()
    
    audio_tag <- tags$audio(
      id = "audio_element",
      src = file.path("temp_audio", basename(chunk$chunk_path)),
      type = "audio/wav",
      autoplay = TRUE,
      controls = TRUE
    )
    # JS for frequent time updates
    js_script <- tags$script(HTML("
      var audio_player = document.getElementById('audio_element');
      if (window.updateTimer) clearInterval(window.updateTimer);
      if (audio_player) {
        const updatePlayhead = () => { Shiny.setInputValue('current_time', audio_player.currentTime, {priority: 'event'}); };
        const startTimer = () => { if (!window.updateTimer) window.updateTimer = setInterval(updatePlayhead, 50); };
        const stopTimer = () => { if (window.updateTimer) { clearInterval(window.updateTimer); window.updateTimer = null; } };
        audio_player.addEventListener('play', startTimer);
        audio_player.addEventListener('playing', startTimer);
        audio_player.addEventListener('pause', stopTimer);
        audio_player.addEventListener('ended', stopTimer);
      }
    "))
    tagList(audio_tag, js_script)
  })
  
  observeEvent(input$current_time, { playhead_time(input$current_time) })
  
  # File info + progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify, data_storage$current_run)
    total_runs <- length(data_storage$files_to_classify)
    current_run_idx <- data_storage$current_run
    req(current_run_idx <= total_runs) # Ensure run index is valid
    current_file_name <- basename(here(data_storage$files_to_classify[current_run_idx]))
    paste0("Run ", current_run_idx, " of ", total_runs, ": ", current_file_name)
  })
  
  session$onSessionEnded(function() { stopApp() })
}