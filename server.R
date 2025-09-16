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
  
  # === Reactives to keep track of current run ===
  current_run_id <- reactiveVal(NULL)
  
  # Create a temporary directory for storing sliced files and data
  temp_dir <- here("Output", "tmp")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  # Map temporary directory to a web-accessible URL
  addResourcePath("temp_audio", temp_dir)
  
  # --- UI Update Logic ---
  observeEvent(input$upload_file, {
    shinyjs::hide("main_ui")
  })
  
  observeEvent(data_storage$features, {
    if (!is.null(data_storage$features)) {
      shinyjs::show("main_ui")
    }
  })
  
  # --- File Processing and Chunking ---
  observeEvent(input$process_btn, {
    req(input$upload_file)
    
    showModal(modalDialog(
      title = "Processing File",
      "Please wait while the audio is being processed and sliced.",
      footer = NULL
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
    data_storage$files_to_classify <- chunking_results$file_paths
    data_storage$current_run <- 1
    
    removeModal()
  })
  
  # Resume previous session
  observeEvent(input$resume_btn, {
    if (file.exists(file.path(temp_dir, "features.csv")) && file.exists(file.path(temp_dir, "full_wave.RData"))) {
      data_storage$features <- fread(file.path(temp_dir, "features.csv"))
      load(file.path(temp_dir, "full_wave.RData"))
      data_storage$full_wave_object <- full_wave
      
      files <- list.files(temp_dir, pattern = "\\.wav$", full.names = TRUE)
      data_storage$files_to_classify <- files
      data_storage$current_run <- 1
    }
  })
  
  # --- Navigation and Classification ---
  observeEvent(list(input$btn_squawk, input$hotkey_1), {
    if (isTRUE(input$btn_squawk > 0) || isTRUE(input$hotkey_1 > 0)) {
      current_run <- data_storage$current_run
      current_run_id <- data_storage$features$run_id[current_run]
      
      data_storage$features <- classify_and_move(
        label = "Squawk",
        run_id = current_run_id,
        features_df = data_storage$features,
        output_dir = temp_dir
      )
      
      data_storage$current_run <- current_run + 1
    }
  })
  
  observeEvent(list(input$btn_other, input$hotkey_2), {
    if (isTRUE(input$btn_other > 0) || isTRUE(input$hotkey_2 > 0)) {
      current_run <- data_storage$current_run
      current_run_id <- data_storage$features$run_id[current_run]
      
      data_storage$features <- classify_and_move(
        label = "Other Vocalisations",
        run_id = current_run_id,
        features_df = data_storage$features,
        output_dir = temp_dir
      )
      
      data_storage$current_run <- current_run + 1
    }
  })
  
  observeEvent(list(input$btn_unknown, input$hotkey_3), {
    if (isTRUE(input$btn_unknown > 0) || isTRUE(input$hotkey_3 > 0)) {
      current_run <- data_storage$current_run
      current_run_id <- data_storage$features$run_id[current_run]
      
      data_storage$features <- classify_and_move(
        label = "Unknown",
        run_id = current_run_id,
        features_df = data_storage$features,
        output_dir = temp_dir
      )
      
      data_storage$current_run <- current_run + 1
    }
  })
  
  observeEvent(list(input$btn_noise, input$hotkey_4), {
    if (isTRUE(input$btn_noise > 0) || isTRUE(input$hotkey_4 > 0)) {
      current_run <- data_storage$current_run
      current_run_id <- data_storage$features$run_id[current_run]
      
      data_storage$features <- classify_and_move(
        label = "Noise",
        run_id = current_run_id,
        features_df = data_storage$features,
        output_dir = temp_dir
      )
      
      data_storage$current_run <- current_run + 1
    }
  })
  
  observeEvent(input$btn_next, {
    data_storage$current_run <- data_storage$current_run + 1
  })
  
  observeEvent(input$btn_prev, {
    data_storage$current_run <- max(1, data_storage$current_run - 1)
  })
  
  # --- Output Rendering ---
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
    
    # --- JAVASCRIPT SECTION (restored) ---
    js_script <- tags$script(HTML("
      var audio_player = document.getElementById('audio_element');
      if (window.updateTimer) {
        clearInterval(window.updateTimer);
      }
      if (audio_player) {
        const startTimer = function() {
          if (window.updateTimer) { clearInterval(window.updateTimer); }
          window.updateTimer = setInterval(function() {
            Shiny.setInputValue('current_time', audio_player.currentTime, {priority: 'event'});
          }, 50);
        };
        const stopTimer = function() {
          clearInterval(window.updateTimer);
        };
        audio_player.addEventListener('play', startTimer);
        audio_player.addEventListener('playing', startTimer);
        audio_player.addEventListener('pause', stopTimer);
        audio_player.addEventListener('ended', stopTimer);
      }
    "))
    
    tagList(audio_tag, js_script)
  })
  
  # Waveform plot
  output$waveform_plot <- renderPlotly({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_run]
    req(file.exists(here(chunk_path)))
    
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    oscillo_plot <- seewave::oscillo(chunk_wave@left, f = chunk_wave@samp.rate, title = "Waveform", plot = FALSE)
    req(is.matrix(oscillo_plot))
    
    time_vector <- seq(0, seewave::duration(chunk_wave), length.out = nrow(oscillo_plot))
    plot_ly(
      x = time_vector,
      y = oscillo_plot[, 1],
      type = 'scatter',
      mode = 'lines',
      name = "Waveform"
    ) %>%
      layout(xaxis = list(title = "Time (s)"), yaxis = list(title = "Amplitude"))
  })
  
  # Spectrogram plot
  output$spectrogram_plot <- renderPlotly({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_run]
    req(file.exists(here(chunk_path)))
    
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    spectro_plot <- seewave::spectro(chunk_wave@left, f = chunk_wave@samp.rate,
                                     main = "Spectrogram", plot = FALSE, osc = FALSE,
                                     tlim = c(0, seewave::duration(chunk_wave)))
    req(is.list(spectro_plot))
    
    plot_ly(
      z = spectro_plot$amp,
      x = spectro_plot$time,
      y = spectro_plot$freq,
      type = 'heatmap',
      colors = 'viridis',
      hoverinfo = 'x+y+z',
      showscale = FALSE
    ) %>%
      layout(xaxis = list(title = "Time (s)"), yaxis = list(title = "Frequency (kHz)"))
  })
  
  # Sync playhead with plots
  observeEvent(input$current_time, {
    playhead_time(input$current_time)
  })
  
  observeEvent(playhead_time(), {
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_run]
    req(file.exists(here(chunk_path)))
    
    chunk_wave <- readWave(here(chunk_path))
    duration <- seewave::duration(chunk_wave)
    
    plotlyProxy("waveform_plot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = list(list(
          type = 'line',
          x0 = playhead_time(), x1 = playhead_time(),
          y0 = 0, y1 = 1, yref = 'paper',
          line = list(color = 'red', dash = 'dash')
        ))
      ))
    
    plotlyProxy("spectrogram_plot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = list(list(
          type = 'line',
          x0 = playhead_time(), x1 = playhead_time(),
          y0 = 0, y1 = 1, yref = 'paper',
          line = list(color = 'red', dash = 'dash')
        ))
      ))
  }, ignoreInit = TRUE)
  
  # File info + progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify)
    total_runs <- length(data_storage$files_to_classify)
    current_file_name <- basename(here(data_storage$files_to_classify[data_storage$current_run]))
    paste0("Run ", data_storage$current_run, " of ", total_runs, ": ", current_file_name)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
