# server.R: Contains the core logic of the Shiny application.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(here, shiny, dplyr, data.table, seewave, tuneR, shinyjs, plotly)
source(here("helper.R"))

options(shiny.maxRequestSize=30*1024^2)

# --- Server Definition ---
server <- function(input, output, session) {
  
  # reactiveValues object for storing data
  data_storage <- reactiveValues(
    features = NULL,
    current_chunk = 1,
    files_to_classify = NULL,
    full_wave_object = NULL
  )
  
  # Reactive value for the playhead time
  playhead_time <- reactiveVal(0)
  
  # Create a temporary directory for storing sliced files and data
  temp_dir <- here("Output", "tmp")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  # Map temporary directory to a web-accessible URL
  addResourcePath("temp_audio", temp_dir)
  
  # --- UI Update Logic ---
  
  # Hide the main UI until a file is uploaded or a session is resumed
  observeEvent(input$upload_file, {
    shinyjs::hide("main_ui")
  })
  
  observeEvent(data_storage$features, {
    if (!is.null(data_storage$features)) {
      shinyjs::show("main_ui")
    }
  })
  
  # --- File Processing and Chunking ---
  
  # Main logic for processing the uploaded file
  observeEvent(input$process_btn, {
    req(input$upload_file)
    
    # Show processing message
    showModal(modalDialog(
      title = "Processing File",
      "Please wait while the audio is being processed and sliced.",
      footer = NULL
    ))
    
    # Process the full wave file and get features
    processing_results <- process_wav(
      wav_path = input$upload_file$datapath,
      stationary_filter = input$stationary_filter,
      nonstationary_filter = input$nonstationary_filter,
      low_pass_hz = input$low_pass_hz,
      high_pass_hz = input$high_pass_hz,
      window_size = input$window_size,
      window_overlap = input$window_overlap
    )
    
    # Store the full wave object
    data_storage$full_wave_object <- processing_results$proc_wav
    
    # Slice the file into chunks based on the auto-classification
    chunking_results <- group_and_slice_chunks(
      features_df = processing_results$features_df,
      full_wave = data_storage$full_wave_object,
      positive_class = "Squawk", # Hard-coded positive class
      buffer_time = 1.0,         # Hard-coded buffer time
      temp_dir = temp_dir
    )
    
    # Update reactive values
    data_storage$features <- chunking_results$updated_features_df
    data_storage$files_to_classify <- chunking_results$file_paths
    data_storage$current_chunk <- 1
    
    # Hide the modal and proceed
    removeModal()
  })
  
  # Resume previous session
  observeEvent(input$resume_btn, {
    if (file.exists(file.path(temp_dir, "features.csv")) && file.exists(file.path(temp_dir, "full_wave.RData"))) {
      # Load saved features data
      data_storage$features <- fread(file.path(temp_dir, "features.csv"))
      
      # Load the full wave object
      load(file.path(temp_dir, "full_wave.RData"))
      data_storage$full_wave_object <- full_wave
      
      # Get the list of remaining chunks to classify
      files <- list.files(temp_dir, pattern = "\\.wav$", full.names = TRUE)
      data_storage$files_to_classify <- files
      data_storage$current_chunk <- 1
    }
  })
  
  # --- Navigation and Classification ---
  
  # Event handlers for classification buttons and hotkeys
  observeEvent(list(input$btn_squawk, input$hotkey_1), {
    # Check if a click or hotkey event was actually triggered
    if (isTRUE(input$btn_squawk > 0) || isTRUE(input$hotkey_1 > 0)) {
      current_path <- data_storage$files_to_classify[data_storage$current_chunk]
      classify_and_move(
        classification = "Squawk",
        chunk_path = current_path,
        features_df = data_storage$features
      )
      data_storage$current_chunk <- data_storage$current_chunk + 1
    }
  })
  
  observeEvent(list(input$btn_other, input$hotkey_2), {
    if (isTRUE(input$btn_other > 0) || isTRUE(input$hotkey_2 > 0)) {
      current_path <- data_storage$files_to_classify[data_storage$current_chunk]
      classify_and_move(
        classification = "Other Vocalisation",
        chunk_path = current_path,
        features_df = data_storage$features
      )
      data_storage$current_chunk <- data_storage$current_chunk + 1
    }
  })
  
  observeEvent(list(input$btn_unknown, input$hotkey_3), {
    if (isTRUE(input$btn_unknown > 0) || isTRUE(input$hotkey_3 > 0)) {
      current_path <- data_storage$files_to_classify[data_storage$current_chunk]
      classify_and_move(
        classification = "Unknown",
        chunk_path = current_path,
        features_df = data_storage$features
      )
      data_storage$current_chunk <- data_storage$current_chunk + 1
    }
  })
  
  observeEvent(list(input$btn_noise, input$hotkey_4), {
    if (isTRUE(input$btn_noise > 0) || isTRUE(input$hotkey_4 > 0)) {
      current_path <- data_storage$files_to_classify[data_storage$current_chunk]
      classify_and_move(
        classification = "Noise",
        chunk_path = current_path,
        features_df = data_storage$features
      )
      data_storage$current_chunk <- data_storage$current_chunk + 1
    }
  })
  
  # Event handler for next/previous buttons
  observeEvent(input$btn_next, {
    data_storage$current_chunk <- data_storage$current_chunk + 1
  })
  
  observeEvent(input$btn_prev, {
    data_storage$current_chunk <- max(1, data_storage$current_chunk - 1)
  })
  
  # --- Output Rendering ---
  
  # Render the current audio file and waveform/spectrogram
  output$audio_player <- renderUI({
    req(data_storage$files_to_classify)
    
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    req(file.exists(here(chunk_path)))
    
    # Generate the audio tag
    audio_tag <- tags$audio(
      id = "audio_element",
      src = file.path("temp_audio", basename(chunk_path[[1]])),
      type = "audio/wav",
      autoplay = TRUE,
      controls = TRUE
    )
    
    # --- UPDATED JAVASCRIPT LOGIC ---
    # This new script creates and destroys the timer based on play/pause events.
    js_script <- tags$script(HTML("
    // Ensure we have a fresh audio element object
    var audio_player = document.getElementById('audio_element');

    // Make sure the timer variable is accessible on the window object
    // and clear any old timer from a previous chunk.
    if (window.updateTimer) {
      clearInterval(window.updateTimer);
    }

    if (audio_player) {
      // This function STARTS the timer that sends updates to Shiny
      const startTimer = function() {
        // Just in case, clear any existing timer before creating a new one
        if (window.updateTimer) { clearInterval(window.updateTimer); }
        
        window.updateTimer = setInterval(function() {
          Shiny.setInputValue('current_time', audio_player.currentTime, {priority: 'event'});
        }, 50); // Update every 50ms (a good balance for performance)
      };

      // This function STOPS the timer
      const stopTimer = function() {
        clearInterval(window.updateTimer);
      };

      // Add event listeners to the audio player
      audio_player.addEventListener('play', startTimer);    // When user clicks play
      audio_player.addEventListener('playing', startTimer); // When autoplay starts
      audio_player.addEventListener('pause', stopTimer);    // When user clicks pause
      audio_player.addEventListener('ended', stopTimer);    // When the clip finishes
    }
  "))
    # --- END OF JAVASCRIPT UPDATE ---
    
    return(tagList(audio_tag, js_script))
  })
  
  # Render the waveform plot
  output$waveform_plot <- renderPlotly({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    
    # Check if a file actually exists at the specified path
    req(file.exists(here(chunk_path)))
    
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    
    # Create the initial plot using oscillo and convert to plotly
    oscillo_plot <- seewave::oscillo(chunk_wave@left, f = chunk_wave@samp.rate, title = "Waveform", plot = FALSE)
    
    # Defensive check to ensure oscillo_plot is a valid matrix
    req(is.matrix(oscillo_plot))
    
    # Create a time vector to match the amplitude data
    time_vector <- seq(from = 0, to = seewave::duration(chunk_wave), length.out = nrow(oscillo_plot))
    
    # Create the base plot
    p <- plot_ly(
      x = time_vector,
      y = oscillo_plot[,1],
      type = 'scatter',
      mode = 'lines',
      name = "Waveform"
    ) %>%
      layout(
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Amplitude")
      )
    
    return(p)
  })
  
  # Render the spectrogram plot
  output$spectrogram_plot <- renderPlotly({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    
    # Check if a file actually exists at the specified path
    req(file.exists(here(chunk_path)))
    
    chunk_wave <- readWave(here(chunk_path))
    req(seewave::duration(chunk_wave) > 0.001)
    # Use the `tlim` argument to ensure the spectrogram covers the entire duration
    spectro_plot <- seewave::spectro(chunk_wave@left, f = chunk_wave@samp.rate, main = "Spectrogram", plot = FALSE, osc = FALSE, tlim = c(0,seewave::duration(chunk_wave)))
    #spectro_plot <- calculate_spectrogram_manual(chunk_wave, wl = 2048)

    
    # Defensive check to ensure spectro_plot is a valid list
    req(is.list(spectro_plot))
    time_vector <- spectro_plot$time
    freq_vector <- spectro_plot$freq
    amp_matrix <- spectro_plot$amp
    
    p <- plot_ly(
      z = amp_matrix,
      x = time_vector,
      y = freq_vector,
      type = 'heatmap',
      colors = 'viridis',
      hoverinfo = 'x+y+z',
      showscale = FALSE
    ) %>%
      layout(
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Frequency (kHz)")
      )
    
    return(p)
  })
  
  # Update the playhead_time reactive value with the JavaScript input
  observeEvent(input$current_time, {
    playhead_time(input$current_time)
  })
  
  # Use plotlyProxy to update the playhead in real-time
  observeEvent(playhead_time(), {
    req(data_storage$files_to_classify)
    
    # Get the current duration to properly scale the playhead
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    req(file.exists(here(chunk_path)))
    chunk_wave <- readWave(here(chunk_path))
    duration <- seewave::duration(chunk_wave)
    
    # Use plotlyProxy to update the waveform plot
    plotlyProxy("waveform_plot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = list(
          list(
            type = 'line',
            x0 = playhead_time(), x1 = playhead_time(),
            y0 = 0, y1 = 1, yref = 'paper',
            line = list(color = 'red', dash = 'dash')
          )
        )
      ))
    
    # Use plotlyProxy to update the spectrogram plot
    plotlyProxy("spectrogram_plot", session) %>%
      plotlyProxyInvoke("relayout", list(
        shapes = list(
          list(
            type = 'line',
            x0 = playhead_time(), x1 = playhead_time(),
            y0 = 0, y1 = 1, yref = 'paper',
            line = list(color = 'red', dash = 'dash')
          )
        )
      ))
  }, ignoreInit = TRUE)
  
  
  # Display the current file information and progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify)
    
    total_files <- length(data_storage$files_to_classify)
    current_file_name <- basename(here(data_storage$files_to_classify[data_storage$current_chunk]))
    
    paste0("File ", data_storage$current_chunk, " of ", total_files, ": ", current_file_name)
  })
  
  # Stop the app when the session ends
  session$onSessionEnded(function() {
    stopApp()
  })
}
