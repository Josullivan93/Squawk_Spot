if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(here, shiny, dplyr, data.table, seewave, tuneR)
source(here("helper.R"))

# --- Server Definition ---
server <- function(input, output, session) {
  
  # reactiveValues object for storing data
  data_storage <- reactiveValues(
    features = NULL,
    current_chunk = 1,
    files_to_classify = NULL,
    full_wave_object = NULL
  )
  
  # Create a temporary directory for storing sliced files and data
  temp_dir <- file.path("tmp")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }
  
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
    if (file.exists(file.path(temp_dir, "features.csv"))) {
      # Load saved features data
      data_storage$features <- fread(file.path(temp_dir, "features.csv"))
      
      # Load the full wave object
      load(file.path(temp_dir, "full_wave.RData"))
      data_storage$full_wave_object <- full_wave_obj
      
      # Get the list of remaining chunks to classify
      files <- list.files(temp_dir, pattern = "\\.wav$", full.names = TRUE)
      data_storage$files_to_classify <- files
      data_storage$current_chunk <- 1
    }
  })
  
  # --- Navigation and Classification ---
  
  # Event handler for next/previous and classification buttons
  # The input from hotkeys (e.g., input$hotkey_1) is also observed here
  observeEvent(list(input$btn_squawk, input$btn_other, input$btn_unknown, input$btn_noise, input$hotkey_1, input$hotkey_2, input$hotkey_3, input$hotkey_4), {
    
    # Classification logic
    classification <- ""
    if (isTRUE(input$btn_squawk > 0) || isTRUE(input$hotkey_1 > 0)) {
      classification <- "Squawk"
    } else if (isTRUE(input$btn_other > 0) || isTRUE(input$hotkey_2 > 0)) {
      classification <- "Other Vocalisation"
    } else if (isTRUE(input$btn_unknown > 0) || isTRUE(input$hotkey_3 > 0)) {
      classification <- "Unknown"
    } else if (isTRUE(input$btn_noise > 0) || isTRUE(input$hotkey_4 > 0)) {
      classification <- "Noise"
    }
    
    # Check if a classification button was actually clicked
    if (classification != "") {
      # Get the path of the current file
      current_path <- data_storage$files_to_classify[data_storage$current_chunk]
      
      # Classify and move the file, and update the master CSV
      classify_and_move(
        classification = classification,
        chunk_path = current_path,
        features_df = data_storage$features
      )
      
      # Advance to the next chunk
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
    req(file.exists(chunk_path))
    
    tags$audio(src = chunk_path, type = "audio/wav", autoplay = TRUE, controls = TRUE)
  })
  
  # Render the waveform plot
  output$waveform_plot <- renderPlot({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    
    # Check if a file actually exists at the specified path
    req(file.exists(chunk_path))
    
    chunk_wave <- readWave(chunk_path)
    # The `seewave::...` is not required here since it's already a package dependency
    wave(chunk_wave, title = "Waveform")
  })
  
  # Render the spectrogram plot
  output$spectrogram_plot <- renderPlot({
    req(data_storage$files_to_classify)
    chunk_path <- data_storage$files_to_classify[data_storage$current_chunk]
    
    # Check if a file actually exists at the specified path
    req(file.exists(chunk_path))
    
    chunk_wave <- readWave(chunk_path)
    spectro(chunk_wave, osc = FALSE, grid = FALSE, title = "Spectrogram")
  })
  
  # Display the current file information and progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify)
    
    total_files <- length(data_storage$files_to_classify)
    current_file_name <- basename(data_storage$files_to_classify[data_storage$current_chunk])
    
    paste0("File ", data_storage$current_chunk, " of ", total_files, ": ", current_file_name)
  })
  
  # Stop the app when the session ends
  session$onSessionEnded(function() {
    stopApp()
  })
}
