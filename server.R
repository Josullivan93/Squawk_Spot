if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(here, shiny, dplyr, data.table, seewave, tuneR)
source(here("helper.R"))

# --- Server Definition ---
server <- function(input, output, session) {
  
  # Reactive values to store app state
  app_state <- reactiveValues(
    files_to_process = character(0),
    current_index = 1,
    classification_data = NULL,
    resume_mode = FALSE,
    current_file = NULL
  )
  
  # Observer to handle "Process and Slice File" button click
  observeEvent(input$process_btn, {
    req(input$file_upload)
    
    # Reset app state
    app_state$files_to_process <- character(0)
    app_state$current_index <- 1
    app_state$classification_data <- NULL
    app_state$resume_mode <- FALSE
    app_state$current_file <- NULL
    
    showNotification("Processing and slicing file...", duration = NULL, type = "info", id = "processing_note")
    
    # Process the uploaded file and pass filtering options to the helper function
    withProgress(message = "Slicing audio...", value = 0.5, {
      temp_path <- input$file_upload$datapath
      app_state$classification_data <- process_wav(
        wav_path = temp_path,
        stationary_filter = input$stationary_filter,
        nonstationary_filter = input$nonstationary_filter,
        low_pass_hz = input$low_pass_hz,
        high_pass_hz = input$high_pass_hz,
        window_size = input$window_size,
        window_overlap = input$window_overlap
      )
    })
    
    removeNotification(id = "processing_note")
    
    # Get the list of new files to process from the temp directory
    app_state$files_to_process <- list.files("tmp", pattern = ".wav", full.names = TRUE)
    app_state$current_file <- app_state$files_to_process[app_state$current_index]
    
    # Enable buttons
    shinyjs::enable("squawk_btn")
    shinyjs::enable("other_btn")
    shinyjs::enable("unknown_btn")
    shinyjs::enable("noise_btn")
    
    showNotification("Processing complete. Ready to classify.", type = "success")
  })
  
  # Observer to handle "Resume" button click
  observeEvent(input$resume_btn, {
    temp_dir <- "tmp"
    if (!dir.exists(temp_dir)) {
      showNotification("No 'tmp' folder found to resume from.", type = "error")
      return()
    }
    
    # Reset state
    app_state$files_to_process <- character(0)
    app_state$current_index <- 1
    app_state$classification_data <- NULL
    app_state$resume_mode <- TRUE
    app_state$current_file <- NULL
    
    # Load the features CSV
    features_path <- file.path(temp_dir, "features.csv")
    if (!file.exists(features_path)) {
      showNotification("No 'features.csv' found in the 'tmp' folder.", type = "error")
      return()
    }
    app_state$classification_data <- fread(features_path)
    
    # Find the files to process (unclassified ones)
    all_temp_files <- list.files(temp_dir, pattern = ".wav", full.names = TRUE)
    classified_file_names <- app_state$classification_data$classification != "Unclassified"
    
    if (all(classified_file_names)) {
      showNotification("All files in 'tmp' folder have already been classified.", type = "info")
      return()
    }
    
    # Identify which files correspond to unclassified rows
    unclassified_indices <- which(app_state$classification_data$classification == "Unclassified")
    
    app_state$files_to_process <- all_temp_files[unclassified_indices]
    app_state$current_index <- 1
    app_state$current_file <- app_state$files_to_process[app_state$current_index]
    
    # Enable buttons
    shinyjs::enable("squawk_btn")
    shinyjs::enable("other_btn")
    shinyjs::enable("unknown_btn")
    shinyjs::enable("noise_btn")
    
    showNotification("Resumed from temporary files.", type = "info")
  })
  
  # A helper function to advance to the next audio snippet
  advance_to_next <- function() {
    app_state$current_index <- app_state$current_index + 1
    if (app_state$current_index <= length(app_state$files_to_process)) {
      app_state$current_file <- app_state$files_to_process[app_state$current_index]
    } else {
      showNotification("All files have been classified!", type = "success", duration = NULL)
      app_state$current_file <- NULL
      shinyjs::disable("squawk_btn")
      shinyjs::disable("other_btn")
      shinyjs::disable("unknown_btn")
      shinyjs::disable("noise_btn")
    }
  }
  
  # Observers for classification buttons
  observeEvent(input$squawk_btn, {
    req(app_state$current_file)
    original_index <- which(list.files("tmp", pattern=".wav", full.names=TRUE) == app_state$current_file)
    classify_and_move("Squawk", app_state$files_to_process, original_index, app_state$classification_data, file.path("tmp", "features.csv"))
    advance_to_next()
  })
  
  observeEvent(input$other_btn, {
    req(app_state$current_file)
    original_index <- which(list.files("tmp", pattern=".wav", full.names=TRUE) == app_state$current_file)
    classify_and_move("Other Vocalisation", app_state$files_to_process, original_index, app_state$classification_data, file.path("tmp", "features.csv"))
    advance_to_next()
  })
  
  observeEvent(input$unknown_btn, {
    req(app_state$current_file)
    original_index <- which(list.files("tmp", pattern=".wav", full.names=TRUE) == app_state$current_file)
    classify_and_move("Unknown", app_state$files_to_process, original_index, app_state$classification_data, file.path("tmp", "features.csv"))
    advance_to_next()
  })
  
  observeEvent(input$noise_btn, {
    req(app_state$current_file)
    original_index <- which(list.files("tmp", pattern=".wav", full.names=TRUE) == app_state$current_file)
    classify_and_move("Noise", app_state$files_to_process, original_index, app_state$classification_data, file.path("tmp", "features.csv"))
    advance_to_next()
  })
  
  # --- UI Rendering and Plotting ---
  
  # Render the current file name display
  output$current_file_display <- renderText({
    if (!is.null(app_state$current_file)) {
      paste("Classifying: ", basename(app_state$current_file))
    } else {
      "Waiting for file..."
    }
  })
  
  # Render the audio player
  output$audio_player <- renderUI({
    req(app_state$current_file)
    tags$audio(src = sub("www/", "", app_state$current_file), type = "audio/wav", controls = NA, autoplay = "autoplay", style = "display: block; width: 100%;")
  })
  
  # Render the status text
  output$status_text <- renderText({
    if (is.null(app_state$classification_data)) {
      "Ready to start."
    } else {
      paste0("Processing file ", app_state$current_index, " of ", length(app_state$files_to_process))
    }
  })
  
  # Reactive expression to get the current audio snippet
  current_audio <- reactive({
    req(app_state$current_file)
    readWave(app_state$current_file)
  })
  
  # Render the waveform plot
  output$waveform_plot <- renderPlot({
    req(current_audio())
    audio_data <- current_audio()
    plot(audio_data, type = "l", main = "Waveform", xlab = "Time (s)", ylab = "Amplitude")
  })
  
  # Render the spectrogram plot
  output$spectrogram_plot <- renderPlot({
    req(current_audio())
    audio_data <- current_audio()
    seewave::spectro(audio_data, osc = TRUE, main = "Spectrogram")
  })
}
