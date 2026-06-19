# server.R: Contains the core logic of the Shiny application.
# Define the WebR fetch function at the top of the script
submit_score_to_google <- function(score, total) {
  # Validate inputs
  if (is.null(score) || is.null(total) || is.na(score) || is.na(total)) {
    warning("submit_score_to_google called with invalid inputs: score=", score, ", total=", total)
    return(invisible(NULL))
  }
  
  cat("Submitting to Google Sheets: score=", score, ", total=", total, "\n")
  
  google_url <- "https://script.google.com/macros/s/AKfycbwolI-f7jUWTbhO0CKdJ2c98CkvQbhf07SYQKQ68aDcpHXa_JKZjfPp1BKaWtFVqFm4/exec" 
  
  payload <- jsonlite::toJSON(list(score = score, total = total), auto_unbox = TRUE)
  
  cat("Payload:", payload, "\n")
  
  js_code <- sprintf("
    fetch('%s', {
      method: 'POST',
      mode: 'cors',
      headers: { 'Content-Type': 'text/plain' },
      body: '%s'
    })
    .then(response => response.json())
    .then(data => {
      Shiny.setInputValue('google_response', data, {priority: 'event'});
    });
  ", google_url, payload)
  
  shinyjs::runjs(js_code)
}

# Server Definition
server <- function(input, output, session) {
  
  startup_done <- FALSE
  submission_done <- FALSE
  
  observeEvent(TRUE, {
    # This runs ONCE when the server initializes
    if (!startup_done) {
      withProgress(message = 'System Startup', value = 0, {
        incProgress(0.4, detail = "Loading Bioacoustic Toolkits...")
        incProgress(0.2, detail = "Opening Workspace...")
        Sys.sleep(0.5)
        
        # Ensure clean state: only pre_process_sidebar is visible
        shinyjs::hide("loading_overlay")
        shinyjs::hide("main_ui")
        shinyjs::hide("completion_ui")
        shinyjs::show("pre_process_sidebar")
        
        showNotification("Engine Ready", type = "message", duration = 3)
      })
      startup_done <<- TRUE
    }
  }, once = TRUE)
  
  observe({
    # Disable 'Previous' if we are at the very first run
    if (is.null(data_storage$current_run) || data_storage$current_run <= 1) {
      shinyjs::disable("btn_prev")
    } else {
      shinyjs::enable("btn_prev")
    }
  })
  
  # Handle restricting button press until audio plays
  classification_btns <- c("btn_squawk", "btn_alarm", "btn_other", "btn_noise", "btn_unknown", "btn_next")
  #restrict
  observeEvent(data_storage$current_run, {
    for (btn in classification_btns) {
      shinyjs::disable(btn)
    }
  })
  #enable
  observeEvent(input$audio_finished, {
    for (btn in classification_btns) {
      shinyjs::enable(btn)
    }
    showNotification("Audio reviewed. Buttons enabled.", type = "message", duration = 2)
  })
  
  # REACTIVE DATA STORAGE
  data_storage <- reactiveValues(
    features = NULL,
    current_run = 0, # SET TO 0 initially so reactives don't fire yet
    files_to_classify = NULL,
    ground_truth = NULL,
    user_answers = character(),
    final_score = 0
  )

  # Reactive value for the playhead time
  playhead_time <- reactiveVal(0)

  # Combined Reactive: chunk info + audio data
  current_chunk_full <- reactive({
    req(data_storage$current_run > 0)
    req(data_storage$files_to_classify)
    run_idx <- data_storage$current_run

    # Ensure index is valid before proceeding
    req(run_idx > 0, run_idx <= length(data_storage$files_to_classify))

    chunk_path <- data_storage$files_to_classify[run_idx]
    actual_path <- if (file.exists(chunk_path)) chunk_path else here(chunk_path)
    if (!file.exists(actual_path)) {
      return(NULL)
    }

    # Read WAV
    chunk_wave <- preprocess_wav(actual_path, normalise = TRUE)
    duration <- seewave::duration(chunk_wave)

    # Get sample rate
    sample_rate <- chunk_wave@samp.rate

    # Waveform (oscillo)
    n_samples <- length(chunk_wave@left)
    target_points <- 5000
    step_size <-  max(1, floor(n_samples / target_points))
    downsampled_vector <- chunk_wave@left[seq(1, n_samples, by = step_size)]
    f_new = sample_rate/step_size
    osc_data <- seewave::oscillo(downsampled_vector, f = f_new, plot = FALSE)
    #osc_data <- seewave::oscillo(chunk_wave@left, f = sample_rate, plot = FALSE)
    time_vector <- seq(0, duration, length.out = nrow(osc_data))

    # Spectrogram
    spec_data <- seewave::spectro(chunk_wave@left,
      f = sample_rate,
      plot = FALSE, osc = FALSE
    )
    
    max_khz_to_keep <- 15
    freq_idx <- which(spec_data$freq <= max_khz_to_keep)
    spec_data$freq <- spec_data$freq[freq_idx]
    spec_data$amp <- spec_data$amp[freq_idx, ]
    
    target_time_bins <- 1000
    step_t <- max(1, floor(length(spec_data$time) / target_time_bins))
    time_idx <- seq(1, length(spec_data$time), by = step_t)
    spec_data$time <- spec_data$time[time_idx]
    spec_data$amp <- spec_data$amp[,time_idx]

    rm(chunk_wave)
    
    # Extract the highlight times for this specific file
    current_file <- basename(chunk_path)
    file_truth <- data_storage$ground_truth[data_storage$ground_truth$filename == current_file, ]
    
    chunk_highlight <- list(
      start = file_truth$highlight_start,
      end = file_truth$highlight_end
    )
    
    list(
      chunk_path  = chunk_path,
      chunk_start = 0,
      highlight   = chunk_highlight,
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

    y_range_wave <- range(chunk$osc_data[, 1], na.rm = TRUE)
    if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)

    plot_ly(x = chunk$time_vector, y = chunk$osc_data, type = "scatter", mode = "lines", hoverinfo = "x+y", name = "Waveform") |>
      layout(
        xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
        yaxis = list(range = y_range_wave, fixedrange = TRUE, title = "Amplitude")
      )
  })

  output$spectrogram_plot <- renderPlotly({
    req(current_chunk_full())
    chunk <- current_chunk_full()

    amp_db <- chunk$spec_data$amp
    finite_vals <- amp_db[is.finite(amp_db)]
    min_db <- if (length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100 # Safe default
    amp_db[!is.finite(amp_db)] <- min_db

    plot_ly(z = amp_db, x = chunk$spec_data$time, y = chunk$spec_data$freq, type = "heatmap", colors = "inferno", hoverinfo = "x+y+z", showscale = FALSE) |>
      layout(
        xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
        yaxis = list(range = c(0, 13), title = "Frequency (kHz)")
      )
  })
  
  # Demo plots
  output$demo_squawk_spec <- renderPlotly({
    # Load the demo squawk file
    demo_wave <- readWave(here("www/demo/squawk_demo.wav"))
    
    # Generate spectrogram data
    spec_data <- seewave::spectro(demo_wave@left,
                                  f = demo_wave@samp.rate,
                                  plot = FALSE, osc = FALSE
    )
    
    # Trim frequency to 15 kHz (matching your main app)
    max_khz_to_keep <- 15
    freq_idx <- which(spec_data$freq <= max_khz_to_keep)
    spec_data$freq <- spec_data$freq[freq_idx]
    spec_data$amp <- spec_data$amp[freq_idx, ]
    
    # Downsample time bins for performance
    target_time_bins <- 1000
    step_t <- max(1, floor(length(spec_data$time) / target_time_bins))
    time_idx <- seq(1, length(spec_data$time), by = step_t)
    spec_data$time <- spec_data$time[time_idx]
    spec_data$amp <- spec_data$amp[, time_idx]
    
    # Handle non-finite values
    amp_db <- spec_data$amp
    finite_vals <- amp_db[is.finite(amp_db)]
    min_db <- if (length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100
    amp_db[!is.finite(amp_db)] <- min_db
    
    # Create plotly heatmap (identical to main app)
    plot_ly(z = amp_db, x = spec_data$time, y = spec_data$freq, 
            type = "heatmap", colors = "inferno", 
            showscale = FALSE, hoverinfo = "x+y+z", height = 300) |>
      layout(
        title = "Squawk Example Spectrogram",
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Frequency (kHz)", range = c(0, 13)),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  output$demo_alarm_spec <- renderPlotly({
    # Load the demo alarm file
    demo_wave <- readWave(here("www/demo/alarm_demo.wav"))
    
    # Generate spectrogram data
    spec_data <- seewave::spectro(demo_wave@left,
                                  f = demo_wave@samp.rate,
                                  plot = FALSE, osc = FALSE
    )
    
    # Trim frequency to 15 kHz
    max_khz_to_keep <- 15
    freq_idx <- which(spec_data$freq <= max_khz_to_keep)
    spec_data$freq <- spec_data$freq[freq_idx]
    spec_data$amp <- spec_data$amp[freq_idx, ]
    
    # Downsample time bins
    target_time_bins <- 1000
    step_t <- max(1, floor(length(spec_data$time) / target_time_bins))
    time_idx <- seq(1, length(spec_data$time), by = step_t)
    spec_data$time <- spec_data$time[time_idx]
    spec_data$amp <- spec_data$amp[, time_idx]
    
    # Handle non-finite values
    amp_db <- spec_data$amp
    finite_vals <- amp_db[is.finite(amp_db)]
    min_db <- if (length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100
    amp_db[!is.finite(amp_db)] <- min_db
    
    # Create plotly heatmap
    plot_ly(z = amp_db, x = spec_data$time, y = spec_data$freq, 
            type = "heatmap", colors = "inferno", 
            showscale = FALSE, hoverinfo = "x+y+z", height = 300) |>
      layout(
        title = "Alarm Example Spectrogram",
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Frequency (kHz)", range = c(0, 13)),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })

  # Update only playhead (fast)
  observeEvent(playhead_time(), {
    req(current_chunk_full()) 
    chunk <- current_chunk_full()
    
    shapes <- list()
    
    # 1. Conditionally add the Highlight Box
    if (input$show_highlight && !is.null(chunk$highlight$start) && !is.na(chunk$highlight$start)) {
      shapes[[1]] <- list(
        type = "rect",
        fillcolor = "rgba(0, 123, 255, 0.2)", # Semi-transparent blue
        line = list(color = "rgba(0, 123, 255, 0.6)", width = 1),
        x0 = chunk$highlight$start,
        x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper"
      )
    }
    
    # 2. Always add the Playhead Line
    ph <- max(0, min(chunk$duration, as.numeric(playhead_time())))
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = ph, x1 = ph, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "red", width = 2)
    )
    
    # 3. Push both to the UI
    plotlyProxy("waveform_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
    plotlyProxy("spectrogram_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
  })

  # Update progress bar
  observeEvent(data_storage$current_run, {
    if (data_storage$current_run <= 1) {
      shinyjs::disable("btn_prev")
    } else {
      shinyjs::enable("btn_prev")
    }
    
    # Your existing progress bar logic can safely live inside this same block
    total <- length(data_storage$files_to_classify)
    if (total > 0) {
      progress <- ((data_storage$current_run - 1) / total) * 100
      shinyjs::runjs(paste0("document.getElementById('progress_bar').style.width = '", progress, "%';"))
    }
  })

  # File Processing and Chunking
  observeEvent(input$start_btn, {
    shinyjs::show("loading_overlay")
    shinyjs::hide("pre_process_sidebar")
    
    # 1. Load ground truth and all audio files
    master_truth <- read.csv("www/ground_truth.csv")
    all_files <- list.files("www/audio", pattern = "\\.wav$", full.names = TRUE)
    
    # 2. Randomly sample clips
    quiz_files <- sample(all_files) 
    quiz_filenames <- basename(quiz_files)
    
    # 3. Match the ground truth to the selected files
    quiz_truth <- master_truth[match(quiz_filenames, master_truth$filename), ]
    
    # 4. Initialize session state
    data_storage$files_to_classify <- quiz_files
    data_storage$ground_truth <- quiz_truth
    data_storage$user_answers <- character()
    data_storage$current_run <- 1
    
    # 5. UI Transitions
    shinyjs::show("main_ui")
    shinyjs::delay(500, shinyjs::hide("loading_overlay"))
  })
  
  submission_done <- FALSE
  
  advance_ui <- function() {
    # Guard: ensure we have files to classify
    if (is.null(data_storage$files_to_classify) || length(data_storage$files_to_classify) == 0) {
      return(invisible(NULL))
    }
    
    total_files <- length(data_storage$files_to_classify)
    current_run <- data_storage$current_run
    
    # Guard: ensure current_run is a valid number
    if (is.null(current_run) || !is.numeric(current_run)) {
      return(invisible(NULL))
    }
    
    # Move to next file
    data_storage$current_run <- current_run + 1
    playhead_time(0)
    
    # Check if we've gone past the last file
    if (data_storage$current_run > total_files) {
      # Quiz is complete!
      shinyjs::hide("main_ui")
      shinyjs::show("completion_ui")
      
      # Calculate score (count matches between user answers and ground truth)
      score <- sum(data_storage$user_answers == data_storage$ground_truth$true_class, na.rm = TRUE)
      data_storage$final_score <- score
      
      # Debug: print to console to verify score calculation
      cat("Quiz Complete!\n")
      cat("Total files:", total_files, "\n")
      cat("User answers:", paste(data_storage$user_answers, collapse = ", "), "\n")
      cat("True class:", paste(data_storage$ground_truth$true_class, collapse = ", "), "\n")
      cat("Score:", score, "\n")
      
      # Submit to Google Sheets
      submit_score_to_google(score = score, total = total_files)
    }
  }
  
  observeEvent(list(input$btn_squawk, input$hotkey_1), { 
    req(data_storage$current_run > 0)  # Don't fire until quiz has started
    data_storage$user_answers[data_storage$current_run] <- "Squawk"
    advance_ui()
  })
  
  observeEvent(list(input$btn_alarm, input$hotkey_2), { 
    req(data_storage$current_run > 0)
    data_storage$user_answers[data_storage$current_run] <- "Alarm"
    advance_ui()
  })
  
  observeEvent(list(input$btn_other, input$hotkey_3), { 
    req(data_storage$current_run > 0)
    data_storage$user_answers[data_storage$current_run] <- "Other Vocalisation"
    advance_ui()
  })
  
  observeEvent(list(input$btn_noise, input$hotkey_4), { 
    req(data_storage$current_run > 0)
    data_storage$user_answers[data_storage$current_run] <- "Noise"
    advance_ui()
  })
  
  observeEvent(list(input$btn_unknown, input$hotkey_5), { 
    req(data_storage$current_run > 0)
    data_storage$user_answers[data_storage$current_run] <- "Unknown"
    advance_ui()
  })
  
  observeEvent(list(input$btn_skip), { 
    req(data_storage$current_run > 0)
    data_storage$user_answers[data_storage$current_run] <- "Skipped"
    advance_ui()
  })
  observeEvent(input$btn_prev, {
    req(data_storage$current_run > 1) # Prevent going back before file 1
    
    # Remove the last recorded answer
    if (length(data_storage$user_answers) >= data_storage$current_run - 1) {
      data_storage$user_answers <- data_storage$user_answers[-(data_storage$current_run - 1)]
    }
    
    # Decrement the UI
    data_storage$current_run <- data_storage$current_run - 1
  })
  
  observeEvent(input$google_response, {
    res <- input$google_response
    
    # Only proceed if we actually got a response (not NULL on startup)
    if (is.null(res)) return()
    
    if (!is.null(res$success) && res$success) {
      output$final_score_display <- renderUI({
        tagList(
          h2("Quiz Complete!"),
          h3(paste("Your Score:", data_storage$final_score, "/ 10")),
          hr(),
          h4("Conference Statistics:"),
          p(paste("Average Score:", res$average, "/ 10")),
          p(paste("Total participants:", res$total_users))
        )
      })
    } else {
      output$final_score_display <- renderUI({
        tagList(
          h2("Quiz Complete!"),
          h3(paste("Your Score:", data_storage$final_score, "/ 5")),
          p("Offline mode: Could not retrieve crowd statistics.")
        )
      })
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Handle the Try Again button
  observeEvent(input$restart_app, {
    shinyjs::hide("completion_ui")
    shinyjs::show("pre_process_sidebar")
  })
  
  # Observe the highlight checkbox separately
  observeEvent(input$show_highlight, {
    req(current_chunk_full()) 
    chunk <- current_chunk_full()
    
    shapes <- list()
    
    # Add highlight box if enabled
    if (input$show_highlight && !is.null(chunk$highlight$start) && !is.na(chunk$highlight$start)) {
      shapes[[1]] <- list(
        type = "rect",
        fillcolor = "rgba(0, 123, 255, 0.2)",
        line = list(color = "rgba(0, 123, 255, 0.6)", width = 1),
        x0 = chunk$highlight$start,
        x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper"
      )
    }
    
    # Add playhead line
    ph <- max(0, min(chunk$duration, as.numeric(playhead_time())))
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = ph, x1 = ph, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "red", width = 2)
    )
    
    # Update both plots
    plotlyProxy("waveform_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
    plotlyProxy("spectrogram_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
  })
  
  # Audio Player UI
  output$audio_player <- renderUI({
    req(current_chunk_full())
    chunk <- current_chunk_full()

    audio_tag <- tags$audio(
      id = "audio_element",
      src = paste0("audio/", basename(chunk$chunk_path)),
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
        audio_player.addEventListener('ended', () => {
        stopTimer();
        Shiny.setInputValue('audio_finished', Math.random(), {priority: 'event'});
        });
      }
    "))
    tagList(audio_tag, js_script)
  })

  observeEvent(input$current_time, {
    playhead_time(input$current_time)
  })

  # File info + progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify, data_storage$current_run)
    total_runs <- length(data_storage$files_to_classify)
    current_run_idx <- data_storage$current_run
    req(current_run_idx <= total_runs) # Ensure run index is valid
    current_file_name <- basename(here(data_storage$files_to_classify[current_run_idx]))
    paste0("Run ", current_run_idx, " of ", total_runs)
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}