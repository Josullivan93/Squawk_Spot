# server.R: Contains the core logic of the Shiny application.
options(shiny.maxRequestSize = 30 * 1024^2)

# Path to your final trained models
# path_vocal_model <- here("models", "final_vocal_model.rds")
squawk_model <- readRDS(here("models", "final_squawk_model.rds"))
# Extract feature names
features_used <- squawk_model$forest$independent.variable.names
# message(features_used)
# Server Definition
server <- function(input, output, session) {
  
  observe({
    withProgress(message = 'System Startup', value = 0, {
      
      # Step 1: Load R Libraries
      incProgress(0.4, detail = "Loading Bioacoustic Toolkits...")
      pacman::p_load(here, plotly, dplyr, tidyverse, tuneR, seewave, reticulate, data.table, ranger,
                     future.apply, matrixStats, progress, signal)
      
      # Step 2: Initialize Python
      incProgress(0.4, detail = "Starting Python Noise Reduction Engine...")
      # 1. Python Environment Setup ----
      # This runs once when source(helper.R) is called by server.R
      tryCatch(
        {
          if (!reticulate::virtualenv_exists("r-reticulate")) {
            reticulate::virtualenv_create("r-reticulate", packages = c("numpy", "scipy", "noisereduce"))
          }
          reticulate::use_virtualenv("r-reticulate", required = TRUE)
          nr <<- import("noisereduce", convert = FALSE)
          cat("Python (noisereduce) loaded successfully.\n")
        },
        error = function(e) {
          warning("Python setup failed. Noise reduction will be unavailable. Error: ", e$message)
        }
      )
      
      # Step 3: Finalize
      incProgress(0.2, detail = "Opening Workspace...")
      Sys.sleep(0.5) # Brief pause so the user can see it's finished
      
      # HIDE loading screen, SHOW app
      shinyjs::hide("loading_page")
      shinyjs::show("app_workspace")
      
      showNotification("Engine Ready", type = "message", duration = 3)
    })
  },  priority = 10)
  
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
  
  # reactiveValues object for storing data
  data_storage <- reactiveValues(
    features = NULL,
    current_run = 1,
    files_to_classify = NULL,
    full_wave_object = NULL,
    runs_table = NULL,
    history = list()
  )

  # Reactive value for the playhead time
  playhead_time <- reactiveVal(0)

  # Reactive to store current start/end time of run
  current_run_times <- reactive({
    req(data_storage$runs_table, data_storage$current_run)

    run_idx <- data_storage$current_run

    if (run_idx > nrow(data_storage$runs_table)) {
      return(NULL)
    }

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
    actual_path <- if (file.exists(chunk_path)) chunk_path else here(chunk_path)
    if (!file.exists(actual_path)) {
      return(NULL)
    }

    # Read WAV
    chunk_wave <- readWave(actual_path)
    req(seewave::duration(chunk_wave) > 0.001)
    duration <- seewave::duration(chunk_wave)

    # Get sample rate
    sample_rate <- chunk_wave@samp.rate

    # Check stereo and convert to mono if so
    is_stereo <- chunk_wave@stereo
    if (is_stereo) chunk_wave <- mono(chunk_wave, which = "both")

    # Type conversion: Map integers to decimals
    chunk_wave@left <- as.numeric(chunk_wave@left) / 32768

    # Waveform (oscillo)
    osc_data <- seewave::oscillo(chunk_wave@left, f = sample_rate, plot = FALSE)
    time_vector <- seq(0, duration, length.out = nrow(osc_data))

    # Spectrogram
    spec_data <- seewave::spectro(chunk_wave@left,
      f = sample_rate,
      plot = FALSE, osc = FALSE
    )

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
      end_rel <- max(0, min(duration, times$end - chunk_start))
      highlight <- if (end_rel > start_rel){ 
        list(start = start_rel, end = end_rel) 
      }else {list(start = start_rel, end = start_rel)
          }
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
    req(data_storage$current_run <= length(data_storage$files_to_classify))
    req(current_chunk_full())
    chunk <- current_chunk_full()

    y_range_wave <- range(chunk$osc_data[, 1], na.rm = TRUE)
    if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)

    shapes_base <- list()
    if (input$show_highlight && !is.null(chunk$highlight)) {
      shapes_base[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(0, 255, 255, 0.3)", line = list(width = 0)
      )
    }

    plot_ly(x = chunk$time_vector, y = chunk$osc_data[, 1], type = "scatter", mode = "lines", hoverinfo = "x+y", name = "Waveform") |>
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
    min_db <- if (length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100 # Safe default
    amp_db[!is.finite(amp_db)] <- min_db

    shapes_base <- list()
    if (input$show_highlight && !is.null(chunk$highlight)) {
      shapes_base[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(0, 255, 255, 0.3)", line = list(width = 0.5, color = "rgba(0, 255, 255, 1)")
      )
    }

    plot_ly(z = amp_db, x = chunk$spec_data$time, y = chunk$spec_data$freq, type = "heatmap", colors = "inferno", hoverinfo = "x+y+z", showscale = FALSE) |>
      layout(
        xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
        yaxis = list(range = c(0, 13), title = "Frequency (kHz)"),
        shapes = shapes_base
      )
  })

  # Update only playhead (fast)
  observeEvent(playhead_time(), {
    req(current_chunk_full()) # Ensure chunk data exists before updating
    chunk <- current_chunk_full()

    shapes <- list()
    if (input$show_highlight && !is.null(chunk$highlight)) {
      shapes[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(0, 255, 255, 0.3)", line = list(width = 0.5, color = "rgba(0, 255, 255, 1)")
      )
    }

    ph <- max(0, min(chunk$duration, as.numeric(playhead_time())))
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = ph, x1 = ph, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "red", width = 2)
    )

    plotlyProxy("waveform_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
    plotlyProxy("spectrogram_plot", session) |> plotlyProxyInvoke("relayout", list(shapes = shapes))
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

    shinyjs::show("loading_overlay")
    on.exit(shinyjs::hide("loading_overlay"))

    all_feats <- list()
    all_chunks <- list()
    all_files <- list()
    global_run_offset <- 0
    done_file_ids <- c()
    
    # 2. If 'Append' is checked, load the existing master file first
    master_path <- here("Output", "master_features.csv")
    if (file.exists(master_path)) {
      prior_anns <- fread(master_path)
      message("Pre-loading existing master data for merging...")
      if (!is.null(prior_anns)) {
        # Get unique filenames already in the master
        done_file_ids <- unique(prior_anns$file_id)
      }
    }
    
    files <- input$upload_file

    withProgress(message = "Processing...", value = 0, {
      for (i in seq_len(nrow(files))) {
        incProgress(1 / nrow(files), detail = paste("Processing:", files$name[i]))
        
        original_path <- files$datapath[i]
        this_file_id <- tools::file_path_sans_ext(basename(files$name[i]))
        
        # --- THE MISSING CHECK ---
        if (this_file_id %in% done_file_ids) {
          message(paste("Skipping:", this_file_id, "- already in Master Features."))
          next # Jump to the next file in the loop
        }

        # 1. Load and calc features

        features_df <- extract_features(files$name[i], files$datapath[i], p_cfg = list(
          label           = "NonStat_PreEmph",
          noise_reduction = "ns",
          win_len         = 100,
          overlap         = 0.75,
          pre_emph_coeff  = 0.97,
          filter_type     = "None",
          cutoff_highpass = NULL,
          cutoff_lowpass  = NULL,
          filter_order    = 4,
          normalise       = TRUE
        ))
        
        prob_squawk <- predict(squawk_model, features_df)$predictions[, "1"]
        features_df$prob_squawk <- prob_squawk
        features_df$auto_class <- ifelse(features_df$prob_squawk > 0.25, "Squawk", "Background")
        features_df$source_file <- files$name[i]

        data_storage$full_wave_object <- readWave(files$datapath[i])

        chunking_results <- group_and_slice_chunks(
          features_df = features_df,
          full_wave = data_storage$full_wave_object,
          positive_class = "Squawk", # Tell the function to look at our candidate logic
          buffer_time = 1.0, # 1 second before/after for context
          temp_dir = temp_dir,
          target_length = 3.0,
          original_path = files$name[i]
        )

        if (nrow(chunking_results$runs_table) > 0) {
          chunking_results$runs_table[, run_id := run_id + global_run_offset]
          chunking_results$updated_features_df[, run_id := run_id + global_run_offset]

          # Update the global offset for the NEXT file in the loop
          global_run_offset <- max(chunking_results$runs_table$run_id)

          all_chunks[[i]] <- chunking_results$runs_table
          all_feats[[i]] <- chunking_results$updated_features_df
          all_files[[i]] <- chunking_results$runs_table$filepath
        }
      }

      combined_runs <- rbindlist(all_chunks, fill = TRUE)

      # 7.1 Check if anything was actually found
      if (is.null(combined_runs) || nrow(combined_runs) == 0) {
        removeModal()
        showModal(modalDialog(
          title = "No Candidates Found",
          "The model did not detect any squawks in these files based on the current threshold (0.25).",
          footer = modalButton("Try again"),
          easyClose = TRUE
        ))
        return() # Stop the rest of the function from running
      }

      # 7.2 Update reactive storage
      data_storage$features <- rbindlist(all_feats, fill = TRUE)
      # Combine everything into one master classification list
      data_storage$runs_table <- combined_runs
      data_storage$files_to_classify <- unlist(all_files)
      data_storage$current_run <- 1

      # CRITICAL: Write the master files ONCE after the loop
      fwrite(data_storage$runs_table, file.path(temp_dir, "runs.csv"))
      fwrite(data_storage$features, file.path(temp_dir, "features.csv"))

      # Save state for resume
      saveRDS(
        list(runs = data_storage$runs_table, feats = data_storage$features),
        file.path(temp_dir, "app_state.rds")
      )

      removeModal()
      showNotification(paste0("Processing complete! Found ", nrow(combined_runs), " candidates."),
        type = "message"
      )

      shinyjs::hide("loading_overlay")
      shinyjs::hide(id = "pre_process_sidebar")
      shinyjs::show(id = "post_process_sidebar")
      shinyjs::hide(id = "main_placeholder")
      shinyjs::show(id = "main_ui")
    })
  })
  
  # Resume previous session
  observeEvent(input$resume_btn, {
    req(temp_dir)

    addResourcePath("temp_audio", temp_dir)

    feat_path <- file.path(temp_dir, "features.csv")
    runs_path <- file.path(temp_dir, "runs.csv")

    # Check if the session files actually exist
    if (!file.exists(feat_path) || !file.exists(runs_path)) {
      showNotification("No active session found to resume.", type = "error")
      return()
    }

    # 1. Load the remaining unclassified data
    data_storage$features <- fread(feat_path)
    data_storage$runs_table <- fread(runs_path)

    # 2. Update the file list for the audio player
    # These paths point to the .wav clips already sitting in the tmp folder
    data_storage$files_to_classify <- data_storage$runs_table$filepath

    # 3. Reset index to 1 (since finished runs were already removed from these files)
    data_storage$current_run <- 1

    # 4. Flip the UI switches
    shinyjs::hide("pre_process_sidebar")
    shinyjs::hide("main_placeholder")
    shinyjs::show("post_process_sidebar")
    shinyjs::show("main_ui")

    removeModal()
    showNotification(paste("Resumed session with", nrow(data_storage$runs_table), "remaining candidates."), type = "message")
  })

  observeEvent(list(input$btn_squawk, input$hotkey_1), { 
    handle_classification(data_storage, "Squawk", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output"))
  })
  observeEvent(list(input$btn_alarm, input$hotkey_2), { 
    handle_classification(data_storage, "Alarm Call", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output"))
  })
  observeEvent(list(input$btn_noise, input$hotkey_3), { 
    handle_classification(data_storage, "Noise", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output"))
  })
  observeEvent(list(input$btn_unknown, input$hotkey_4), { 
    handle_classification(data_storage, "Unknown", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output"))
  })
  observeEvent(list(input$btn_other, input$hotkey_5), { 
    handle_classification(data_storage, "Other Vocalisation", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output"))
  })
  
  observeEvent(input$btn_next, { 
    handle_classification(data_storage, "Skipped", temp_dir, here("Output"))
    check_completion(data_storage, temp_dir, here("Output")) 
    })

  observeEvent(input$btn_prev, {
    req(length(data_storage$history) > 0)
    
    # 1. Pop the last action off the stack
    last_action <- data_storage$history[[1]]
    data_storage$history <- data_storage$history[-1] 
    
    # 2. Physically move the file back to the 'tmp' folder
    if (file.exists(last_action$new_path)) {
      file.rename(last_action$new_path, last_action$old_path)
    }
    
    # 3. Clean up the Output CSVs (if it wasn't a skip)
    if (last_action$label != "Skipped") {
      # Remove from Master
      master_csv <- here("Output", "master_features.csv")
      if (file.exists(master_csv)) {
        m_df <- fread(master_csv)
        m_df <- m_df[run_id != last_action$run_id] # Purge the bad run
        fwrite(m_df, master_csv)
      }
      # Remove from Label Specific CSV
      label_csv <- here("Output", gsub(" ", "_", last_action$label), paste0(gsub(" ", "_", last_action$label), ".csv"))
      if (file.exists(label_csv)) {
        l_df <- fread(label_csv)
        l_df <- l_df[run_id != last_action$run_id]
        fwrite(l_df, label_csv)
      }
    }
    
    # 4. Reconstruct the features (Undo the label and filepath overwrites)
    restored_features <- copy(last_action$moved_features)
    restored_features[, user_class := NA] 
    restored_features[, filepath := last_action$old_path]
    
    # 5. Restore data to memory and disk in 'tmp'
    data_storage$features <- rbind(data_storage$features, restored_features, fill = TRUE)
    
    feat_path <- file.path(temp_dir, "features.csv")
    if (file.exists(feat_path)) {
      fwrite(rbind(fread(feat_path), restored_features, fill = TRUE), feat_path)
    }
    
    # Restore the runs.csv row (We can grab this right from memory!)
    runs_path <- file.path(temp_dir, "runs.csv")
    run_row <- data_storage$runs_table[run_id == last_action$run_id]
    if (file.exists(runs_path)) {
      fwrite(rbind(fread(runs_path), run_row, fill = TRUE), runs_path)
    }
    
    # 6. Decrement the UI
    data_storage$current_run <- max(1, data_storage$current_run - 1)
    
    # Save State
    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )
    
    showNotification(paste("Undid classification for Run", last_action$run_id), type = "warning")
  })

  # Restart button
  observeEvent(input$restart_app, {
    shinyjs::hide("completion_ui")
    shinyjs::hide("main_ui")
    shinyjs::hide("post_process_sidebar")
    shinyjs::show("pre_process_sidebar")
    shinyjs::show("main_placeholder")
    shinyjs::show("app_workspace")
    shinyjs::reset("upload_file")
    # Reset data storage
    data_storage$features <- NULL
    data_storage$current_run <- 1
    data_storage$files_to_classify <- NULL
    data_storage$runs_table <- NULL
  })
  
  # Zip and end button
  observeEvent(input$btn_prep_zip, {
    showModal(modalDialog(
      title = "Finalize & Archive Session",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_final_zip", "Confirm & Zip", class = "btn-success")
      ),
      p("Please enter your initials or an identifier. This will be added as a prefix to your output ZIP file."),
      textInput("user_id", "Identifier:", placeholder = "e.g., JO"),
      helpText("The app will close automatically after the ZIP is created.")
    ))
  })
  
  # 2. The actual zipping logic (triggered from inside the Modal)
  observeEvent(input$btn_final_zip, {
    req(input$user_id)
    
    user_prefix <- trimws(input$user_id)
    if (user_prefix == "") {
      showNotification("Identifier cannot be empty.", type = "error")
      return()
    }
    
    # Remove the modal so it doesn't hang
    removeModal()
    
    # Sanitize filename and create timestamp
    safe_prefix <- gsub("[^[:alnum:]]", "_", user_prefix)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    zip_filename <- paste0(safe_prefix, "_SquawkSpot_", timestamp, ".zip")
    
    withProgress(message = 'Creating Archive...', value = 0.5, {
      tryCatch({
        # Zip the Output folder
        zip::zipr(zipfile = zip_filename, 
                  files = here("Output"))
        
        showNotification(paste("Success! Created:", zip_filename), type = "message")
        
        # Wait and Close
        Sys.sleep(2)
        stopApp()
        
      }, error = function(e) {
        showNotification(paste("Zipping failed:", e$message), type = "error")
      })
    })
  })

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

  output$debug_state <- renderPrint({
    list(
      current_run_idx = data_storage$current_run,
      total_runs_in_table = if (!is.null(data_storage$runs_table)) nrow(data_storage$runs_table) else 0,
      files_to_classify_count = length(data_storage$files_to_classify),
      active_temp_files = list.files(temp_dir)
    )
  })


  # File info + progress
  output$file_info <- renderText({
    req(data_storage$files_to_classify, data_storage$current_run)
    total_runs <- length(data_storage$files_to_classify)
    current_run_idx <- data_storage$current_run
    req(current_run_idx <= total_runs) # Ensure run index is valid
    current_file_name <- basename(here(data_storage$files_to_classify[current_run_idx]))
    paste0("Run ", current_run_idx, " of ", total_runs, ": ", current_file_name)
  })

  # Check for existing session on app launch
  isolate({
    if (file.exists(file.path(temp_dir, "runs.csv")) && nrow(fread(file.path(temp_dir, "runs.csv"))) > 0) {
      showModal(modalDialog(
        title = "Interrupted Session Detected",
        "It looks like the last session wasn't finished. Would you like to resume classifying the remaining clips?",
        footer = tagList(
          actionButton("resume_btn", "Yes, Resume", class = "btn-success"),
          modalButton("No, Start Fresh")
        )
      ))
    }
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}
