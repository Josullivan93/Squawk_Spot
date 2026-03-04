# server.R: Contains the core logic of the Shiny application.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(here, shiny, dplyr, data.table, seewave, tuneR, shinyjs, plotly, ranger)
source(here("helper.R"))

options(shiny.maxRequestSize = 30 * 1024^2)

# Path to your final trained models
# path_vocal_model <- here("models", "final_vocal_model.rds")
squawk_model <- readRDS(here("models", "final_squawk_model.rds"))
# Extract feature names
features_used <- squawk_model$forest$independent.variable.names
# message(features_used)
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
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(227, 79, 38, 0.3)", line = list(width = 0)
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
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(227, 79, 38, 0.3)", line = list(width = 0.5, color = "rgba(227, 79, 38, 1)")
      )
    }

    plot_ly(z = amp_db, x = chunk$spec_data$time, y = chunk$spec_data$freq, type = "heatmap", colors = "viridis", hoverinfo = "x+y+z", showscale = FALSE) |>
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
    if (input$show_highlight && !is.null(chunk$highlight)) {
      shapes[[1]] <- list(
        type = "rect", x0 = chunk$highlight$start, x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(227, 79, 38, 0.3)", line = list(width = 0.5, color = "rgba(227, 79, 38, 1)")
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

  # Temp directory & resource path
  temp_dir <- here("Output", "tmp")
  output_dir <- here("Output")
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
    req(data_storage$current_run <= length(data_storage$files_to_classify))

    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Squawk",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1

    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )

    check_completion(data_storage, temp_dir, output_dir)
  })
  
  observeEvent(list(input$btn_alarm, input$hotkey_2), {
    req(data_storage$current_run <= length(data_storage$files_to_classify))
    
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Alarm",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1
    check_completion(data_storage)
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

    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )

    check_completion(data_storage, temp_dir, output_dir)
  })

  observeEvent(list(input$btn_unknown, input$hotkey_4), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Unknown",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1

    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )

    check_completion(data_storage, temp_dir, output_dir)
  })

  observeEvent(list(input$btn_noise, input$hotkey_3), {
    data_storage$features <- classify_run(
      features_df = data_storage$features,
      runs_table = data_storage$runs_table,
      current_run_idx = data_storage$current_run,
      label = "Noise",
      temp_dir = temp_dir,
      output_dir = here("Output")
    )
    data_storage$current_run <- data_storage$current_run + 1

    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )

    check_completion(data_storage, temp_dir, output_dir)
  })

  observeEvent(input$btn_next, {
    data_storage$current_run <- data_storage$current_run + 1

    saveRDS(
      list(runs = data_storage$runs_table, feats = data_storage$features),
      file.path(temp_dir, "app_state.rds")
    )

    check_completion(data_storage, temp_dir, output_dir)
  })

  observeEvent(input$btn_prev, {
    data_storage$current_run <- max(1, data_storage$current_run - 1)
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
