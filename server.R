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
    
    # --- FIX: Sanitize the audio signal ---
    # Replace any non-finite values (NA, NaN, Inf) with 0 to prevent crashes.
    signal_sanitized <- chunk_wave@left
    signal_sanitized[!is.finite(signal_sanitized)] <- 0
    # ----------------------------------------
    
    # Waveform (oscillo)
    osc_data <- seewave::oscillo(signal_sanitized, f = chunk_wave@samp.rate, plot = FALSE)
    time_vector <- seq(0, duration, length.out = nrow(osc_data))
    
    # Spectrogram (spec object with $time, $freq, $amp)
    spec_data <- seewave::spectro(signal_sanitized, f = chunk_wave@samp.rate,
                                  plot = FALSE, osc = FALSE)
    
    # Determine chunk_start (slice_start) if runs_table includes it
    chunk_start <- 0
    if (!is.null(data_storage$runs_table) && nrow(data_storage$runs_table) >= run_idx) {
      # prefer slice_start column if exists (saved by group_and_slice_chunks)
      if ("slice_start" %in% names(data_storage$runs_table)) {
        ss <- data_storage$runs_table$slice_start[run_idx]
        if (!is.na(ss)) chunk_start <- as.numeric(ss)
      } else {
        # fallback to start_time (absolute) — client audio is a sliced file so playhead is relative to slice start
        ss2 <- data_storage$runs_table$start_time[run_idx]
        if (!is.na(ss2)) chunk_start <- as.numeric(ss2)
      }
    }
    
    # Highlight times made relative to chunk_start, clipped to [0, duration]
    highlight <- NULL
    times <- current_run_times()
    if (!is.null(times)) {
      start_rel <- times$start - chunk_start
      end_rel   <- times$end   - chunk_start
      # clamp:
      start_rel <- max(0, min(duration, start_rel))
      end_rel   <- max(0, min(duration, end_rel))
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
  
  # --- Render plots once per chunk (robust spectrogram & shapes) ---
  observeEvent(current_chunk_full(), {
    chunk <- current_chunk_full()
    
    # compute waveform y-range
    y_range_wave <- range(chunk$osc_data[,1], na.rm = TRUE)
    if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)
    
    # Build shapes (highlight only here; playhead added on playhead updates)
    shapes_base <- list()
    if (!is.null(chunk$highlight)) {
      shapes_base[[length(shapes_base) + 1]] <- list(
        type = "rect",
        x0 = chunk$highlight$start,
        x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper",
        fillcolor = "rgba(255, 255, 0, 0.3)",
        line = list(width = 0)
      )
    }
    # placeholder playhead line (x0==x1==0 until play begins)
    shapes_base[[length(shapes_base) + 1]] <- list(
      type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "red", width = 2)
    )
    
    # Render waveform fresh for this chunk
    output$waveform_plot <- renderPlotly({
      plot_ly(
        x = chunk$time_vector,
        y = chunk$osc_data[,1],
        type = 'scatter',
        mode = 'lines',
        hoverinfo = 'x+y',
        name = "Waveform"
      ) %>%
        layout(
          xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
          yaxis = list(range = y_range_wave, fixedrange = TRUE, title = "Amplitude"),
          shapes = shapes_base
        )
    })
    
    # --- ROBUST SPECTROGRAM PLOTTING WITH SAFETY NET ---
    output$spectrogram_plot <- renderPlotly({
      # First, check if the spectro output is minimally valid (has dimensions)
      if (!is.null(chunk$spec_data$amp) && all(dim(chunk$spec_data$amp) > 0)) {
        
        # --- OUTPUT SANITIZATION ---
        # Create a clean copy and replace any NaN/Inf values from spectro's internals with 0.
        amp_matrix <- chunk$spec_data$amp
        amp_matrix[!is.finite(amp_matrix)] <- 0
        # ---------------------------
        
        # Now, convert the guaranteed-clean matrix to dB
        amp_db <- 20 * log10(amp_matrix)
        finite_vals <- amp_db[is.finite(amp_db)]
        
        # This final check now only handles the true "all silent" case
        if (length(finite_vals) > 0) {
          # --- Plot the spectrogram ---
          min_finite_db <- min(finite_vals, na.rm = TRUE)
          amp_db[!is.finite(amp_db)] <- min_finite_db
          
          plot_ly(
            z = amp_db, x = chunk$spec_data$time, y = chunk$spec_data$freq,
            type = 'heatmap', colors = 'viridis', hoverinfo = 'x+y+z',
            colorbar = list(title = "dB"), showscale = TRUE
          ) %>%
            layout(
              xaxis = list(range = c(0, chunk$duration), fixedrange = TRUE, title = "Time (s)"),
              yaxis = list(title = "Frequency (kHz)"),
              shapes = shapes_base
            )
        } else {
          # Fallback for completely silent clips
          show_unavailable_message(chunk)
        }
      } else {
        # Fallback for empty/invalid spectro output
        show_unavailable_message(chunk)
      }
    })
  })
  
  # --- Update only playhead (fast) ---
  observeEvent(playhead_time(), {
    # only update the red line x position (fast; doesn't recompute spectrogram)
    chunk <- current_chunk_full()
    # prepare shapes: keep highlight and add playhead line at current playhead time
    shapes <- list()
    if (!is.null(chunk$highlight)) {
      shapes[[length(shapes)+1]] <- list(
        type = "rect",
        x0 = chunk$highlight$start,
        x1 = chunk$highlight$end,
        y0 = 0, y1 = 1, yref = "paper",
        fillcolor = "rgba(255, 255, 0, 0.3)",
        line = list(width = 0)
      )
    }
    ph <- playhead_time()
    # clamp to chunk duration
    ph <- max(0, min(chunk$duration, as.numeric(ph)))
    shapes[[length(shapes)+1]] <- list(
      type = "line",
      x0 = ph, x1 = ph, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = 'red', width = 2)
    )
    
    # apply to both plots
    plotlyProxy("waveform_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes = shapes))
    plotlyProxy("spectrogram_plot", session) %>% plotlyProxyInvoke("relayout", list(shapes = shapes))
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
    plot_ly(type='heatmap', colors='viridis', showscale=FALSE) %>%
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
  
  # --- File info + progress ---
  output$file_info <- renderText({
    req(data_storage$files_to_classify)
    total_runs <- length(data_storage$files_to_classify)
    current_file_name <- basename(here(data_storage$files_to_classify[data_storage$current_run]))
    paste0("Run ", data_storage$current_run, " of ", total_runs, ": ", current_file_name)
  })
  
  session$onSessionEnded(function() { stopApp() })
}