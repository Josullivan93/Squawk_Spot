# Helper Functions

# # Noise Reduction
# noise_reduce_channel <- function(channel_py, sr_py, nr_flag, nr_object) {
#   # Apply noise reduction (prop_decrease = 0.75 for balance)
#   signal_float <- nr_object$reduce_noise(y = channel_py, sr = sr_py, stationary = nr_flag, prop_decrease = 0.75)
#   return(as.numeric(py_to_r(signal_float)))
# }

# Preprocess Wav
preprocess_wav <- function(path, # Full filepath of wav file
                           pre_emph_coeff = NULL, # Pre-emphasis coefficient (0.9 to 1, e.g. 0.97)
                           noise_reduction = "None", # Noise Reduction "none", "s", "ns"
                           filter_type = "None", # "lowpass", "highpass", "bandpass", "none"
                           cutoff_highpass = NULL, # Lower frequency cutoff (Hz) for highpass or bandpass
                           cutoff_lowpass = NULL, # Upper frequency cutoff (Hz) for lowpass or bandpass
                           filter_order = 4, # Butterworth filter order
                           normalise = TRUE # Peak normalisation
) {
  # Match arguments
  noise_reduction <- match.arg(tolower(noise_reduction), c("none", "s", "ns"))
  filter_type <- match.arg(tolower(filter_type), c("none", "lowpass", "highpass", "bandpass"))

  # Load wav from path
  wav_obj <- readWave(path)

  sample_rate <- wav_obj@samp.rate
  nyquist <- sample_rate / 2
  is_stereo <- wav_obj@stereo

  # Type Conversion: Map integers (-32768:32767) to decimals (-1:1)
  wav_obj@left <- as.numeric(wav_obj@left) / 32768
  if (wav_obj@stereo) wav_obj@right <- as.numeric(wav_obj@right) / 32768
  wav_obj@bit <- 32 # change wav metadata
  wav_obj@pcm <- FALSE

  # Remove DC component
  wav_obj@left <- wav_obj@left - mean(wav_obj@left, na.rm = TRUE)
  if (wav_obj@stereo) wav_obj@right <- wav_obj@right - mean(wav_obj@right, na.rm = TRUE)


  # Apply Pre-emphasis
  if (!is.null(pre_emph_coeff)) {
    b <- c(1, -pre_emph_coeff) # Filter Coefficients
    a <- 1 # Denominator coefficient

    wav_obj@left <- as.numeric(signal::filter(b, a, as.numeric(wav_obj@left)))

    if (is_stereo) {
      wav_obj@right <- as.numeric(signal::filter(b, a, as.numeric(wav_obj@right)))
    }
  }

  # Apply Band filters

  if (filter_type != "none") {
    filter_coeffs <- NULL

    if (filter_type == "lowpass") {
      if (is.null(cutoff_lowpass)) stop("cutoff_lowpass must be provided for lowpass filter.")
      if (cutoff_lowpass <= 0 || cutoff_lowpass >= nyquist) stop("cutoff_lowpass must be between 0 and Nyquist frequency (", nyquist, " Hz).")
      Wn <- cutoff_lowpass / nyquist
      filter_coeffs <- butter(n = filter_order, W = Wn, type = "low")
    } else if (filter_type == "highpass") {
      if (is.null(cutoff_highpass)) stop("cutoff_highpass must be provided for highpass filter.")
      if (cutoff_highpass <= 0 || cutoff_highpass >= nyquist) stop("cutoff_highpass must be between 0 and Nyquist frequency (", nyquist, " Hz).")
      Wn <- cutoff_highpass / nyquist
      filter_coeffs <- butter(n = filter_order, W = Wn, type = "high")
    } else if (filter_type == "bandpass") {
      if (is.null(cutoff_highpass) || is.null(cutoff_lowpass)) stop("cutoff_highpass and cutoff_lowpass must be provided for bandpass filter.")
      if (cutoff_highpass <= 0 || cutoff_lowpass <= 0 || cutoff_highpass >= cutoff_lowpass || cutoff_lowpass >= nyquist) {
        stop("Bandpass cutoffs must satisfy 0 < cutoff_highpass < cutoff_lowpass < Nyquist frequency (", nyquist, " Hz).")
      }
      Wn <- c(cutoff_highpass / nyquist, cutoff_lowpass / nyquist)
      filter_coeffs <- butter(n = filter_order, W = Wn, type = "pass")
    } else {
      stop("Invalid filter_type. Choose 'lowpass', 'highpass', 'bandpass', or 'none'.")
    }

    # Apply filter
    if (!is.null(filter_coeffs)) {
      wav_obj@left <- filtfilt(filter_coeffs, as.numeric(wav_obj@left))
      if (is_stereo) {
        wav_obj@right <- filtfilt(filter_coeffs, as.numeric(wav_obj@right))
      }
    }
  }

  # Apply Noise Reduction
  # Requires confirmation that python is loaded and ready
  if (noise_reduction != "none") {
    # # Pass to python
    # left_py <- r_to_py(wav_obj@left)
    # sr_py <- r_to_py(wav_obj@samp.rate)
    # 
    # # set flag based on noise_reduction argument
    # nr_flag <- ifelse(noise_reduction == "s", TRUE, FALSE)
    # 
    # wav_obj@left <- noise_reduce_channel(left_py, sr_py, nr_flag, nr)
    # rm(left_py)
    # 
    # if (is_stereo) {
    #   right_py <- r_to_py(wav_obj@right)
    #   wav_obj@right <- noise_reduce_channel(right_py, sr_py, nr_flag, nr)
    #   rm(right_py)
    # }
  }
  if (normalise == TRUE) {
    # Remove DC Offset (Center the wave at zero)
    wav_obj@left <- wav_obj@left - mean(wav_obj@left, na.rm = TRUE)
    if (wav_obj@stereo) {
      wav_obj@right <- wav_obj@right - mean(wav_obj@right, na.rm = TRUE)
    }

    # Get the absolute maximum across all channels to maintain stereo balance
    max_val <- max(abs(c(wav_obj@left, if (wav_obj@stereo) wav_obj@right else NULL)))

    # Avoid division by zero for silent files
    if (max_val > 0) {
      # Scaling to 0.9 (approx -1dB) to avoid clipping during any subsequent processing
      wav_obj@left <- (wav_obj@left / max_val) * 0.9
      if (wav_obj@stereo) {
        wav_obj@right <- (wav_obj@right / max_val) * 0.9
      }
    }
  }

  # return processed wav
  return(wav_obj)
}

# classify_and_move
classify_and_move <- function(label, run_id, features_df = NULL,
                              temp_dir = NULL, output_dir = NULL, save_copy = FALSE) {
  if (missing(label) || missing(run_id)) stop("label and run_id are required.")
  if (is.null(temp_dir) && is.null(features_df)) {
    stop("Either 'features_df' (in-memory) or 'temp_dir' must be provided (features.csv lives in temp_dir).")
  }

  # default temp_dir detection
  if (is.null(temp_dir)) {
    tentative <- file.path(here("Output", "tmp"))
    if (file.exists(file.path(tentative, "features.csv"))) temp_dir <- tentative
  }

  # default output_dir = parent of temp_dir or here("Output")
  if (is.null(output_dir)) {
    if (!is.null(temp_dir)) {
      output_dir <- normalizePath(dirname(temp_dir), winslash = "/", mustWork = FALSE)
    } else {
      output_dir <- here("Output")
    }
  } else {
    if (!is.null(temp_dir) && normalizePath(output_dir, winslash = "/", mustWork = FALSE) == normalizePath(temp_dir, winslash = "/", mustWork = FALSE)) {
      output_dir <- normalizePath(dirname(temp_dir), winslash = "/", mustWork = FALSE)
      message("classify_and_move: output_dir equals temp_dir → writing classification folders to ", output_dir)
    }
  }

  # load features if not provided
  feat_path <- file.path(temp_dir, "features.csv")
  if (is.null(features_df)) {
    if (!file.exists(feat_path)) stop("features.csv not found in temp_dir: ", temp_dir)
    features_df <- fread(feat_path)
  } else {
    if (!is.data.table(features_df)) {
      features_df <- as.data.table(copy(features_df))
    } else {
      features_df <- copy(features_df)
    }
  }

  # read runs table if present
  runs_path <- file.path(temp_dir, "runs.csv")
  runs_dt <- NULL
  if (file.exists(runs_path)) {
    runs_dt <- fread(runs_path)
  }

  # required columns
  if (!"run_id" %in% names(features_df)) stop("features_df must contain 'run_id' column.")
  if (!"filepath" %in% names(features_df) && is.null(runs_dt)) {
    stop("features_df must contain 'filepath' or temp_dir must have runs.csv")
  }

  # --- Filter rows for the selected run_id only ---
  run_id_int <- as.integer(run_id)
  features_df[, run_id := as.integer(run_id)]
  rows_to_move <- features_df[run_id == run_id_int, ]
  if (nrow(rows_to_move) == 0) stop("No rows found for run_id = ", run_id_int)

  # resolve clip file path
  clip_path <- NULL
  if (!is.null(runs_dt) && "run_id" %in% names(runs_dt)) {
    runs_dt[, run_id := as.integer(run_id)]
    candidate <- runs_dt[run_id == run_id_int, filepath]
    if (length(candidate) >= 1 && file.exists(candidate[1])) clip_path <- normalizePath(candidate[1], winslash = "/", mustWork = FALSE)
  }
  if (is.null(clip_path)) {
    cand <- unique(rows_to_move$filepath)
    for (c in cand) {
      if (file.exists(c)) {
        clip_path <- normalizePath(c, winslash = "/", mustWork = FALSE)
        break
      }
      c2 <- file.path(temp_dir, basename(c))
      if (file.exists(c2)) {
        clip_path <- normalizePath(c2, winslash = "/", mustWork = FALSE)
        break
      }
    }
  }

  if (is.null(clip_path) || !file.exists(clip_path)) {
    stop("Could not resolve clip file for run_id=", run_id_int)
  }

  if (startsWith(clip_path, output_dir) && !startsWith(clip_path, temp_dir)) {
    message("File already classified. Skipping move.")
    return(features_df)
  }

  # ensure output dirs exist
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  label_dir <- file.path(output_dir, gsub(" ", "_", label))
  if (!dir.exists(label_dir)) dir.create(label_dir, recursive = TRUE)

  # --- destination file with collision safeguard ---
  base_name <- tools::file_path_sans_ext(basename(clip_path))
  ext <- tools::file_ext(clip_path)
  safe_name <- paste0(base_name, "_run", run_id_int, ".", ext)
  new_clip_path <- file.path(label_dir, safe_name)

  # Save annotated copy before original is moved/deleted <<<
  annotated_path <- NULL
  is_skip <- (label == "Skipped")
  if (save_copy && !is_skip) {
    annotated_dir <- file.path(output_dir, "Annotated_Copies")
    if (!dir.exists(annotated_dir)) dir.create(annotated_dir, recursive = TRUE)
    
    annotated_path <- file.path(annotated_dir, safe_name)
    file.copy(clip_path, annotated_path, overwrite = TRUE)
  }
  
  # move or copy
  moved <- FALSE
  try(
    {
      moved <- file.rename(clip_path, new_clip_path)
    },
    silent = TRUE
  )
  if (!moved) {
    copied <- file.copy(clip_path, new_clip_path, overwrite = TRUE)
    if (!copied) stop("Failed to move or copy clip to ", new_clip_path)
    unlink(clip_path)
  }

  # Assign label ONLY to this run’s rows
  rows_to_move[, user_class := label]
  rows_to_move[, filepath := new_clip_path]
  
  # Dont write if skipped
  #is_skip <- (label == "Skipped")
  
  if(!is_skip){
    # Append these rows to master_features.csv
    master_csv <- file.path(output_dir, "master_features.csv")
    if (file.exists(master_csv)) {
      master_df <- fread(master_csv)
      master_df <- rbind(master_df, rows_to_move, fill = TRUE)
      fwrite(master_df, master_csv)
    } else {
      fwrite(rows_to_move, master_csv)
    }

    # Append to label-specific CSV
    label_csv <- file.path(label_dir, paste0(gsub(" ", "_", label), ".csv"))
    if (file.exists(label_csv)) {
      lab_df <- fread(label_csv)
      lab_df <- rbind(lab_df, rows_to_move, fill = TRUE)
      fwrite(lab_df, label_csv)
    } else {
      fwrite(rows_to_move, label_csv)
    }
  } else {
    
    skip_log_csv <- file.path(output_dir, "skipped_features.csv")
    if(file.exists(skip_log_csv)){
      
      skip_df <- fread(skip_log_csv)
      skip_df <- rbind(skip_df, rows_to_move, fill = TRUE)
      fwrite(skip_df, skip_log_csv)
      
    } else { 
      fwrite(rows_to_move, skip_log_csv)
      }
    
  }
  
  # Remove only this run from features_df and save back to tmp
  remaining <- features_df[run_id != run_id_int | is.na(run_id), ]
  fwrite(remaining, feat_path)

  # Remove this run from runs.csv
  if (!is.null(runs_dt)) {
    runs_dt[, run_id := as.integer(run_id)]
    remaining_runs <- runs_dt[run_id != run_id_int]
    fwrite(remaining_runs, runs_path)
  }

  message(sprintf("run_id=%s labelled '%s' and clip moved to: %s", run_id_int, label, new_clip_path))

  # Return a receipt of action taken
  return(list(
    remaining_features = remaining,
    moved_features = rows_to_move,
    old_path = clip_path,
    new_path = new_clip_path,
    annotated_path = annotated_path
  ))
  
}

# Classification button
classify_run <- function(features_df, runs_table, current_run_idx, label, temp_dir, output_dir, save_copy = FALSE) {
  # Guard: check runs_table exists and run index is valid
  if (is.null(runs_table) || current_run_idx > nrow(runs_table)) {
    return(features_df)
  }

  req_row <- runs_table[current_run_idx, ]
  current_run_id <- req_row$run_id

  if (length(current_run_id) == 0) {
    return(features_df)
  }

  # Call classify_and_move
  updated_features <- classify_and_move(
    label = label,
    run_id = current_run_id,
    features_df = features_df,
    temp_dir = temp_dir,
    output_dir = output_dir,
    save_copy = save_copy
  )

  return(updated_features)
}

# Cleanup function
ann_cleanup <- function(temp_dir = here("Output", "tmp"),
                        output_dir = here("Output")) {
  features_path <- file.path(temp_dir, "features.csv")
  null_csv <- file.path(output_dir, "Null_Annotations.csv")

  # Move or append remaining features
  if (file.exists(features_path)) {
    remaining_features <- fread(features_path)

    if (file.exists(null_csv)) {
      # Append to existing Null_Annotations.csv
      existing_null <- fread(null_csv)
      combined <- rbind(existing_null, remaining_features, fill = TRUE)
      fwrite(combined, null_csv)
    } else {
      # First-time creation
      fwrite(remaining_features, null_csv)
    }

    # Remove features.csv from tmp
    unlink(features_path)
  }

  # Remove all WAV files in tmp
  if (basename(temp_dir) != "tmp") stop("Cleanup aborted: temp_dir must point specifically to the 'tmp' folder.")
  wav_files <- list.files(temp_dir, pattern = "\\.wav$", full.names = TRUE)
  if (length(wav_files) > 0) unlink(wav_files)

  # Optional: remove runs.csv
  runs_path <- file.path(temp_dir, "runs.csv")
  if (file.exists(runs_path)) unlink(runs_path)

  message("Temporary folder cleaned and Null_Annotations updated.")
}

# Helper: generate shapes for Plotly
generate_shapes <- function(playhead = NULL, highlight = NULL) {
  shapes <- list()

  # highlight rectangle
  if (!is.null(highlight)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "rect",
      x0 = highlight$start,
      x1 = highlight$end,
      y0 = 0, y1 = 1, yref = "paper",
      fillcolor = "rgba(255, 255, 0, 0.3)",
      line = list(width = 0)
    )
  }

  # playhead line
  if (!is.null(playhead)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line",
      x0 = playhead, x1 = playhead,
      y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "red", dash = "dash")
    )
  }

  shapes
}

# Helper function to avoid repeating the "unavailable" plot code
show_unavailable_message <- function(chunk) {
  plot_ly() %>%
    layout(
      xaxis = list(range = c(0, chunk$duration), title = "Time (s)"),
      yaxis = list(title = "Frequency (kHz)", range = c(0, chunk$wave@samp.rate / 2000)),
      annotations = list(
        x = chunk$duration / 2, y = (chunk$wave@samp.rate / 2000) / 2,
        text = "Spectrogram not available",
        showarrow = FALSE, font = list(size = 16, color = "grey")
      )
    )
}

# Helper to handle completion
check_completion <- function(data_storage, temp_dir, output_dir) {
  # 1. First, ensure there is actually data loaded
  if (is.null(data_storage$runs_table)) {
    return(FALSE)
  }

  # --- Automatic cleanup if last run ---
  if (data_storage$current_run > nrow(data_storage$runs_table)) {
    ann_cleanup(temp_dir = temp_dir, output_dir = output_dir)
    message("All runs classified — tmp folder cleaned automatically.")

    shinyjs::hide("app_workspace")
    shinyjs::show("completion_ui")
    showNotification("Session Complete! All candidates reviewed.", type = "message")
    return(TRUE)
  }
  return(FALSE)
}

# Function to handle annotation logic
handle_classification <- function(data_storage, label_name, temp_dir, output_dir, save_copy = FALSE) {
  
  if (data_storage$current_run > length(data_storage$files_to_classify)) {
    return(NULL) # Prevent out-of-bounds errors
  }
  
  # Run the classification (which now returns history)
  res <- classify_run(
    features_df = data_storage$features,
    runs_table = data_storage$runs_table,
    current_run_idx = data_storage$current_run,
    label = label_name,
    temp_dir = temp_dir,
    output_dir = output_dir,
    save_copy = save_copy
  )
  
  # Update memory
  data_storage$features <- res$remaining_features
  
  # Push the receipt onto the history stack
  action_record <- list(
    run_id = data_storage$runs_table$run_id[data_storage$current_run],
    label = label_name,
    old_path = res$old_path,
    new_path = res$new_path,
    annotated_path = res$annotated_path#,
    #moved_features = res$moved_features
  )
  data_storage$history <- c(list(action_record), data_storage$history)
  
  if (length(data_storage$history) > 25) {
    data_storage$history <- data_storage$history[1:25]
  }
  
  # Advance the UI
  data_storage$current_run <- data_storage$current_run + 1
  
  saveRDS(
    list(runs = data_storage$runs_table,
         feats = data_storage$features,
         history = data_storage$history,
         current = data_storage$current_run
         ),
    file.path(temp_dir, "app_state.rds")
  )
  
  gc()
  
  return(TRUE)
}
