if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(here, tidyverse, data.table, signal, seewave, tuneR, parallel, future, future.apply, reticulate)
# Set up and use a dedicated Python virtual environment for consistency
if (!reticulate::virtualenv_exists("r-reticulate")) {
  message("Creating Python virtual environment 'r-reticulate'...")
  reticulate::virtualenv_create("r-reticulate", packages = c("noisereduce", "scipy", "numpy"))
}
reticulate::use_virtualenv("r-reticulate", required = TRUE)


# Helper Functions

# PSD and flux helpers 
psd_calc <- function(win_dat,
                     samp_rate,
                     fft_len = 2^ceiling(log2(length(win_dat)))
) {
  spec_out <- tryCatch({
    seewave::spec(win_dat, f = samp_rate, wl = fft_len, wn = "hanning",
                  PSD = TRUE, norm = FALSE, dB = NULL, plot = FALSE)
  }, error = function(e) {
    warning("Could not compute spectrum: ", e$message)
    return(NULL)
  })
  return(spec_out)
}

flux_calc <- function(psd_pair) {
  spec_curr <- psd_pair$curr
  spec_prev <- psd_pair$prev
  if (is.null(spec_curr) || is.null(spec_prev) ||
      !is.matrix(spec_curr) || !is.matrix(spec_prev) ||
      nrow(spec_curr) != nrow(spec_prev)) {
    return(NA_real_)
  }
  flux <- sqrt(sum((spec_curr[, 2] - spec_prev[, 2])^2, na.rm = TRUE))
  return(flux)
}

# process_wav
process_wav <- function(wav_path, stationary_filter, nonstationary_filter, low_pass_hz, high_pass_hz, window_size, window_overlap) {
  message("Processing Audio...")
  wav_obj <- readWave(wav_path)
  proc_wav <- wav_obj
  sample_rate <- proc_wav@samp.rate
  is_stereo <- proc_wav@stereo
  signal_left <- as.numeric(proc_wav@left)
  if (is_stereo) signal_right <- as.numeric(proc_wav@right)
  
  # noise reduction via python (unchanged)
  if (stationary_filter) {
    message("Applying Stationary Noise Reduction...")
    py_left <- r_to_py(signal_left)
    py_rate <- r_to_py(sample_rate)
    nr <- import("noisereduce")
    new_signal <- nr$reduce_noise(y = py_left, sr = py_rate, stationary = TRUE, prop_decrease = 0.99)
    signal_left <- as.numeric(py_to_r(new_signal)); rm(new_signal)
    if (is_stereo) {
      py_right <- r_to_py(signal_right)
      new_signal <- nr$reduce_noise(y = py_right, sr = py_rate, stationary = TRUE)
      signal_right <- as.numeric(py_to_r(new_signal)); rm(new_signal)
      
    }
  } else if (nonstationary_filter) {
    message("Applying Non-Stationary Noise Reduction...")
    py_left <- r_to_py(signal_left)
    py_rate <- r_to_py(sample_rate)
    nr <- import("noisereduce")
    new_signal <- nr$reduce_noise(y = py_left, sr = py_rate, stationary = FALSE, prop_decrease = 0.99)
    signal_left <- as.numeric(py_to_r(new_signal)); rm(new_signal)
    if (is_stereo) {
      py_right <- r_to_py(signal_right)
      new_signal <- nr$reduce_noise(y = py_right, sr = py_rate, stationary = FALSE)
      signal_right <- as.numeric(py_to_r(new_signal)); rm(new_signal)
    }
  }
  
  if (is_stereo) {
    proc_wav <- tuneR::Wave(left = signal_left, right = signal_right, samp.rate = sample_rate, bit = proc_wav@bit)
  } else {
    proc_wav <- tuneR::Wave(left = signal_left, samp.rate = sample_rate, bit = proc_wav@bit)
  }
  
  # frequency filtering (unchanged)
  if ((!is.na(low_pass_hz) | !is.na(high_pass_hz))) {
    message("Applying Frequency Filtering...")
    from_hz <- high_pass_hz
    to_hz <- low_pass_hz
    if (!is.na(from_hz) & !is.na(to_hz) && from_hz > to_hz) {
      temp_hz <- from_hz; from_hz <- to_hz; to_hz <- temp_hz
    }
    proc_wav <- ffilter(proc_wav, from = from_hz, to = to_hz)
  }
  
  # Calculate features (calls calc_features below)
  features_df <- calc_features(proc_wav, window_size, window_overlap)
  message("Audio Processing and Feature Calculation Complete.")
  return(list(proc_wav = proc_wav, features_df = features_df))
}

# process_single_window
process_single_window <- function(index, step_size, wav_data, win_len, samp_rate, fft_len) {
  start_sample <- ((index - 1) * step_size) + 1
  end_sample <- start_sample + win_len - 1
  if (end_sample > length(wav_data)) return(NULL)
  window_audio <- wav_data[start_sample:end_sample]
  prev_start_sample <- ((index - 2) * step_size) + 1
  prev_end_sample <- prev_start_sample + win_len - 1
  prev_window_audio <- wav_data[prev_start_sample:prev_end_sample]
  
  rms_val <- NA; spec_centroid_val <- NA; spec_entropy_val <- NA
  spec_skew_val <- NA; spec_kurt_val <- NA; flux_val <- NA
  
  tryCatch({
    rms_val <- sqrt(mean(window_audio^2, na.rm = TRUE))
    psd_obj <- psd_calc(window_audio, samp_rate = samp_rate, fft_len = fft_len)
    spec_props <- tryCatch({
      seewave::specprop(spec = psd_obj, f = samp_rate, plot = FALSE)
    }, error = function(e) {
      warning("Could not compute spectral properties: ", e$message); return(NULL)
    })
    if (!is.null(spec_props)) {
      spec_centroid_val <- spec_props$cent
      spec_entropy_val <- spec_props$sh
      spec_skew_val <- spec_props$skewness
      spec_kurt_val <- spec_props$kurtosis
    }
    psd_prev <- psd_calc(prev_window_audio, samp_rate = samp_rate, fft_len = fft_len)
    if (!is.null(psd_obj) && !is.null(psd_prev)) flux_val <- flux_calc(list(prev = psd_prev, curr = psd_obj))
  }, error = function(e) {
    warning(paste("Window index", index, ": Could not compute features:", e$message))
  })
  
  return(list(
    rms = rms_val,
    centroid = spec_centroid_val,
    entropy = spec_entropy_val,
    skewness = spec_skew_val,
    kurtosis = spec_kurt_val,
    flux = flux_val,
    window_index = index,
    start_time = (start_sample - 1) / samp_rate,
    end_time = (end_sample - 1) / samp_rate
  ))
}

# calc_features
calc_features <- function(proc_wav, window_size, window_overlap) {
  message('Calculating Features...')
  plan(multisession, workers = round(parallel::detectCores() - 2))
  samp_rate <- proc_wav@samp.rate
  wav_data <- proc_wav@left
  win_len <- round(window_size * samp_rate)
  step_size <- floor(win_len * (1 - window_overlap / 100))
  num_windows <- floor((length(wav_data) - win_len) / step_size) + 1
  standard_fft_len <- 2^ceiling(log2(win_len))
  message(paste("Processing", num_windows, "windows in parallel..."))
  
  results_list <- future.apply::future_lapply(
    X = 2:num_windows,
    FUN = process_single_window,
    future.chunk.size = 1000,
    step_size = step_size,
    wav_data = wav_data,
    win_len = win_len,
    samp_rate = samp_rate,
    fft_len = standard_fft_len,
    future.packages = c("seewave")
  )
  
  message("Combining features and finalising...")
  first_window <- wav_data[1:win_len]
  first_psd <- psd_calc(first_window, samp_rate = samp_rate, fft_len = standard_fft_len)
  first_spec <- tryCatch({
    seewave::specprop(spec = first_psd, f = samp_rate, plot = FALSE)
  }, error = function(e) { warning("Could not compute spectral properties: ", e$message); return(NULL) })
  
  first_cent <- NA; first_entropy <- NA; first_skew <- NA; first_kurt <- NA
  if (!is.null(first_spec)) {
    first_cent <- first_spec$cent; first_entropy <- first_spec$sh
    first_skew <- first_spec$skewness; first_kurt <- first_spec$kurtosis
  }
  
  features_1 <- list(
    rms = sqrt(mean(first_window^2, na.rm = TRUE)),
    centroid = first_cent,
    entropy = first_entropy,
    skewness = first_skew,
    kurtosis = first_kurt,
    flux = as.numeric(NA),
    window_index = 1,
    start_time = 0,
    end_time = (win_len - 1) / samp_rate
  )
  
  valid_results <- results_list[!sapply(results_list, is.null)]
  all_results_list <- c(list(features_1), valid_results)
  final_features <- rbindlist(all_results_list, fill = TRUE)
  setorder(final_features, window_index)
  
  # Add classification columns
  final_features$auto_class <- "Unclassified"
  final_features$user_class <- "Unclassified"
  
  for (i in 1:nrow(final_features)) {
    final_features$auto_class[i] <- ifelse(final_features$rms[i] > 28.32544 & final_features$centroid[i] > 1635.022, "Squawk", "Non-Squawk")
  }
  
  return(final_features)
}

# group_and_slice_chunks
group_and_slice_chunks <- function(features_df, full_wave, positive_class,
                                   buffer_time = 1.0, temp_dir,
                                   target_length = 3.0) {
  # Ensure temp dir exists
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  
  # Ensure data.table
  if (!is.data.table(features_df)) features_df <- as.data.table(copy(features_df))
  
  # Require auto_class column
  if (!"auto_class" %in% names(features_df)) {
    stop("features_df must contain a column named 'auto_class'")
  }
  
  # Normalize auto_class to character & ensure ordering
  features_df[, auto_class := as.character(auto_class)]
  if ("window_index" %in% names(features_df)) setorder(features_df, window_index)
  
  # compute logical vector (avoid NSE scoping issues)
  is_pos <- features_df$auto_class == positive_class
  
  # identify run starts and assign run_id (only positive windows get run_id)
  run_start_vec <- is_pos & !c(FALSE, head(is_pos, -1))    # TRUE when a positive window follows a non-positive
  run_id_vec <- cumsum(run_start_vec)
  run_id_vec[!is_pos] <- NA_integer_
  features_df[, run_id := run_id_vec]
  features_df[, is_positive := is_pos]
  
  # Make a runs table (one row per run_id)
  runs_dt <- features_df[!is.na(run_id), .(
    start_time = min(start_time, na.rm = TRUE),
    end_time   = max(end_time, na.rm = TRUE),
    windows    = list(window_index)
  ), by = run_id]
  if (nrow(runs_dt) == 0) {
    # nothing to slice
    fwrite(features_df, file.path(temp_dir, "features.csv"))
    save(full_wave, file = file.path(temp_dir, "full_wave.RData"))
    fwrite(runs_dt, file.path(temp_dir, "runs.csv"))
    return(list(updated_features_df = features_df,
                file_paths = character(0),
                run_metadata = list(),
                runs_table = runs_dt))
  }
  
  # process runs in order
  setorder(runs_dt, run_id)
  clip_paths <- character(nrow(runs_dt))
  run_metadata <- vector("list", nrow(runs_dt))
  
  dur <- length(full_wave@left) / full_wave@samp.rate
  
  for (i in seq_len(nrow(runs_dt))) {
    rid <- runs_dt$run_id[i]
    st <- runs_dt$start_time[i]
    ed <- runs_dt$end_time[i]
    
    # extend to target_length if shorter; do NOT cut long runs
    if ((ed - st) < target_length) ed <- st + target_length
    
    # buffer applies only to slice; it does NOT affect merging/detection logic
    slice_start <- max(0, st - buffer_time)
    slice_end   <- min(ed + buffer_time, dur)
    
    # Convert start/end to sample index for slicing
    slice_start_ind <- slice_start * full_wave@samp.rate + 1
    slice_end_ind <-  slice_end * full_wave@samp.rate
    
    # slice and save
    clip <- full_wave@left[slice_start_ind:slice_end_ind]
    clip <- Wave(left = clip, samp.rate = full_wave@samp.rate, bit = full_wave@bit, pcm = full_wave@pcm)
    
    out_name <- paste0("run_", sprintf("%04d", rid),
                       "_", sprintf("%.2f-%.2f", slice_start, slice_end), ".wav")
    out_path <- file.path(temp_dir, out_name)
    savewav(clip, filename = out_path) # Use seewave::savwav as implementation of writeWave with normalise step included
    clip_paths[i] <- out_path
    
    # metadata
    run_metadata[[i]] <- list(
      run_id = rid,
      windows = unlist(runs_dt$windows[i]),
      filepath = out_path,
      start_time = st,
      end_time = ed,
      slice_start = slice_start,
      slice_end = slice_end
    )
    
    # annotate features_df windows with filepath for convenience
    features_df[run_id == rid, filepath := out_path]
  }
  
  # Persist outputs
  fwrite(features_df, file.path(temp_dir, "features.csv"))
  
  # make runs table to save (with simpler structure)
  runs_export <- data.table(
    run_id = runs_dt$run_id,
    start_time = runs_dt$start_time,
    end_time = runs_dt$end_time,
    filepath = clip_paths,
    slice_start = sapply(run_metadata, function(x) x$slice_start),
    slice_end   = sapply(run_metadata, function(x) x$slice_end)
  )
  fwrite(runs_export, file.path(temp_dir, "runs.csv"))
  save(full_wave, file = file.path(temp_dir, "full_wave.RData"))
  
  return(list(
    updated_features_df = features_df,
    file_paths = clip_paths,
    run_metadata = run_metadata,
    runs_table = runs_export
  ))
}

# classify_and_move
classify_and_move <- function(label, run_id, features_df = NULL,
                              temp_dir = NULL, output_dir = NULL) {
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
      output_dir <- normalizePath(dirname(temp_dir))
    } else {
      output_dir <- here("Output")
    }
  } else {
    if (!is.null(temp_dir) && normalizePath(output_dir) == normalizePath(temp_dir)) {
      output_dir <- normalizePath(dirname(temp_dir))
      message("classify_and_move: output_dir equals temp_dir → writing classification folders to ", output_dir)
    }
  }
  
  # load features if not provided
  feat_path <- file.path(temp_dir, "features.csv")
  if (is.null(features_df)) {
    if (!file.exists(feat_path)) stop("features.csv not found in temp_dir: ", temp_dir)
    features_df <- fread(feat_path)
  } else {
    if (!is.data.table(features_df)) features_df <- as.data.table(copy(features_df))
    else features_df <- copy(features_df)
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
    if (length(candidate) >= 1 && file.exists(candidate[1])) clip_path <- candidate[1]
  }
  if (is.null(clip_path)) {
    cand <- unique(rows_to_move$filepath)
    for (c in cand) {
      if (file.exists(c)) { clip_path <- c; break }
      c2 <- file.path(temp_dir, basename(c))
      if (file.exists(c2)) { clip_path <- c2; break }
    }
  }
  if (is.null(clip_path) || !file.exists(clip_path)) {
    stop("Could not resolve clip file for run_id=", run_id_int)
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
  
  # move or copy
  moved <- FALSE
  try({ moved <- file.rename(clip_path, new_clip_path) }, silent = TRUE)
  if (!moved) {
    copied <- file.copy(clip_path, new_clip_path, overwrite = TRUE)
    if (!copied) stop("Failed to move or copy clip to ", new_clip_path)
    unlink(clip_path)
  }
  
  # Assign label ONLY to this run’s rows
  rows_to_move[, user_class := label]
  rows_to_move[, filepath := new_clip_path]
  
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
  
  # --- Automatic cleanup if last run ---
  if (nrow(remaining) == 0) {
    ann_cleanup(temp_dir = temp_dir, output_dir = output_dir)
    message("All runs classified — tmp folder cleaned automatically.")
  }
  
  invisible(remaining)
}

# Classification button
classify_run <- function(features_df, runs_table, current_run_idx, label, temp_dir, output_dir) {
  
  # Guard: check runs_table exists and run index is valid
  if (is.null(runs_table) || current_run_idx > nrow(runs_table)) return(features_df)
  
  current_run_id <- runs_table$run_id[current_run_idx]
  if (length(current_run_id) == 0) return(features_df)
  
  # Call classify_and_move
  updated_features <- classify_and_move(
    label = label,
    run_id = current_run_id,
    features_df = features_df,
    temp_dir = temp_dir,
    output_dir = output_dir
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
  if(!is.null(highlight)) {
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
  if(!is.null(playhead)) {
    shapes[[length(shapes) + 1]] <- list(
      type = 'line',
      x0 = playhead, x1 = playhead,
      y0 = 0, y1 = 1, yref = 'paper',
      line = list(color = 'red', dash = 'dash')
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
