if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(
  future.apply, tidyverse, matrixStats, seewave, tuneR, 
  reticulate, here, progress, signal, data.table
)

# 1. Python Environment Setup ----
# This runs once when source(helper.R) is called by server.R
cat("Setting up Python environment...\n")
tryCatch({
  if (!reticulate::virtualenv_exists("r-reticulate")) {
    reticulate::virtualenv_create("r-reticulate", packages = c("numpy", "scipy", "noisereduce"))
  }
  reticulate::use_virtualenv("r-reticulate", required = TRUE)
  nr <- import("noisereduce", convert = FALSE)
  cat("Python (noisereduce) loaded successfully.\n")
}, error = function(e) {
  warning("Python setup failed. Noise reduction will be unavailable. Error: ", e$message)
})

# Helper Functions

# Noise Reduction
noise_reduce_channel <- function(channel_py, sr_py, nr_flag, nr_object){
  # Apply noise reduction (prop_decrease = 0.75 for balance)
  signal_float <- nr_object$reduce_noise(y = channel_py, sr = sr_py, stationary = nr_flag, prop_decrease = 0.75)
  return(as.numeric(py_to_r(signal_float)))
}

# Preprocess Wav
preprocess_wav <- function(path,                  # Full filepath of wav file
                           pre_emph_coeff = NULL, # Pre-emphasis coefficient (0.9 to 1, e.g. 0.97)
                           noise_reduction = "None", # Noise Reduction "none", "s", "ns"
                           filter_type = "None",  # "lowpass", "highpass", "bandpass", "none"
                           cutoff_highpass = NULL,     # Lower frequency cutoff (Hz) for highpass or bandpass
                           cutoff_lowpass = NULL,    # Upper frequency cutoff (Hz) for lowpass or bandpass
                           filter_order = 4,       # Butterworth filter order
                           normalise = TRUE        # Peak normalisation
){
  # Match arguments
  noise_reduction <- match.arg(tolower(noise_reduction), c("none", "s", "ns"))
  filter_type <- match.arg(tolower(filter_type), c("none", "lowpass", "highpass", "bandpass"))
  
  # Load wav from path
  wav_obj <- readWave(path)
  
  sample_rate <- wav_obj@samp.rate
  nyquist <- sample_rate/2
  is_stereo <- wav_obj@stereo
  
  # Type Conversion: Map integers (-32768:32767) to decimals (-1:1)
  wav_obj@left <- as.numeric(wav_obj@left) / 32768
  if(wav_obj@stereo) wav_obj@right <- as.numeric(wav_obj@right) / 32768
  wav_obj@bit <- 32 # change wav metadata
  wav_obj@pcm <- FALSE
  
  # Remove DC component
  wav_obj@left <- wav_obj@left - mean(wav_obj@left, na.rm = TRUE)
  if(wav_obj@stereo) wav_obj@right <- wav_obj@right - mean(wav_obj@right, na.rm = TRUE)
  
  
  # Apply Pre-emphasis
  if (!is.null(pre_emph_coeff)){
    
    b <- c(1, -pre_emph_coeff) # Filter Coefficients
    a <- 1 # Denominator coefficient
    
    wav_obj@left <- as.numeric(signal::filter(b, a, as.numeric(wav_obj@left)))
    
    if(is_stereo){
      wav_obj@right <- as.numeric(signal::filter(b, a, as.numeric(wav_obj@right)))
    }
    
  }
  
  # Apply Band filters
  
  if (filter_type != "none"){
    
    filter_coeffs <- NULL
    
    if (filter_type == "lowpass"){
      if (is.null(cutoff_lowpass)) stop("cutoff_lowpass must be provided for lowpass filter.")
      if (cutoff_lowpass <= 0 || cutoff_lowpass >= nyquist) stop("cutoff_lowpass must be between 0 and Nyquist frequency (", nyquist, " Hz).")
      Wn <- cutoff_lowpass / nyquist
      filter_coeffs <- butter(n = filter_order, W = Wn, type = "low")
      
    } else if (filter_type == "highpass"){
      if (is.null(cutoff_highpass)) stop("cutoff_highpass must be provided for highpass filter.")
      if (cutoff_highpass <= 0 || cutoff_highpass >= nyquist) stop("cutoff_highpass must be between 0 and Nyquist frequency (", nyquist, " Hz).")
      Wn <- cutoff_highpass / nyquist
      filter_coeffs <- butter(n = filter_order, W = Wn, type = "high")
      
    } else if (filter_type == "bandpass"){
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
    if (!is.null(filter_coeffs)){
      wav_obj@left <- filtfilt(filter_coeffs, as.numeric(wav_obj@left))
      if(is_stereo){
        wav_obj@right <- filtfilt(filter_coeffs, as.numeric(wav_obj@right))
      }
    }
  }
  
  # Apply Noise Reduction
  # Requires confirmation that python is loaded and ready
  if ( noise_reduction != "none"){
    
    # Pass to python
    left_py <- r_to_py(wav_obj@left)
    sr_py <- r_to_py(wav_obj@samp.rate)
    
    # set flag based on noise_reduction argument
    nr_flag <- ifelse(noise_reduction == "s", TRUE, FALSE)
    
    wav_obj@left <- noise_reduce_channel(left_py, sr_py, nr_flag, nr)
    rm(left_py)
    
    if(is_stereo){
      
      right_py <- r_to_py(wav_obj@right)
      wav_obj@right <- noise_reduce_channel(right_py, sr_py, nr_flag, nr)
      rm(right_py)
    }
    
  }
  
  if (normalise == TRUE) {
    
    # Remove DC Offset (Center the wave at zero)
    wav_obj@left <- wav_obj@left - mean(wav_obj@left, na.rm = TRUE)
    if(wav_obj@stereo) {
      wav_obj@right <- wav_obj@right - mean(wav_obj@right, na.rm = TRUE)
    }
    
    # Get the absolute maximum across all channels to maintain stereo balance
    max_val <- max(abs(c(wav_obj@left, if(wav_obj@stereo) wav_obj@right else NULL)))
    
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


# Feature Extraction

calc_features <- function(wav_input, window_ms = 30, overlap = 0.5) {
  # If stereo audio then convert to mono
  if(wav_input@stereo) wav_input <- mono(wav_input, which = "both")
  # Store just the signal for manipulation
  samples <- wav_input@left
  
  #Sample rate
  sr <- wav_input@samp.rate
  window_samps <- round((window_ms / 1000) * sr)
  hop_samps <- window_samps * (1 - overlap)
  # FFT length
  fft_len <- 2^ceiling(log2(window_samps))
  
  # Feature Caclulation
  # Pre-calculate MFCCs
  all_mels <- tuneR::melfcc(wav_input, numcep = 13, wintime = window_ms/1000, 
                            hoptime = hop_samps/sr, preemph = 0)
  mels_t <- t(all_mels)
  delta_mels <- tuneR::deltas(mels_t)
  delta_delta_mels <- t(tuneR::deltas(delta_mels))
  delta_mels <- t(delta_mels)
  
  # Pre-calc spectra
  zp_req <- (fft_len-window_samps) - ((fft_len-window_samps)%%2)
  res <- seewave::spectro(wav_input@left, f= sr, wl = window_samps, ovlp = overlap * 100, zp = zp_req, wn = "hanning",
                          plot = FALSE, norm = FALSE, dB = NULL)
  spec_matrix <- res$amp
  freqs_khz <- res$freq
  
  # Calculate number of windows and trim mfcc and spec to match
  #n_frames <- floor((length(samples) - window_samps) / hop_samps) + 1
  n_frames <- min(ncol(spec_matrix), nrow(all_mels))
  spec_matrix <- spec_matrix[, 1:n_frames]
  all_mels <- all_mels[ 1:n_frames, ]
  
  # Spectral Slope (Algebraic)
  # This is much faster than doing it inside the loop and/or via Linear Regression
  x_bar <- mean(freqs_khz, na.rm = TRUE)
  x_diff <- freqs_khz - x_bar
  slope_denom <- sum(x_diff^2)
  # colSums and matrix multiplication are significantly faster than apply(..., 2)
  #slopes <- colSums(x_diff * (spec_matrix - colMeans(spec_matrix))) / slope_denom
  slopes <- colSums(x_diff * (spec_matrix - matrixStats::colMeans2(spec_matrix))) / slope_denom
  
  # Spectral Flux
  spec_diffs <- spec_matrix[, 2:n_frames] - spec_matrix[, 1:(n_frames-1)]
  flux <- c(NA, sqrt(colSums(spec_diffs^2)))
  
  # Spectral Difference Variance (DFV)
  #dfv_vals <- c(0, apply(abs(spec_diffs), 2, var))
  dfv_vals <- c(NA, matrixStats::colVars(abs(spec_diffs)))
  
  # Loop for functions that cannot be vectorised (reduce overhead)
  starts <- round(seq(1, length(samples) - window_samps, length.out = n_frames))
  
  # Pre-allocate results for loop-based features
  time_feats <- matrix(NA_real_, nrow = n_frames, ncol = 6) # RMS, ZCR, F0_mean, F0_sd, Crest, TempEnt
  spec_props_list <- vector("list", n_frames)

  for (i in 1:n_frames) {
    
    chunk <- samples[starts[i]:(starts[i] + window_samps - 1)]
    
    # Time Domain
    rms_val <-  sqrt(mean(chunk^2, na.rm = TRUE))
    time_feats[i, 1] <- rms_val  # RMS
    time_feats[i, 2] <- sum(abs(diff(sign(chunk))) / 2) / (length(chunk) - 1) # ZCR
    
    # SpecProp (Requires a 2-col matrix input)
    spec_props_list[[i]] <- as.data.frame(seewave::specprop(cbind(freqs_khz, spec_matrix[, i]), f = sr))
    
    # F0 tracking
    f0_res <- tryCatch({
      seewave::fund(chunk, f = sr, wl = min(fft_len, 512), ovlp = 0, plot = FALSE)
    }, error = function(e) { NULL })
    
    if (!is.null(f0_res)) {
      valid_f0 <- f0_res[!is.na(f0_res[, 2]) & f0_res[, 2] > 0, 2]
      if (length(valid_f0) > 1) { # Requires at least 2 points for a valid SD
        time_feats[i, 3] <- mean(valid_f0, na.rm = TRUE) * 1000
        time_feats[i, 4] <- sd(valid_f0, na.rm = TRUE) * 1000
      } else if (length(valid_f0) == 1) {
        time_feats[i, 3] <- valid_f0 * 1000
        time_feats[i, 4] <- NA # We have a mean, but no measurable variance
      } else {
        time_feats[i, 3:4] <- NA # No pitch found
      }
    } else {
      time_feats[i, 3:4] <- NA # Error in fund()
    }
    
    peak_val <- max(abs(chunk))
    time_feats[i, 5] <- if(rms_val > 0) peak_val / rms_val else 0 # Crest factor
    
    if(rms_val > 0){
      env_chunk <- seewave::env(chunk, f = sr, plot=FALSE, norm = TRUE)
      time_feats[i, 6] <- seewave::sh(env_chunk) # Temporal Shannon Entropy
    } else {
      time_feats[i, 6] <- 0
    }
    
  }
  
  mfcc_df <- as.data.frame(all_mels[1:n_frames, ])
  mfcc_d1_df <- as.data.frame(delta_mels[1:n_frames, ])
  mfcc_d2_df <- as.data.frame(delta_delta_mels[1:n_frames, ])
  colnames(mfcc_df) <- paste0("mfcc", 1:ncol(mfcc_df))
  colnames(mfcc_d1_df) <- paste0("d1_mfcc", 1:ncol(mfcc_d1_df))
  colnames(mfcc_d2_df) <- paste0("d2_mfcc", 1:ncol(mfcc_d2_df))
  
  # Final Assembly
  final_df <- data.frame(
    window_index = 1:n_frames,
    start_time = (starts - 1) / sr,
    end_time = (starts - 1 + window_samps) / sr,
    rms_energy = time_feats[, 1],
    zcr = time_feats[, 2],
    spectral_slope = slopes,
    spectral_flux = flux,
    spec_dfv = dfv_vals,
    f0_mean = time_feats[, 3],
    f0_sd = time_feats[, 4],
    crest_factor = time_feats[, 5],
    temporal_entropy = time_feats[, 6]
  ) %>% 
    cbind(dplyr::bind_rows(spec_props_list), mfcc_df, mfcc_d1_df, mfcc_d2_df) %>%
    dplyr::select(-any_of("prec"))
  
  return(final_df)
  
}

extract_features <- function(audio_path, p_cfg = list()) {
  
  # 1. Pre-process (Noise Reduction)
  message(paste0("Processing: ", tools::file_path_sans_ext(audio_path)))
  # This returns a transient wave object or a temporary path to a cleaned file
  cleaned_wave <- preprocess_wav(audio_path,
                                 pre_emph_coeff = p_cfg$pre_emph_coeff,
                                 noise_reduction = p_cfg$noise_reduction,
                                 filter_type = p_cfg$filter_type,
                                 cutoff_highpass = p_cfg$cutoff_highpass,
                                 cutoff_lowpass = p_cfg$cutoff_lowpass,
                                 filter_order = p_cfg$filter_order,
                                 normalise = p_cfg$normalise
  )
  
  if (is.null(cleaned_wave)) stop("Pre-processing failed.")
  message("Processing Complete.")
  # 2. Feature Calculation
  # This runs the FFTs, MFCCs, and spectral math on the cleaned wave
  message("Calculating Features...")
  features_df <- calc_features(wav_input = cleaned_wave,
                               window_ms = p_cfg$win_len,
                               overlap = p_cfg$overlap
  )
  message("Feature Calculation Complete.")
  # 3. Metadata Tagging
  # We add the method options used here so it's baked into the dataframe
  features_df <- features_df %>%
    mutate(
      file_id      = tools::file_path_sans_ext(basename(audio_path)),
      proc_id      = p_cfg$label # Human-readable ID for the config
    )
  
  return(features_df)
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

# Helper to handle completion
check_completion <- function() {
  if (data_storage$current_run > length(data_storage$files_to_classify)) {
    shinyjs::hide("main_ui")
    shinyjs::hide("post_process_sidebar")
    shinyjs::show("completion_ui") # We will add this to ui.R
    showNotification("Session Complete! All candidates reviewed.", type = "message")
    return(TRUE)
  }
  return(FALSE)
}