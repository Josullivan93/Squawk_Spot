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
reticulate::py_config()


# Helper Functions

#' Calculates Power Spectral Density (PSD).
#'
#' This function takes audio data from a single window and calculates its
#' Power Spectral Density using a Hanning window.
#'
#' @param win_dat Numeric vector of audio data for a single window.
#' @param samp_rate Numeric, the sample rate of the audio.
#' @param fft_len Numeric, the length of the FFT to use.
#' @return A data frame with PSD data or NULL if computation fails.
psd_calc <- function(win_dat,
                     samp_rate,
                     fft_len = 2^ceiling(log2(length(win_dat)))
){
  # Frequency Domain - Calculate Power Spectral Density
  # Using tryCatch for spectral analysis as it can fail on weird inputs
  spec_out <- tryCatch({
    # Use norm=FALSE as some seewave functions expect non-normalized power
    seewave::spec(win_dat, f = samp_rate, wl = fft_len, wn = "hanning",
                  PSD = TRUE, norm = FALSE, dB = NULL, plot = FALSE)
  }, error = function(e) {
    warning("Could not compute spectrum: ", e$message)
    return(NULL)
  })
  return(spec_out)
}

#' Placeholder for the flux calculation function.
#'
#' @param psd_list A list containing the PSD objects for the previous and current window.
#' @return A numeric value representing the spectral flux.
flux_calc <- function(psd_pair) {
  
  spec_curr <- psd_pair$curr
  spec_prev <- psd_pair$prev
  
  # Basic checks
  if (is.null(spec_curr) || is.null(spec_prev) ||
      !is.matrix(spec_curr) || !is.matrix(spec_prev) ||
      nrow(spec_curr) != nrow(spec_prev)) {
    # Return NA if spectra are invalid or don't match dimensions
    return(NA_real_)
  }
  # Calculate Euclidean distance between power vectors (spec[,2])
  flux <- sqrt(sum((spec_curr[, 2] - spec_prev[, 2])^2, na.rm = TRUE))
  return(flux)
  
}

#' Processes a single audio window and calculates its features.
#'
#' This function is called by `wav_min_feat` to process individual windows of audio
#' data and calculate various features like RMS and spectral properties.
#'
#' @param index Numeric, the index of the current window.
#' @param step_size Numeric, the step size between windows in samples.
#' @param wav_data Numeric vector of the full audio data.
#' @param win_len Numeric, the window length in samples.
#' @param samp_rate Numeric, the sample rate of the audio.
#' @param fft_len Numeric, the length of the FFT.
#' @return A list of features for the window, or NULL if an error occurs.
process_single_window <- function(index, step_size, wav_data, win_len, samp_rate, fft_len) {
  
  # 1. Calculate the start sample for this index
  start_sample <- ((index - 1) * step_size) + 1
  end_sample <- start_sample + win_len - 1
  
  # Check for edge case where the last window might be out of bounds
  if (end_sample > length(wav_data)) {
    return(NULL) # This window can't be processed, return nothing
  }
  
  window_audio <- wav_data[start_sample:end_sample]
  
  # Do same for previous window
  prev_start_sample <- ((index - 2) * step_size) + 1
  prev_end_sample <- prev_start_sample + win_len - 1
  
  prev_window_audio <- wav_data[prev_start_sample:prev_end_sample]
  
  rms_val <- NA
  spec_centroid_val <- NA
  spec_entropy_val <- NA
  spec_skew_val <- NA
  spec_kurt_val <- NA
  flux_val <- NA
  
  tryCatch({
    
    # 2. Calculate RMS for the window
    rms_val <- sqrt(mean(window_audio^2, na.rm = TRUE))
    
    # 3. Calculate PSD and then the spectral centroid
    spec_centroid_val <- NA # Default value in case of error
    psd_obj  <- NULL
    
    psd_obj <- psd_calc(window_audio, samp_rate = samp_rate, fft_len = fft_len)
    
    spec_props <- tryCatch({
      seewave::specprop(spec = psd_obj, f = samp_rate, plot = FALSE)
    }, error = function(e) {
      warning("Could not compute spectral properties: ", e$message)
      # If specprop fails, we return NULL and handle it later
      return(NULL)
    })
    
    if (!is.null(spec_props)) {
      spec_centroid_val <- spec_props$cent
      spec_entropy_val <- spec_props$sh
      spec_skew_val <- spec_props$skewness
      spec_kurt_val <- spec_props$kurtosis
    }
    
    
    # 4. Calculate PSD for previous window & Flux between previous and current
    psd_prev <- psd_calc(prev_window_audio, samp_rate = samp_rate, fft_len = fft_len)
    
    if (!is.null(psd_obj) && !is.null(psd_prev)) {
      flux_val <- flux_calc(list(prev = psd_prev, curr = psd_obj))
    }
    
  }, error = function(e) {
    warning(paste("Window index", index, ": Could not compute features:", e$message))
  })
  
  
  
  # 5. Return features for this window as a single, small list
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

#' Calculates features and timestamps for audio processing.
#'
#' This function takes a processed wave object and calculates timestamps
#' and generates a data frame of features based on a specified window size and overlap.
#' This function replaces the original `calculate_features` with your custom
#' feature extraction logic.
#'
#' @param processed_wave The wave object after any filtering has been applied.
#' @param window_size Numeric, the size of each audio window in seconds.
#' @param window_overlap Numeric, the percentage of overlap between consecutive windows.
#' @return A data frame containing the generated features and timestamps.
calc_features <- function(proc_wav, window_size, window_overlap){
  
  message('Calculating Features...')
  
  # Set parallel plan
  plan(multisession, workers = round(parallel::detectCores()-2))
  
  samp_rate <- proc_wav@samp.rate
  wav_data <- proc_wav@left # Just processes left channel at this time
  
  # Timestamp based segmentation
  #duration <- proc_wav@duration
  #step_size <- window_size * (1 - window_overlap/100)
  #timestamps <- seq(from = 0, to = duration - window_size, by = step_size)
  
  # Sample number based segmentation
  win_len <- round(window_size * samp_rate)
  step_size <- floor(win_len * (1 - window_overlap/100))
  num_windows <- floor((length(wav_data) - win_len) / step_size) + 1
  
  # Standardised FFT length
  standard_fft_len <- 2^ceiling(log2(win_len))
  
  message(paste("Processing", num_windows, "windows in parallel..."))
  
  # Calculate features in parallel:
  results_list <- future.apply::future_lapply(
    X = 2:num_windows,
    FUN = process_single_window,
    future.chunk.size = 1000,
    # Pass necessary arguments to the helper function
    step_size = step_size,
    wav_data = wav_data,
    win_len = win_len,
    samp_rate = samp_rate,
    fft_len = standard_fft_len,
    # Explicitly list packages needed on the workers
    future.packages = c("seewave")
  )
  
  message("Combining features and finalising...")
  
  # Calculate feature (except flux) for first window
  first_window <- wav_data[1:win_len]
  
  first_cent <- NA # Default value in case of error
  first_entropy <- NA
  first_skew <- NA
  first_kurt <- NA
  first_psd  <- NULL
  
  first_psd <- psd_calc(first_window , samp_rate = samp_rate, fft_len = standard_fft_len)
  
  first_spec <- tryCatch({
    seewave::specprop(spec = first_psd, f = samp_rate, plot = FALSE)
  }, error = function(e) {
    warning("Could not compute spectral properties: ", e$message)
    # If specprop fails, we return NULL and handle it later
    return(NULL)
  })
  
  if (!is.null(first_spec)) {
    first_cent <- first_spec$cent
    first_entropy <- first_spec$sh
    first_skew <- first_spec$skewness
    first_kurt <- first_spec$kurtosis
  }
  
  features_1 <- list(
    rms = sqrt(mean(first_window^2, na.rm = TRUE)),
    centroid = first_cent,
    entropy = first_entropy,
    skewness = first_skew,
    kurtosis = first_kurt,
    flux = NA,
    window_index = 1,
    start_time = 0,
    end_time = ( win_len - 1 ) / samp_rate
  )
  
  # Combine window 1 results with all other window results
  # Extract features and remove null
  valid_results <- results_list[!sapply(results_list, is.null)]
  
  # Prepend features_1_list to the list of other results
  # Note: features_1_list must be wrapped in list() to become element 1 of all_results_list
  all_results_list <- c(list(features_1), valid_results)
  
  final_features <- rbindlist(all_results_list, fill = TRUE)
  
  # Ensure order if needed (though rbind should preserve it)
  setorder(final_features, window_index)
  
  # Add the classification column needed by the app
  final_features$auto_class <- "Unclassified"
  
  ###### Add logic for automatic classification ######
  for( i in 1:nrow(final_features)){
    
    final_features$auto_class[i] <- ifelse(final_features$rms[i] > 28.32544 & final_features$centroid[i] > 1635.022, "Squawk", "Non-Squawk")
    
  }
  
  return(final_features)
}

# ---------------------------------------------------------
# Group features into clips and export temp wavs
# ---------------------------------------------------------

group_and_slice_chunks <- function(features_df, full_wave, positive_class,
                                   buffer_time = 1.0, temp_dir,
                                   target_length = 3.0) {
  
  # Ensure temp dir exists
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  
  # Require auto_class column
  if (!"auto_class" %in% names(features_df)) {
    stop("features_df must contain a column named 'auto_class'")
  }
  
  # Normalize auto_class to character
  features_df[, auto_class := as.character(auto_class)]
  
  # Add logical flag for positive class
  features_df[, is_positive := auto_class == positive_class]
  
  # Order by window index
  setorder(features_df, window_index)
  
  # Identify runs based strictly on consecutive positives
  features_df[, run_boundary := (get("is_positive") & !shift(get("is_positive"), fill = FALSE))]
  features_df[, run_id := cumsum(run_boundary)]
  features_df[!get("is_positive"), run_id := NA_integer_]
  
  # Collect unique run IDs
  valid_runs <- na.omit(unique(features_df$run_id))
  
  clip_paths <- c()
  run_metadata <- list()
  
  # Process each run
  for (rid in valid_runs) {
    run_windows <- features_df[run_id == rid]
    
    # Run start/end times (based only on actual positive windows)
    start_time <- min(run_windows$start_time)
    end_time   <- max(run_windows$end_time)
    
    # Extend to target_length if needed
    if ((end_time - start_time) < target_length) {
      end_time <- start_time + target_length
    }
    
    # Add buffer outside of run bounds (only affects slice, not run detection)
    slice_start <- max(0, start_time - buffer_time)
    slice_end   <- min(end_time + buffer_time, length(full_wave@left) / full_wave@samp.rate)
    
    # Slice audio
    clip <- cutw(full_wave, from = slice_start, to = slice_end, output = "Wave")
    out_path <- file.path(temp_dir, paste0("run_", rid, ".wav"))
    writeWave(clip, out_path)
    
    clip_paths <- c(clip_paths, out_path)
    
    # Store metadata
    run_metadata[[as.character(rid)]] <- list(
      run_id = rid,
      windows = run_windows$window_index,
      filepath = out_path
    )
    
    # Update features_df with clip path
    features_df[run_id == rid, clip_path := out_path]
  }
  
  # Save updated features and full_wave
  fwrite(features_df, file.path(temp_dir, "features.csv"))
  save(full_wave, file = file.path(temp_dir, "full_wave.RData"))
  
  return(list(
    updated_features_df = features_df,
    file_paths = clip_paths,
    run_metadata = run_metadata
  ))
}

# ---------------------------------------------------------
# Classify and move (run_id-level)
# ---------------------------------------------------------
classify_and_move <- function(label, run_id, features_df, output_dir) {
  stopifnot("run_id" %in% names(features_df))
  stopifnot("filepath" %in% names(features_df))
  
  # Subset rows for this run
  run_rows <- features_df[run_id == !!run_id]
  if (nrow(run_rows) == 0) stop(paste("No rows found for run_id =", run_id))
  
  # Assign label to auto_class column
  features_df[run_id == !!run_id, auto_class := label]
  
  # Determine source file
  src_file <- unique(run_rows$filepath)
  if (length(src_file) != 1) stop(paste("Expected one filepath for run_id =", run_id,
                                        "but found", length(src_file)))
  
  # Destination directory
  dest_dir <- file.path(output_dir, label)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  # Copy clip to destination
  dest_file <- file.path(dest_dir, basename(src_file))
  file.copy(src_file, dest_file, overwrite = TRUE)
  
  # Persist updated features.csv
  fwrite(features_df, file.path(output_dir, "features.csv"))
  
  return(features_df)
}