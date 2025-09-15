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
  final_features$user_class <- "Unclassified"
  
  ###### Add logic for automatic classification ######
  for( i in 1:nrow(final_features)){
    
    final_features$auto_class[i] <- ifelse(final_features$rms[i] > 28.32544 & final_features$centroid[i] > 1635.022, "Squawk", "Non-Squawk")
    
  }
  
  return(final_features)
}

#' Groups consecutive windows of a specific class and slices them into new files.
#'
#' This function filters the feature data for a specified "positive class",
#' identifies consecutive sequences of windows, and slices the original WAV file
#' to create new, longer audio chunks with a user-defined buffer.
#'
#' @param features_df The data frame containing features for all windows.
#' @param full_wave The original wave object.
#' @param positive_class The classification to filter for.
#' @param buffer_time The time in seconds to add as a buffer.
#' @param temp_dir The temporary directory to save the sliced chunks.
#' @param resume Logical, if TRUE, it will only slice and return unclassified chunks.
#' @return A list containing the file paths of the new chunks and the updated features data frame.
group_and_slice_chunks <- function(features_df, full_wave, positive_class, buffer_time, negative_percentage = 0.05, temp_dir) {
  
  message("Grouping consecutive windows and slicing...")
  
  # Clear existing files in tmp folder unless resuming
  file.remove(list.files(temp_dir, full.names = TRUE, recursive = TRUE))
  
  save(full_wave, file = file.path(temp_dir, "full_wave.RData"))
  
  # Identify chunks of the positive class
  positive_chunks <- features_df %>%
    dplyr::filter(auto_class == positive_class) %>%
    arrange(window_index) %>%
    group_by(is_consecutive = c(0, cumsum(diff(window_index) != 1))) %>%
    summarise(
      start_index = first(window_index),
      end_index = last(window_index),
      start_time = first(start_time),
      end_time = last(end_time),
      chunk_id = cur_group_id(),
      .groups = "drop" # Ensure correct grouping behavior
    ) %>%
    ungroup()
  
  # Identify negative windows and randomly sample a percentage
  negative_windows <- features_df %>%
    dplyr::filter(auto_class != positive_class)
  
  sample_size <- floor(nrow(negative_windows) * negative_percentage)
  
  if (sample_size > 0) {
    sampled_negative_windows <- negative_windows %>%
      sample_n(sample_size) %>%
      arrange(window_index)
    
    # Create single-window chunks for each negative sample
    max_chunk_id <- max(positive_chunks$chunk_id, 0)
    
    negative_chunks <- sampled_negative_windows %>%
      mutate(
        start_index = window_index,
        end_index = window_index,
        chunk_id = row_number() + max_chunk_id
      ) %>%
      select(start_index, end_index, start_time, end_time, chunk_id)
    
    # Combine positive and negative chunks
    all_chunks <- bind_rows(positive_chunks, negative_chunks) %>%
      arrange(start_time)
  } else {
    all_chunks <- positive_chunks
  }
  
  # Create new chunk_id column in features_df and assign based on all_chunks
  features_df$chunk_id <- NA
  for (i in 1:nrow(all_chunks)) {
    start <- all_chunks$start_index[i]
    end <- all_chunks$end_index[i]
    features_df$chunk_id[features_df$window_index >= start & features_df$window_index <= end] <- all_chunks$chunk_id[i]
  }
  
  # Add a filepath column for the chunks
  all_chunks$chunk_filepath <- NA
  
  # Slice and save the new audio files with a buffer
  file_paths <- lapply(1:nrow(all_chunks), function(i) {
    chunk <- all_chunks[i, ]
    
    # Calculate the buffered start and end times
    # Note: centerpoint is already handled by start_time and end_time
    # The buffer just adds time before the start and after the end.
    buffered_start <- max(0, chunk$start_time - buffer_time)
    buffered_end <- min(round(length(full_wave@left)/full_wave@samp.rate,2), chunk$end_time + buffer_time)
    # Slice the wave object
    sliced_wave <- cutw(full_wave, from = buffered_start, to = buffered_end, plot = FALSE, output = "Wave")
    
    #sliced_wave <- cutw(full_wave, from = buffered_start, to = buffered_end, plot = FALSE, output = "Wave")
    # Normalize the data before writing
    sliced_wave <- normalize(sliced_wave, unit = "16")
    # Alternative slice as cutw causing errors
    #sliced_wave <- Wave(full_wave@left[buffered_start:buffered_end], samp.rate = full_wave@samp.rate, bit = full_wave@bit)
    
    # Save the sliced wave file
    file_name <- paste0("chunk_", chunk$chunk_id, "_", format(buffered_start, nsmall = 2), ".wav")
    file_path <- file.path(temp_dir, file_name)
    writeWave(sliced_wave, file_path)
    
    # Update the filepath column in the chunks data frame
    all_chunks$chunk_filepath[i] <<- file_path
    
    return(file_path)
  })
  
  # Update the original features_df with chunk_filepaths
  features_df <- features_df %>%
    left_join(all_chunks %>% select(chunk_id, chunk_filepath), by = "chunk_id") %>%
    mutate(chunk_filepath = replace_na(chunk_filepath, "None"))
  
  # Save the features data.table for resuming later
  data.table::fwrite(features_df, file.path(temp_dir, "features.csv"))
  
  # Return the list of file paths and the updated features data frame
  return(list(file_paths = file_paths, updated_features_df = features_df))
}


#' Processes a WAV file, applies selected filters, and slices it into chunks.
#'
#' This function acts as the core audio processing engine. It reads a full WAV file,
#' applies optional filtering as specified by the user's selections in the UI,
#' generates a CSV of features and timestamps, and saves sliced audio files
#' into a temporary directory.
#'
#' @param wav_path Path to the input WAV file.
#' @param stationary_filter Logical, TRUE to apply stationary noise reduction.
#' @param nonstationary_filter Logical, TRUE to apply non-stationary noise reduction.
#' @param low_pass_hz Numeric, the low-pass frequency in Hz. Can be NULL.
#' @param high_pass_hz Numeric, the high-pass frequency in Hz. Can be NULL.
#' @return A data frame containing the generated features and timestamps.

process_wav <- function(wav_path, stationary_filter, nonstationary_filter, low_pass_hz, high_pass_hz, window_size, window_overlap){
  
  message("Processing Audio...")

  # Read in wav file
  wav_obj <- readWave(wav_path)
  
  proc_wav <- wav_obj
  
  sample_rate <- proc_wav@samp.rate
  nyquist <- sample_rate/2
  is_stereo <- proc_wav@stereo
  signal_left <- as.numeric(proc_wav@left)
  
  if (is_stereo){
    signal_right <- as.numeric(proc_wav@right)
  }
  
  # Apply Noise Reduction
  if (stationary_filter) {
    message("Applying Stationary Noise Reduction...")
    
    py_left <- r_to_py(signal_left)
    py_rate <- r_to_py(sample_rate)
    
    nr <- import("noisereduce")
    new_signal <- nr$reduce_noise(y = py_left, sr = py_rate, stationary = TRUE)
    signal_left <- as.numeric(py_to_r(new_signal))
    rm(new_signal)
    
    if(is_stereo){
      py_right <- r_to_py(signal_right)
      new_signal <- nr$reduce_noise(y = py_right, sr = py_rate, stationary = TRUE)
      signal_right <- as.numeric(py_to_r(new_signal))
      rm(new_signal)    
    }
    
  }else if (nonstationary_filter) {
    
    message("Applying Non-Stationary Noise Reduction...")
    
    py_left <- r_to_py(signal_left)
    py_rate <- r_to_py(sample_rate)
    
    nr <- import("noisereduce")
    new_signal <- nr$reduce_noise(y = py_left, sr = py_rate, stationary = FALSE)
    signal_left <- as.numeric(py_to_r(new_signal))
    rm(new_signal)
    
    if(is_stereo){
      py_right <- r_to_py(signal_right)
      new_signal <- nr$reduce_noise(y = py_right, sr = py_rate, stationary = FALSE)
      signal_right <- as.numeric(py_to_r(new_signal))
      rm(new_signal)    
    }
    
  }
  
  if (is_stereo) {
    proc_wav <- tuneR::Wave(left = signal_left, right = signal_right, samp.rate = sample_rate, bit = proc_wav@bit)
  } else {
    proc_wav <- tuneR::Wave(left = signal_left, samp.rate = sample_rate, bit = proc_wav@bit)
  }
  
  # 2. Apply frequency filters
  if ((!is.na(low_pass_hz) | !is.na(high_pass_hz))) {
    message("Applying Frequency Filtering...")
    # The ffilter() function handles low-pass, high-pass, and band-pass automatically
    # based on which arguments are provided.
    from_hz <- high_pass_hz
    to_hz <- low_pass_hz
    if (!is.na(from_hz) & !is.na(to_hz) && from_hz > to_hz) {
      temp_hz <- from_hz
      from_hz <- to_hz
      to_hz <- temp_hz
    }
    
    proc_wav <- ffilter(
      proc_wav, 
      from = from_hz, 
      to = to_hz
    )
  }
  
  #Calculate Features
  features_df <- calc_features(proc_wav, window_size, window_overlap)
  
  message("Audio Processing and Feature Calculation Complete.")
  return(list(proc_wav = proc_wav, features_df =features_df))
}

#' Classifies the current audio snippet, moves the file, and updates the CSV.
#' @param classification The classification string (e.g., "Squawk").
#' @param chunk_path The path to the chunk file.
#' @param features_df The updated features data frame from reactive state.
classify_and_move <- function(classification, chunk_path, features_df) {
  # Create the destination folder if it doesn't exist
  dest_dir <- here("Output", classification)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
  
  # Move the chunk file from the temp directory to the classified directory
  file.rename(here(chunk_path), file.path(dest_dir, basename(chunk_path[[1]])))
  
  # Update the classification for all the windows in the original features file
  chunk_id <- features_df %>% dplyr::filter(chunk_filepath == chunk_path) %>% pull(chunk_id) %>% .[!is.na(.)] %>% unique()
  
  if (length(chunk_id) > 0) {
    all_features <- fread(here("Output","tmp", "features.csv"))
    
    # Update the user_class for all rows with the matching chunk ID
    all_features[which(all_features$chunk_id == chunk_id), user_class := classification]
    
    # Save the updated master CSV
    data.table::fwrite(all_features, here("Output","tmp", "features.csv"))
    
    # Append the rows to the classification-specific CSV
    dest_csv_path <- file.path(dest_dir, paste0(classification, ".csv"))
    
    # Get the rows corresponding to the current chunk
    # Now using the user_class column for the filepath in the CSV
    rows_to_add <- all_features %>%
      dplyr::filter(chunk_id == chunk_id) %>%
      mutate(filepath = file.path(dest_dir, basename(chunk_path[[1]])))
    
    # Append to the CSV
    data.table::fwrite(rows_to_add, dest_csv_path, append = TRUE, col.names = !file.exists(dest_csv_path))
  }
}
