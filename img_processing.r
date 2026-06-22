# --- RUN LOCALLY ON YOUR DESKTOP ---
library(tuneR)
library(seewave)
library(ggplot2)
library(viridis)

# 1. Trim Demo Audio (Change paths to match your local setup)
squawk_full <- readWave("app/www/demo/squawk_demo.wav")
alarm_full <- readWave("app/www/demo/alarm_demo.wav")

# Extract exactly 5 seconds
squawk_demo <- extractWave(squawk_full, from = 0, to = 2, xunit = "time")
alarm_demo <- extractWave(alarm_full, from = 0, to = 2, xunit = "time")

writeWave(squawk_demo, "app/www/demo/squawk_demo_clip.wav")
writeWave(alarm_demo, "app/www/demo/alarm_demo_clip.wav")

# 2. Batch Generate PNGs for ALL audio (Demos + Quiz Files)
audio_dir <- "app/www/audio"
demo_dir <- "app/www/demo"
output_dir <- "app/www/images"
if(!dir.exists(output_dir)) dir.create(output_dir)

files_to_process <- list.files(audio_dir, pattern = "\\.wav$", full.names = TRUE)
demo_files <- list.files(demo_dir, pattern = "\\.wav$", full.names = TRUE)
all_files <- c(files_to_process, demo_files)

for (file in all_files) {
  filename <- tools::file_path_sans_ext(basename(file))
  wav <- readWave(file)
  
  # ==========================================
  # 0. PREPROCESSING: DC Offset & Normalization
  # ==========================================
  # Remove DC offset to prevent 0Hz artifacts in the spectrogram
  wav@left <- wav@left - mean(wav@left, na.rm = TRUE)
  
  # Normalize amplitude to 90% of max to prevent clipping and standardize visuals
  max_val <- max(abs(wav@left))
  if (max_val > 0) {
    wav@left <- (wav@left / max_val) * 0.9
  }
  
  # ==========================================
  # 1. WAVEFORM PLOT (No axes, margins, or whitespace)
  # ==========================================
  duration <- length(wav@left) / wav@samp.rate
  time_vector <- seq(0, duration, length.out = length(wav@left))
  
  y_range_wave <- range(wav@left, na.rm = TRUE)
  if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)
  
  wave_df <- data.frame(Time = time_vector, Amplitude = wav@left)
  
  p_wave <- ggplot(wave_df, aes(x = Time, y = Amplitude)) +
    geom_line(color = "#1f77b4", linewidth = 0.15, alpha = 0.7) + 
    coord_cartesian(xlim = c(0, duration), ylim = y_range_wave, expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Lock resolution: 1200px width = full audio duration
  # Height can match your spec plot
  ggsave(file.path(output_dir, paste0(filename, "_wave.png")), 
         plot = p_wave, width = 12, height = 3, dpi = 100, bg = "white")
  
  
  # ==========================================
  # 2. SPECTROGRAM PLOT (No axes, margins, or whitespace)
  # ==========================================
  spec <- spectro(wav, f = wav@samp.rate, wl = 512, ovlp = 75, plot = FALSE)
  amp_db <- spec$amp
  
  finite_vals <- amp_db[is.finite(amp_db)]
  min_db <- if (length(finite_vals) > 0) min(finite_vals, na.rm = TRUE) else -100
  amp_db[!is.finite(amp_db)] <- min_db
  
  time_grid <- rep(spec$time, each = length(spec$freq))
  freq_grid <- rep(spec$freq, times = length(spec$time))
  amp_vec <- as.vector(amp_db)
  
  spec_df <- data.frame(Time = time_grid, Frequency = freq_grid, Amplitude = amp_vec)
  
  p_spec <- ggplot(spec_df, aes(x = Time, y = Frequency, fill = Amplitude)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "inferno") + 
    coord_cartesian(xlim = c(0, duration), ylim = c(0, 8), expand = FALSE) + 
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Lock exact same resolution and dimensions as the waveform
  # 1200px width = full audio duration, so playhead positioning is pixel-perfect
  ggsave(file.path(output_dir, paste0(filename, "_spec.png")), 
         plot = p_spec, width = 12, height = 3, dpi = 100, bg = "white")
  
  cat("Processed:", filename, "\n")
}

cat("All visuals generated successfully!\n")
