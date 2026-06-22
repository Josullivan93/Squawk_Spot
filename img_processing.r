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
  # 1. WAVEFORM PLOT (Mimicking Plotly Scatter)
  # ==========================================
  duration <- length(wav@left) / wav@samp.rate
  time_vector <- seq(0, duration, length.out = length(wav@left))
  
  y_range_wave <- range(wav@left, na.rm = TRUE)
  if (any(!is.finite(y_range_wave))) y_range_wave <- c(-1, 1)
  
  wave_df <- data.frame(Time = time_vector, Amplitude = wav@left)
  
  p_wave <- ggplot(wave_df, aes(x = Time, y = Amplitude)) +
    geom_line(color = "#1f77b4", linewidth = 0.15, alpha = 0.7) + 
    # Force Y-axis labels to be exactly 5 characters wide (e.g., " -1.0", "  0.5")
    scale_y_continuous(labels = function(x) sprintf("%5.1f", x)) +
    coord_cartesian(xlim = c(0, duration), ylim = y_range_wave, expand = FALSE) +
    theme_minimal() +
    labs(x = "Time (s)", y = "Amplitude") +
    theme(
      # Use a monospaced font for the Y-axis text to ensure identical pixel width
      axis.text.y = element_text(family = "mono", size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      panel.grid.major = element_line(color = "#ebebeb"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # Lock resolution and dimensions
  ggsave(file.path(output_dir, paste0(filename, "_wave.png")), 
         plot = p_wave, width = 8, height = 3.5, dpi = 300)
  
  
  # ==========================================
  # 2. SPECTROGRAM PLOT (Perfect Sync Version)
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
    # Force Y-axis labels to be exactly 5 characters wide (e.g., " 10.0", "  5.0")
    scale_y_continuous(labels = function(x) sprintf("%5.1f", x)) +
    coord_cartesian(xlim = c(0, duration), ylim = c(0, 8), expand = FALSE) + 
    theme_minimal() +
    labs(x = "Time (s)", y = "Frequency") + # Trimmed to match the width of "Amplitude"
    theme(
      # Match the monospaced font and sizing perfectly to the waveform
      axis.text.y = element_text(family = "mono", size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "none", 
      panel.grid = element_blank(), 
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # Lock exact same resolution and dimensions as the waveform
  ggsave(file.path(output_dir, paste0(filename, "_spec.png")), 
         plot = p_spec, width = 8, height = 3.5, dpi = 300)
  
  cat("Processed:", filename, "\n")
}

cat("All visuals generated successfully!\n")
