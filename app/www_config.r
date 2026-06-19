# www_config.R
# Check if running in shinylive (WebAssembly) vs local
is_shinylive <- Sys.getenv("SHINYLIVE") == "true" || 
  exists("window") # Rough check for browser environment

if (is_shinylive) {
  # Point to raw GitHub URLs
  AUDIO_BASE_URL <- "https://raw.githubusercontent.com/Josullivan93/Squawk_Spot/fd04437c2857d0ab279c6c14cc862fdcd33edffd/www/audio/"
  
  DEMO_BASE_URL <- "https://raw.githubusercontent.com/Josullivan93/Squawk_Spot/fd04437c2857d0ab279c6c14cc862fdcd33edffd/www/demo/"
  
  ALL_AUDIO_FILES <- c(
    "clip_01.wav", "clip_02.wav", "clip_03.wav", "clip_04.wav", "clip_05.wav",
    "clip_06.wav", "clip_07.wav", "clip_08.wav", "clip_09.wav", "clip_10.wav"
    # Add all your audio files here
  )
  
} else {
  # Local development
  AUDIO_BASE_URL <- "audio"
  DEMO_BASE_URL <- "demo"
  ALL_AUDIO_FILES <- list.files("www/audio", pattern = "\\.wav$", full.names = FALSE)
}

get_audio_url <- function(filename) {
  paste0(AUDIO_BASE_URL, "/", filename)
}

get_demo_url <- function(filename) {
  paste0(DEMO_BASE_URL, "/", filename)
}