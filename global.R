# global.R
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(
  # Web & UI
  shiny, shinyjs, plotly,
  # Data Manipulation & Paths
  dplyr, tidyr, lubridate, data.table, here, zoo, matrixStats,
  # Audio / Signal Processing
  tuneR, seewave, signal,
  # Modeling & Performance
  ranger, future, future.apply, reticulate,
  # Utilities
  zip
)

# Load your helper functions (Logic only, no heavy data yet)
source("helper.R")

# Temp directory & resource path
temp_dir <- here("Output", "tmp")
output_dir <- here("Output")
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
addResourcePath("temp_audio", temp_dir)