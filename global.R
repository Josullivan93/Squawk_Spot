# global.R
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load( shiny, shinyjs, here, plotly)

# Load your helper functions (Logic only, no heavy data yet)
source("helper.R")

# Temp directory & resource path
temp_dir <- here("Output", "tmp")
output_dir <- here("Output")
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
addResourcePath("temp_audio", temp_dir)