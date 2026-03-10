# global.R
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load( shiny, shinyjs, here, plotly)

# Load your helper functions (Logic only, no heavy data yet)
source("helper.R")