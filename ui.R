# --- UI Definition ---

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(here, shiny, shinyjs)

fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Audio Classifier"),
  
  # Sidebar layout with a sidebar for controls and a main panel for outputs
  sidebarLayout(
    sidebarPanel(
      
      # 1. File Upload Section
      h3("1. Load Audio File"),
      fileInput(
        "upload_file",
        "Choose WAV File",
        accept = c(".wav")
      ),
      
      hr(),
      
      actionButton("resume_btn", "Resume Previous Session"),
      
      hr(),
      
      # 2. Audio Processing Options
      h3("2. Audio Processing Options"),
      
      # Noise reduction options
      checkboxInput("stationary_filter", "Stationary Noise Reduction", value = FALSE),
      checkboxInput("nonstationary_filter", "Non-Stationary Noise Reduction", value = FALSE),
      
      # Frequency filtering options
      numericInput("low_pass_hz", "Low-Pass Filter (Hz)", value = NULL),
      numericInput("high_pass_hz", "High-Pass Filter (Hz)", value = NULL),
      
      hr(),
      
      # Feature calculation options
      h3("3. Feature Calculation Options"),
      numericInput("window_size", "Window Size (s)", value = 0.5, min = 0.1),
      sliderInput("window_overlap", "Window Overlap (%)", min = 0, max = 99, value = 50),
      
      hr(),
      
      # Action Buttons
      actionButton("process_btn", "Process and Slice File"),
      actionButton("clear_btn", "Clear Session"),
      
      hr(),
      
      # 4. Classification Buttons
      h3("4. Classification"),
      p("Use buttons or keyboard shortcuts (Ctrl + 1-4 / Cmd + 1-4)"),
      
      fluidRow(
        column(6, actionButton("btn_squawk", "Squawk (1)", class = "btn-success")),
        column(6, actionButton("btn_other", "Other Vocalisation (2)", class = "btn-primary"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("btn_unknown", "Unknown (3)", class = "btn-warning")),
        column(6, actionButton("btn_noise", "Noise (4)", class = "btn-danger"))
      ),
      
      hr(),
      
      # 5. Navigation Buttons
      h3("5. Navigation"),
      actionButton("btn_prev", "Previous"),
      actionButton("btn_next", "Next"),
      
    ),
    
    # Main panel for outputs (hidden by default)
    mainPanel(
      div(
        id = "main_ui",
        h3(textOutput("file_info")),
        
        # Audio Player
        uiOutput("audio_player"),
        
        # Plots
        plotOutput("waveform_plot"),
        plotOutput("spectrogram_plot")
      )
    )
  )
)