# UI Definition
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(here, shiny, shinyjs, plotly)

ui <- fluidPage(
  useShinyjs(), # Required for toggling UI sections
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f7f6; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .well { background-color: #fff; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .btn-classify { margin-bottom: 10px; width: 100%; font-weight: bold; }
      #progress_container { background-color: #eee; border-radius: 13px; padding: 3px; margin-bottom: 20px; }
      #progress_bar { background-color: #007bff; width: 0%; height: 20px; border-radius: 10px; transition: width 0.4s ease; }
    "))
  ),
  
  titlePanel("SquawkSpot: Automated Vocalization Classifier"),
  
  sidebarLayout(
    sidebarPanel(
      # SECTION 1: Pre-Processing (Visible at start)
      div(id = "pre_process_sidebar",
          h4("Step 1: Upload Audio"),
          fileInput("upload_file", "Choose WAV File", accept = c(".wav")),
          actionButton("process_btn", "Process & Detect", class = "btn-primary", width = "100%")
      ),
      
      # SECTION 2: Annotation (Hidden until processing is done)
      shinyjs::hidden(
        div(id = "post_process_sidebar",
            h4("Step 2: Classify Detections"),
            
            # Progress bar
            div(id = "progress_container", div(id = "progress_bar")),
            textOutput("file_info"),
            hr(),
            
            # Classification Buttons
            actionButton("btn_squawk", "1: Squawk (Positive)", class = "btn-success btn-classify"),
            actionButton("btn_other", "2: Other Vocalisation", class = "btn-info btn-classify"),
            actionButton("btn_noise", "3: Noise / Background", class = "btn-warning btn-classify"),
            actionButton("btn_unknown", "4: Unknown / Unsure", class = "btn-secondary btn-classify"),
            
            hr(),
            h4("Navigation"),
            splitLayout(
              actionButton("btn_prev", "Previous"),
              actionButton("btn_next", "Skip/Next")
            ),
            br(),
            p(tags$small("Hotkeys: 1, 2, 3, 4 for classification"))
        )
      )
    ),
    
    mainPanel(
      # Placeholder visible at startup
      div(id = "main_placeholder",
          wellPanel(
            h3("Welcome"),
            p("Upload a field recording on the left. The system will automatically use the trained models to find candidate vocalizations for your review.")
          )
      ),
      
      # Main UI hidden until processing completes
      shinyjs::hidden(
        div(id = "main_ui",
            wellPanel(
              uiOutput("audio_player"),
              hr(),
              plotlyOutput("waveform_plot", height = "250px"),
              plotlyOutput("spectrogram_plot", height = "350px")
            ),
            
            # HOTKEY SCRIPT PLACED HERE
            # This script only initializes when this 'div' becomes visible
            tags$script(HTML("
          $(document).on('keydown', function(e) {
            // 1. Only run if we aren't typing in a text box
            if (e.target.tagName !== 'INPUT' && e.target.tagName !== 'TEXTAREA') {
              
              var inputId = '';
              if(e.which == 49) inputId = 'hotkey_1'; // Key 1
              if(e.which == 50) inputId = 'hotkey_2'; // Key 2
              if(e.which == 51) inputId = 'hotkey_3'; // Key 3
              if(e.which == 52) inputId = 'hotkey_4'; // Key 4
              
              if(inputId !== '') {
                // We send a timestamp/random value to ensure Shiny 
                // registers it as a 'change' every time the key is pressed
                Shiny.setInputValue(inputId, Math.random(), {priority: 'event'});
              }
            }
          });
        "))
        )
      ),
      
      shinyjs::hidden(
        div(id = "completion_ui",
            wellPanel(
              style = "text-align: center; padding: 50px;",
              h2("Session Complete!", style = "color: #28a745; border: none;"),
              p("You have reviewed all detected candidates for this file."),
              hr(),
              actionButton("restart_app", "Process New File", class = "btn-primary btn-lg")
            )
        )
      )
      
    )
  )
)