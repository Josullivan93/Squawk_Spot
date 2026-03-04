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
      #completion_ui { margin-top: 100px; }
      #audio_element { width: 100%; margin-top: 10px; }
      .btn-classify:hover { filter: brightness(90%); transform: translateY(-1px); }
      .well { border: 1px solid #ddd; }
      #file_info { font-weight: bold; color: #555; font-size: 1.1em; }
    "))
  ),
  titlePanel("SquawkSpot: Automated Vocalization Classifier"),
  div(
    id = "loading_overlay",
    style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%;
             background: rgba(255, 255, 255, 0.8); z-index: 9999; text-align: center; padding-top: 200px;",
    tags$img(
      src = "loader.gif",
      class = "rounded-circle", height = "150px"
    ),
    h3("Analyzing Audio...", style = "color: #333; margin-top: 20px;")
  ),
  div(
    id = "app_workspace",
    sidebarLayout(
      sidebarPanel(
        # SECTION 1: Pre-Processing (Visible at start)
        div(
          id = "pre_process_sidebar",
          h4("Step 1: Upload Audio"),
          fileInput("upload_file", "Choose WAV File",
            accept = c(".wav"), multiple =
              TRUE
          ),
          actionButton("process_btn", "Process & Classify", class = "btn-primary", icon = icon("play"))
        ),

        # SECTION 2: Annotation (Hidden until processing is done)
        shinyjs::hidden(
          div(
            id = "post_process_sidebar",
            h4("Step 2: Classify Detections"),

            # Progress bar
            div(id = "progress_container", div(id = "progress_bar")),
            textOutput("file_info"),
            hr(),

            # Classification Buttons
            actionButton("btn_squawk", "1: Squawk", class = "btn-success btn-classify"),
            actionButton("btn_alarm", "2: Alarm Call", class = "btn-info btn-classify"),
            actionButton("btn_other", "3: Other Vocalisation", class = "btn-warning btn-classify"),
            actionButton("btn_noise", "4: Noise / Background", class = "btn-danger btn-classify"),
            actionButton("btn_unknown", "5: Unknown / Unsure", class = "btn-secondary btn-classify"),
            
            hr(),
            h4("Navigation"),
            splitLayout(
              actionButton("btn_prev", "Previous"),
              actionButton("btn_next", "Skip/Next")
            ),
            br(),
            p(tags$small("Hotkeys: 1, 2, 3, 4, or 5 for classification")),
            h4("Plotting Options"),

            # Toggle highlight off
            checkboxInput("show_highlight", "Show/Hide Squawk Highlight", value = TRUE),
            br(),
            hr(),
            h4("System Audit"),
            verbatimTextOutput("debug_state")
          )
        )
      ),
      mainPanel(
        # Placeholder visible at startup
        div(
          id = "main_placeholder",
          wellPanel(
            h3("Welcome"),
            p("Upload a field recording on the left. The system will automatically use the trained models to find candidate vocalizations for your review.")
          )
        ),

        # Main UI hidden until processing completes
        shinyjs::hidden(
          div(
            id = "main_ui",
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
              if(e.which == 53) inputId = 'hotkey_5'; // Key 5

              if(inputId !== '') {
                // We send a timestamp/random value to ensure Shiny
                // registers it as a 'change' every time the key is pressed
                Shiny.setInputValue(inputId, Math.random(), {priority: 'event'});
              }
            }
          });
        "))
          )
        )
      )
    )
  ),
  shinyjs::hidden(
    div(
      id = "completion_ui",
      fluidRow(
        column(
          width = 8, offset = 2,
          wellPanel(
            style = "text-align: center; padding: 50px;",
            h2("Session Complete!", style = "color: #28a745;"),
            p("You have reviewed all detected candidates for this file."),
            hr(),
            actionButton("restart_app", "Process New File", class = "btn-primary btn-lg")
          )
        )
      )
    )
  )
)
