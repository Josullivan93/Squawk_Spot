# --- UI Definition ---

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(here, shiny, shinyjs, plotly)

# --- Define UI ---
ui <- fluidPage(
  
  # Use shinyjs for showing/hiding elements
  useShinyjs(),
  
  tags$head(
    # CSS for a clean, modern look
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f4f7f6;
        color: #333;
      }
      .well {
        background-color: #fff;
        border: 1px solid #e3e3e3;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border-radius: 8px;
        padding: 20px;
      }
      h2, h3 {
        color: #007bff;
        border-bottom: 2px solid #e3e3e3;
        padding-bottom: 10px;
        margin-top: 0;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      .main-panel {
        background-color: #fff;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .shiny-plot-output {
        border: 1px solid #ccc;
        border-radius: 4px;
        background-color: #f9f9f9;
      }
      .file-upload-section {
        margin-bottom: 20px;
      }
      .progress-bar-wrapper {
          width: 100%;
          background-color: #f0f0f0;
          height: 10px;
          border-radius: 5px;
          margin-top: 10px;
      }
      .progress-bar {
          height: 100%;
          background-color: #007bff;
          border-radius: 5px;
          transition: width 0.3s ease;
      }
      .button-row {
          display: flex;
          justify-content: center;
          gap: 10px;
          margin-top: 20px;
      }
    "))
  ),
  
  # A simple title for the app
  titlePanel(h2("SquawkSpot")),
  
  # Main layout with a sidebar and main panel
  sidebarLayout(
    
    # Sidebar for file upload and options
    sidebarPanel(
      
      div(id = "pre_process_sidebar",
        div(class = "file-upload-section",
            h3("1. Upload or Resume"),
            fileInput("upload_file", "Choose .wav File", accept = c(".wav")),
            actionButton("process_btn", "Process File", class = "btn-primary", style = "margin-top: 10px;"),
            actionButton("resume_btn", "Resume Session", class = "btn-primary", style = "margin-top: 10px;")
        ),
        
        h3("2. Audio Processing Options"),
        checkboxInput("stationary_filter", "Stationary Noise Reduction"),
        checkboxInput("nonstationary_filter", "Non-Stationary Noise Reduction"),
        numericInput("low_pass_hz", "Low-Pass Filter (Hz)", value = NULL),
        numericInput("high_pass_hz", "High-Pass Filter (Hz)", value = NULL),
        
        h3("3. Feature Calculation"),
        numericInput("window_size", "Window Size (seconds)", value = 0.3, min = 0.01, step = 0.01),
        sliderInput("window_overlap", "Window Overlap (%)", min = 0, max = 99, value = 50),
        
        hr()
      ),
      
      shinyjs::hidden(
        div(id = "post_process_sidebar",
          h3("4. Classify"),
          
          div(class = "button-row",
              actionButton("btn_squawk", "Squawk", class = "btn-primary"),
              actionButton("btn_other", "Other Vocalisation", class = "btn-primary"),
              actionButton("btn_unknown", "Unknown", class = "btn-primary"),
              actionButton("btn_noise", "Noise", class = "btn-primary")
          ),
          
          div(class = "button-row",
              actionButton("btn_prev", "Previous", class = "btn-default"),
              actionButton("btn_next", "Next", class = "btn-default")
          ),
      
          # Hidden element to capture hotkeys from JavaScript
          tags$div(
            id = "hotkey_inputs",
            tags$input(id = "hotkey_1", type = "hidden"),
            tags$input(id = "hotkey_2", type = "hidden"),
            tags$input(id = "hotkey_3", type = "hidden"),
            tags$input(id = "hotkey_4", type = "hidden")
          ),
          
          # JavaScript to listen for hotkeys
          tags$script(HTML("
            $(document).on('keydown', function(event) {
              if (event.ctrlKey || event.metaKey) {
                var inputId = null;
                if (event.which == 49) { // Shift+1
                  inputId = 'hotkey_1';
                } else if (event.which == 50) { // Shift+2
                  inputId = 'hotkey_2';
                } else if (event.which == 51) { // Shift+3
                  inputId = 'hotkey_3';
                } else if (event.which == 52) { // Shift+4
                  inputId = 'hotkey_4';
                }
                if (inputId) {
                  var input = $('#' + inputId);
                  var currentValue = parseInt(input.val() || 0);
                  input.val(currentValue + 1).trigger('change');
                  event.preventDefault(); // Prevent default browser action
                }
              }
            });
          "))
        )
      )
    ),
    
    # Placeholderr main panel
    div(id = "main_placeholder",
        class = "main-panel", # Use same style for consistency
        h3("Welcome to SquawkSpot"),
        p("Please upload a .wav file and click 'Process File' to begin the analysis.")
    ),
    
    # Main panel for displaying plots and controls
    shinyjs::hidden(
      mainPanel(
        
        id = "main_ui", # ID to show/hide this entire section
        
        h3("Extracted Audio for Review"),
        
        div(id = "audio_container",
            uiOutput("audio_player")
        ),
        
        div(class = "progress-bar-wrapper",
            div(id = "progress_bar", class = "progress-bar", style = "width: 0%;")
        ),
        
        textOutput("file_info"),
        
        hr(),
        
        # Use plotlyOutput instead of plotOutput
        plotlyOutput("waveform_plot"),
        
        hr(),
        
        plotlyOutput("spectrogram_plot"),
        
        hr(),
        
        # JavaScript to send current audio playback time to the server
        tags$script(HTML("
          var audio_player = document.getElementById('audio_element');
          if (audio_player) {
            audio_player.addEventListener('timeupdate', function() {
              Shiny.setInputValue('current_time', audio_player.currentTime);
            });
          }
        "))
      )
    )
  )
)
