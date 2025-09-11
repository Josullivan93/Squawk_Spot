# --- UI Definition ---

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(here, shiny, shinyjs)

fluidPage(
  useShinyjs(), # Initialize shinyjs for keyboard shortcuts
  
  tags$head(
    # CSS for a clean, responsive layout and button styling
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f0f4f8;
        color: #1a202c;
      }
      .container-fluid {
        padding: 20px;
      }
      .well {
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        padding: 20px;
        border: none;
      }
      h1 {
        color: #2c5282;
        font-weight: 700;
        text-align: center;
      }
      h3 {
        color: #4a5568;
        margin-top: 0;
      }
      .btn {
        width: 100%;
        margin-top: 10px;
        border-radius: 8px;
        font-weight: 700;
        text-transform: uppercase;
        transition: background-color 0.3s, transform 0.2s;
        border: none;
        padding: 12px 20px;
      }
      .btn-primary {
        background-color: #4299e1;
      }
      .btn-primary:hover {
        background-color: #3182ce;
        transform: translateY(-2px);
      }
      #squawk_btn, #other_btn, #unknown_btn, #noise_btn {
        background-color: #f6ad55;
        color: #fff;
      }
      #squawk_btn:hover, #other_btn:hover, #unknown_btn:hover, #noise_btn:hover {
        background-color: #ed8936;
        transform: translateY(-2px);
      }
      .plot-container {
        border: 2px solid #e2e8f0;
        border-radius: 12px;
        padding: 10px;
        background-color: #ffffff;
        margin-bottom: 20px;
      }
      .plot-caption {
        text-align: center;
        margin-top: 10px;
        font-style: italic;
        color: #718096;
      }
    ")),
    # JavaScript for keyboard shortcuts
    tags$script(HTML("
      $(document).on('keydown', function(event) {
        // Check for either Ctrl (Windows/Linux) or Cmd (macOS)
        if (event.ctrlKey || event.metaKey) {
            // Check for Cmd/Ctrl + S for 'Squawk'
            if (event.key === 's') {
              $('#squawk_btn').click();
              return false; // Prevent browser default
            }
            // Check for Cmd/Ctrl + O for 'Other Vocalisation'
            if (event.key === 'o') {
              $('#other_btn').click();
              return false;
            }
            // Check for Cmd/Ctrl + U for 'Unknown'
            if (event.key === 'u') {
              $('#unknown_btn').click();
              return false;
            }
            // Check for Cmd/Ctrl + N for 'Noise'
            if (event.key === 'n') {
              $('#noise_btn').click();
              return false;
            }
        }
      });
    "))
  ),
  
  titlePanel(
    h1("Audio Classification App", class = "mb-4")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "well",
      h3("1. Load Audio"),
      p("Select a WAV file to process, or resume from a previous session."),
      fileInput("file_upload", "Choose WAV File",
                accept = c("audio/wav", ".wav")
      ),
      actionButton("resume_btn", "Resume from temp folder", class = "btn-primary"),
      hr(),
      h3("2. Audio Processing Options"),
      p("Select one or more filters to apply to the audio file before slicing."),
      checkboxInput("stationary_filter", "Apply Stationary Noise Reduction", value = FALSE),
      checkboxInput("nonstationary_filter", "Apply Non-stationary Noise Reduction", value = FALSE),
      numericInput("low_pass_hz", "Low-pass Filter (Hz)", value = NULL, min = 0),
      numericInput("high_pass_hz", "High-pass Filter (Hz)", value = NULL, min = 0),
      p(em("Note: If both Low-pass and High-pass are enabled, a Band-pass filter is applied.")),
      hr(),
      h3("3. Feature Extraction Options"),
      p("Define the window size and overlap for feature calculation."),
      numericInput("window_size", "Window Size (seconds)", value = 1, min = 0.1, step = 0.1),
      sliderInput("window_overlap", "Window Overlap (%)", min = 0, max = 99, value = 50),
      hr(),
      actionButton("process_btn", "Process and Slice File", class = "btn-primary"),
      hr(),
      h3("4. Classify (Press Ctrl/Cmd + Key)"),
      disabled(actionButton("squawk_btn", "Squawk (Ctrl/Cmd + S)")),
      disabled(actionButton("other_btn", "Other Vocalisation (Ctrl/Cmd + O)")),
      disabled(actionButton("unknown_btn", "Unknown (Ctrl/Cmd + U)")),
      disabled(actionButton("noise_btn", "Noise (Ctrl/Cmd + N)")),
      hr(),
      p(em(strong("Current Status:"))),
      verbatimTextOutput("status_text")
    ),
    
    mainPanel(
      class = "well",
      h3(textOutput("current_file_display")),
      uiOutput("audio_player"),
      hr(),
      fluidRow(
        column(6,
               div(class = "plot-container",
                   plotOutput("waveform_plot")
               ),
               p("Waveform of the audio snippet.", class = "plot-caption")
        ),
        column(6,
               div(class = "plot-container",
                   plotOutput("spectrogram_plot")
               ),
               p("Spectrogram of the audio snippet.", class = "plot-caption")
        )
      )
    )
  )
)
