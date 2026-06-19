# UI Definition
ui <- fluidPage(
  shinyjs::useShinyjs(), # Required for toggling UI sections
  
  tags$head(
    tags$style(HTML("
    body { background-color: #f4f7f6; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
    .well { background-color: #fff; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border: 1px solid #ddd; }
    .btn-classify { margin-bottom: 10px; width: 100%; font-weight: bold; height: 50px; font-size: 16px; }
    .btn-classify:hover { filter: brightness(90%); transform: translateY(-1px); }
    #file_info { font-weight: bold; color: #555; font-size: 1.2em; text-align: center; margin-bottom: 15px; }
    #audio_element { width: 100%; margin-top: 10px; }
    
    /* Loader CSS */
    .circular-loader-container { display: flex; justify-content: center; align-items: center; width: 100%; margin: 20px 0; }
    .loader-circle { width: 150px; height: 150px; overflow: hidden; border-radius: 50%; border: 3px solid #007bff; box-shadow: 0 4px 10px rgba(0,0,0,0.1); display: flex; justify-content: center; align-items: center; }
    .loader-circle img { height: 100%; width: 100%; object-fit: cover; }
    
    /* Progress Bar CSS - ADD THIS */
    #progress_container {
      width: 100%;
      height: 12px;
      background-color: #e9ecef;
      border-radius: 6px;
      overflow: hidden;
      margin-bottom: 20px;
      box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);
    }
    #progress_bar {
      height: 100%;
      background: linear-gradient(90deg, #007bff, #0056b3);
      border-radius: 6px;
      transition: width 0.3s ease;
      width: 0%;
    }
    
    /* Hide divs completely from document flow */
    #pre_process_sidebar.shiny-hidden,
    #main_ui.shiny-hidden,
    #completion_ui.shiny-hidden {
      display: none !important;
    }
    
   /* Demo audio/spectrogram styling */
  .demo-card {
    background-color: #f9f9f9;
    border-radius: 8px;
    padding: 20px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .demo-card audio {
    width: 100%;
    margin-bottom: 15px;
    border-radius: 4px;
  }
  
  .demo-card img {
    width: 100%;
    border-radius: 6px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .demo-card h4 {
    margin-top: 0;
    margin-bottom: 15px;
    font-weight: bold;
  }
    
    @media (max-width: 768px) {
      .demo-card {
        margin-bottom: 20px;
      }
      .col-sm-8 { padding-left: 0; padding-right: 0; }
      .col-sm-4 { padding-left: 0; padding-right: 0; }
    }
  "))
  ),
  
  titlePanel("Squawk-Spot"),
  
  # ---------------------------------------------------------
  # 0. THE LOADING OVERLAY (Hidden by default)
  # ---------------------------------------------------------
  shinyjs::hidden(
    div(
      id = "loading_overlay",
      style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
               background: rgba(255, 255, 255, 0.9); z-index: 9999; 
               display: flex; flex-direction: column; justify-content: center; align-items: center;",
      div(class = "circular-loader-container",
          div(class = "loader-circle",
              tags$img(src = "loader.gif") # Must be in www/ root
          )
      ),
      h3("Processing...", style = "color: #333; text-align: center; margin-top: 20px;")
    )
  ),
  
  div(id = "app_workspace",
      
      # ---------------------------------------------------------
      # 1. THE START SCREEN 
      # ---------------------------------------------------------
      div(
        id = "pre_process_sidebar",
        fluidRow(
          column(width = 12, align = "center", style = "margin-top: 5vh;",
                 wellPanel(style = "max-width: 900px; padding: 40px;",
                           h2("Squawk-Spot Classification Demo", style="color: #007bff; font-weight: bold;"),
                           p("Can you accurately classify these vocalizations?", style = "font-size: 1.1em; margin-bottom: 30px;"),
                           
                           # Demo examples
                           fluidRow(
                             column(width = 6,
                                    wellPanel(style = "background-color: #f9f9f9;",
                                              h4("🔊 Squawk", style = "color: #28a745;"),
                                              tags$audio(
                                                src = "demo/squawk_demo.wav",
                                                type = "audio/wav",
                                                controls = TRUE,
                                                style = "width: 100%; margin-bottom: 15px;"
                                              ),
                                              plotlyOutput("demo_squawk_spec", height = "300px"),
                                              p("Short, sharp vocalizations", style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                                    )
                             ),
                             column(width = 6,
                                    wellPanel(style = "background-color: #f9f9f9;",
                                              h4("🔊 Alarm", style = "color: #007bff;"),
                                              tags$audio(
                                                src = "demo/alarm_demo.wav",
                                                type = "audio/wav",
                                                controls = TRUE,
                                                style = "width: 100%; margin-bottom: 15px;"
                                              ),
                                              plotlyOutput("demo_alarm_spec", height = "300px"),
                                              p("Rapid, repetitive calls", style = "font-size: 0.9em; color: #666; margin-top: 10px;")
                                    )
                             )
                           ),
                           
                           hr(),
                           p("Listen to the examples above and study the spectrograms to learn the differences.", 
                             style = "font-size: 1em; color: #555; margin-bottom: 20px;"),
                           br(),
                           actionButton("start_btn", "Start Quiz", class = "btn-primary btn-lg", 
                                        style="width: 60%; padding: 15px;")
                 )
          )
        )
      ),
      
      # ---------------------------------------------------------
      # 2. THE MAIN UI (Hidden until Start is clicked)
      # ---------------------------------------------------------
      shinyjs::hidden(
        div(
          id = "main_ui",
          div(id = "progress_container",
              div(id = "progress_bar")
          ),
          sidebarLayout(
            sidebarPanel(
              textOutput("file_info"), # Used to display "Clip 1 of 5"
              hr(),
              
              # Classification Buttons (Ensure labels map to your CSV true_class column)
              actionButton("btn_squawk", "Squawk", class = "btn-success btn-classify"),
              actionButton("btn_alarm", "Alarm", class = "btn-info btn-classify"),
              actionButton("btn_other", "Other Vocalisation", class = "btn-warning btn-classify"),
              actionButton("btn_noise", "Noise / Background", class = "btn-danger btn-classify"),
              actionButton("btn_unknown", "Unknown", class = "btn-secondary btn-classify"),
              hr(),
              
              # Add these missing buttons:
              actionButton("btn_skip", "Skip", class = "btn-light btn-classify"),
              actionButton("btn_prev", "← Previous", class = "btn-outline-secondary btn-classify"),
              
              hr(),
              
              checkboxInput("show_highlight", "Show Squawk-Spot Highlight", value = TRUE),
              hr()
            ),
            
            mainPanel(
              wellPanel(
                uiOutput("audio_player"),
                hr(),
                plotlyOutput("waveform_plot", height = "250px"),
                plotlyOutput("spectrogram_plot", height = "350px")
              )
            )
          )
        )
      ),
      
      # ---------------------------------------------------------
      # 3. THE COMPLETION UI (Hidden until final clip is classified)
      # ---------------------------------------------------------
      shinyjs::hidden(
        div(
          id = "completion_ui",
          fluidRow(
            column(
              width = 12, align = "center", style = "margin-top: 10vh;",
              wellPanel(style = "max-width: 600px; padding: 40px;",
                        uiOutput("final_score_display"), # Dynamically rendered from server.R
                        hr(),
                        actionButton("restart_app", "Try Again", class = "btn-primary btn-lg", 
                                     icon = icon("sync"))
              )
            )
          )
        )
      )
  )
)
