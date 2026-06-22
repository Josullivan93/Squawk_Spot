ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    body { 
      background-color: #f4f7f6; 
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; 
      overflow-x: hidden;
      margin: 0;
      padding: 0;
    }
    
    /* Slim header */
    .navbar-default {
      background-color: #fff;
      border-bottom: 1px solid #ddd;
      margin-bottom: 8px;
      min-height: 40px;
      padding: 4px 15px;
    }
    
    .navbar-brand {
      display: flex;
      align-items: center;
      gap: 8px;
      font-size: 16px;
      font-weight: bold;
      color: #333;
      padding: 0;
      height: 40px;
      line-height: 40px;
    }
    
    .navbar-brand img {
      height: 30px;
      width: auto;
      object-fit: contain;
    }
    
    .well { 
      background-color: #fff; 
      border-radius: 8px; 
      box-shadow: 0 4px 6px rgba(0,0,0,0.1); 
      border: 1px solid #ddd;
      padding: 10px;
      margin-bottom: 10px;
    }
    
    /* Title */
    h2 { font-size: 20px; margin: 10px 0; }
    h4 { font-size: 14px; margin: 8px 0; }
    
    /* Classification Buttons */
    .btn-classify { 
      margin-bottom: 6px; 
      width: 100%; 
      font-weight: bold; 
      font-size: 13px;
      padding: 8px 6px;
      transition: transform 0.1s; 
    }
    .btn-classify:active { transform: scale(0.98); }
    
    /* Horizontal button layout for mobile */
    .button-row {
      display: flex;
      gap: 4px;
      margin-bottom: 6px;
    }
    .button-row .btn {
      flex: 1;
      min-width: 0;
      font-size: 11px;
      padding: 6px 4px;
      margin-bottom: 0;
    }
    
    /* Progress Bar */
    #progress_container { 
      width: 100%; 
      height: 6px; 
      background-color: #e9ecef; 
      border-radius: 4px; 
      margin-bottom: 10px;
    }
    #progress_bar { 
      height: 100%; 
      background: linear-gradient(90deg, #007bff, #0056b3); 
      border-radius: 4px; 
      transition: width 0.3s ease; 
      width: 0%; 
    }
    
    /* Landscape Warning */
    #landscape_warning { display: none; }
    @media screen and (max-width: 768px) and (orientation: portrait) {
      #app_workspace { display: none !important; }
      #landscape_warning { 
        display: flex; 
        position: fixed; 
        top: 0; left: 0; 
        width: 100vw; height: 100vh; 
        background-color: #333; 
        color: white; 
        flex-direction: column; 
        justify-content: center; 
        align-items: center; 
        z-index: 10000; 
        text-align: center; 
        padding: 20px; 
      }
    }
    
    /* Visualization containers */
    .viz-container { 
      position: relative; 
      width: 100%; 
      margin-bottom: 8px; 
      background: #fff; 
      border: 1px solid #eee; 
      border-radius: 4px; 
      overflow: hidden; 
    }
    .static-viz { 
      width: 100%; 
      display: block; 
      object-fit: fill; 
    }
    
    audio { 
      width: 100%;
      height: 30px;
      margin-bottom: 8px;
    }
    
    #file_info { 
      font-size: 12px;
      font-weight: bold;
      text-align: center;
      margin: 5px 0;
      color: #555;
    }
    
    hr { margin: 8px 0; }
    
    .checkbox { 
      margin: 8px 0; 
      font-size: 12px;
    }
    
    /* Desktop: Buttons LEFT, Figures RIGHT */
    @media (min-width: 1024px) {
      .buttons-panel { 
        order: -1;
        padding-right: 15px;
      }
      .figures-panel {
        padding-left: 15px;
      }
      .spec-img { height: 300px; }
      .wave-img { height: 100px; }
      .btn-classify { font-size: 14px; padding: 10px 8px; }
    }
    
    /* Tablet: Buttons LEFT, Figures RIGHT, scaled down */
    @media (max-width: 1023px) and (min-width: 769px) {
      .buttons-panel { 
        order: -1;
        padding-right: 10px;
      }
      .figures-panel {
        padding-left: 10px;
      }
      .spec-img { height: 220px; }
      .wave-img { height: 70px; }
      .btn-classify { font-size: 12px; padding: 8px 6px; }
    }
    
    /* Phone landscape: Buttons on left, figures on right, scaled way down */
    @media (max-width: 768px) {
      .buttons-panel { 
        order: -1;
        padding-right: 8px;
        min-width: 280px;
      }
      .figures-panel {
        padding-left: 8px;
        flex: 1;
      }
      .spec-img { height: 120px; }
      .wave-img { height: 50px; }
      .btn-classify { font-size: 11px; padding: 6px 4px; margin-bottom: 4px; }
      
      /* Hide secondary buttons on very narrow screens */
      #btn_prev { display: none; }
      .checkbox { display: none; }
    }
    
    /* Very narrow: Stack buttons horizontally below figures */
    @media (max-width: 600px) {
      .buttons-panel { 
        order: 1;
        padding-right: 0;
        padding-left: 0;
        padding-top: 8px;
        min-width: auto;
      }
      .figures-panel {
        padding-left: 0;
        flex: 1;
      }
      
      /* Main classification buttons in horizontal row */
      .primary-buttons {
        display: flex;
        gap: 4px;
        flex-wrap: wrap;
      }
      .primary-buttons .btn {
        flex: 1;
        min-width: 60px;
        font-size: 10px;
        padding: 5px 3px;
        margin-bottom: 0;
      }
      
      .spec-img { height: 100px; }
      .wave-img { height: 40px; }
    }
  "))
  ),
  
  # Portrait Lock Screen
  div(id = "landscape_warning",
      icon("mobile-alt", class = "fa-4x", style = "transform: rotate(90deg); margin-bottom: 20px;"),
      h3("Please rotate your device"),
      p("Squawk-Spot requires a landscape view to display the bioacoustic data correctly.")
  ),
  
  # Main App Container
  div(id = "app_workspace", style = "display: none;",
      # Custom slim header with logo
      div(style = "background-color: #fff; border-bottom: 1px solid #ddd; padding: 6px 15px; margin-bottom: 8px; display: flex; align-items: center; gap: 10px; height: 40px;",
          tags$img(src = "logo.png", style = "height: 30px; width: auto; object-fit: contain;"),
          h3("Squawk-Spot", style = "margin: 0; font-size: 16px; font-weight: bold; color: #333;")
      ),
      
      # 1. START SCREEN
      div(id = "pre_process_sidebar",
          wellPanel(style = "max-width: 900px; margin: 0 auto; text-align: center; padding: 20px;",
                    h2("Squawk-Spot Training"),
                    p("Study the examples below before beginning."),
                    
                    fluidRow(
                      column(6, wellPanel(
                        h4("🔊 Squawk"),
                        tags$audio(src = file.path(DEMO_BASE_URL, "squawk_demo.wav"), type = "audio/wav", controls = TRUE, style = "width: 100%;"),
                        tags$img(src = file.path(IMAGE_BASE_URL, "squawk_demo_spec.png"), class = "static-viz spec-img", style = "margin-top: 10px; max-height: 200px;")
                      )),
                      column(6, wellPanel(
                        h4("🔊 Alarm"),
                        tags$audio(src = file.path(DEMO_BASE_URL, "alarm_demo.wav"), type = "audio/wav", controls = TRUE, style = "width: 100%;"),
                        tags$img(src = file.path(IMAGE_BASE_URL, "alarm_demo_spec.png"), class = "static-viz spec-img", style = "margin-top: 10px; max-height: 200px;")
                      ))
                    ),
                    br(),
                    actionButton("start_btn", "Start Classification", class = "btn-primary btn-lg", style = "width: 60%;")
          )
      ),
      
      # 2. MAIN CLASSIFICATION UI
      div(id = "main_ui", style = "display: none;",
          div(id = "progress_container", div(id = "progress_bar")),
          
          fluidRow(style = "display: flex; gap: 0;",
                   # BUTTONS COLUMN (LEFT)
                   column(width = 3,
                          wellPanel(class = "buttons-panel",
                                    h4(textOutput("file_info")),
                                    hr(),
                                    
                                    # Primary classification buttons
                                    div(class = "primary-buttons",
                                        actionButton("btn_squawk", "Squawk", class = "btn-success btn-classify"),
                                        actionButton("btn_alarm", "Alarm", class = "btn-info btn-classify"),
                                        actionButton("btn_other", "Other", class = "btn-warning btn-classify"),
                                        actionButton("btn_noise", "Noise", class = "btn-danger btn-classify"),
                                        actionButton("btn_unknown", "Unknown", class = "btn-secondary btn-classify")
                                    ),
                                    
                                    hr(),
                                    
                                    # Secondary buttons
                                    fluidRow(
                                      column(6, actionButton("btn_prev", "Prev", class = "btn-outline-secondary btn-classify")),
                                      column(6, actionButton("btn_skip", "Skip", class = "btn-light btn-classify"))
                                    ),
                                    
                                    hr(),
                                    
                                    # Checkbox
                                    checkboxInput("show_highlight", "Expert Highlight", value = TRUE)
                          )
                   ),
                   
                   # FIGURES COLUMN (RIGHT)
                   column(width = 9,
                          wellPanel(class = "figures-panel",
                                    # Audio player
                                    uiOutput("audio_player_ui"),
                                    
                                    # Spectrogram
                                    div(class = "viz-container",
                                        uiOutput("spec_img_ui")
                                    ),
                                    
                                    # Waveform
                                    div(class = "viz-container",
                                        uiOutput("wave_img_ui")
                                    )
                          )
                   )
          )
      ),
      
      # 3. COMPLETION UI
      div(id = "completion_ui", style = "display: none;",
          wellPanel(style = "max-width: 700px; margin: 5vh auto; text-align: center; padding: 40px; background: #fff;",
                    h2("Quiz Complete!", style = "color: #333;"),
                    uiOutput("final_score_display"),
                    hr(),
                    actionButton("restart_app", "Try Again", class = "btn-primary btn-lg", icon = icon("sync"))
          )
      ),
      
      div(id = "loading_overlay", style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.7); z-index: 9999; display: flex; flex-direction: column; justify-content: center; align-items: center;",
          tags$div(style = "width: 100px; height: 100px; border-radius: 50%; background-color: transparent; display: flex; justify-content: center; align-items: center; margin-bottom: 20px; overflow: hidden;",
                   tags$img(src = "loader.gif", style = "width: 100%; height: 100%; border-radius: 50%; object-fit: cover;")
          ),
          h3("Loading...", style = "color: white; margin: 0;")
      ),
      
      # Score submission overlay - appears only at end
      div(id = "score_overlay", style = "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.7); z-index: 9999; display: flex; flex-direction: column; justify-content: center; align-items: center;",
          tags$div(style = "width: 100px; height: 100px; border-radius: 50%; background-color: transparent; display: flex; justify-content: center; align-items: center; margin-bottom: 20px; overflow: hidden;",
                   tags$img(src = "loader.gif", style = "width: 100%; height: 100%; border-radius: 50%; object-fit: cover;")
          ),
          h3("Submitting your score...", style = "color: white; margin: 0;")
      )
  ),
  
  tags$script(HTML("
  document.addEventListener('keydown', function(e) {
    if (e.key === '1') Shiny.setInputValue('hotkey_1', Math.random(), {priority: 'event'});
    if (e.key === '2') Shiny.setInputValue('hotkey_2', Math.random(), {priority: 'event'});
    if (e.key === '3') Shiny.setInputValue('hotkey_3', Math.random(), {priority: 'event'});
    if (e.key === '4') Shiny.setInputValue('hotkey_4', Math.random(), {priority: 'event'});
    if (e.key === '5') Shiny.setInputValue('hotkey_5', Math.random(), {priority: 'event'});
  });
"))
)