ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f7f6; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; overflow-x: hidden; }
      .well { background-color: #fff; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border: 1px solid #ddd; }
      
      /* Classification Buttons - Touch Optimized */
      .btn-classify { margin-bottom: 8px; width: 100%; font-weight: bold; height: 50px; font-size: 16px; transition: transform 0.1s; }
      .btn-classify:active { transform: scale(0.98); }
      
      /* Progress Bar */
      #progress_container { width: 100%; height: 8px; background-color: #e9ecef; border-radius: 4px; margin-bottom: 15px; }
      #progress_bar { height: 100%; background: linear-gradient(90deg, #007bff, #0056b3); border-radius: 4px; transition: width 0.3s ease; width: 0%; }
      
      /* Landscape Warning Override */
      #landscape_warning { display: none; }
      @media screen and (max-width: 768px) and (orientation: portrait) {
        #app_workspace { display: none !important; }
        #landscape_warning { display: flex; position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background-color: #333; color: white; flex-direction: column; justify-content: center; align-items: center; z-index: 10000; text-align: center; padding: 20px; }
      }
      
      /* Playhead Styling */
      .viz-container { position: relative; width: 100%; margin-bottom: 10px; background: #fff; border: 1px solid #eee; border-radius: 4px; overflow: hidden; }
      .static-viz { width: 100%; display: block; object-fit: fill; }
      .playhead { position: absolute; top: 0; left: 0%; width: 2px; height: 100%; background-color: rgba(255, 0, 0, 0.8); z-index: 10; pointer-events: none; }
      
      /* Heights for desktop alignment */
      @media (min-width: 768px) {
        .full-height-panel { height: 100%; display: flex; flex-direction: column; justify-content: space-between; }
        .spec-img { height: 300px; }
        .wave-img { height: 100px; }
      }
      @media (max-width: 767px) {
        .spec-img { height: 20vh; }
        .wave-img { height: 10vh; }
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
  div(id = "app_workspace", style = "display: none;", # Hidden until server unlocks it
      titlePanel("Squawk-Spot"),
      
      # 1. START SCREEN
      div(id = "pre_process_sidebar",
          wellPanel(style = "max-width: 900px; margin: 0 auto; text-align: center; padding: 30px;",
                    h2("Squawk-Spot Training", style="color: #007bff; font-weight: bold;"),
                    p("Study the examples below before beginning."),
                    
                    fluidRow(
                      column(6, wellPanel(
                        h4("🔊 Squawk"),
                        tags$audio(src = file.path(DEMO_BASE_URL, "squawk_demo.wav"), type = "audio/wav", controls = TRUE, style = "width: 100%;"),
                        tags$img(src = file.path(IMAGE_BASE_URL, "squawk_demo_spec.png"), class = "static-viz spec-img", style="margin-top: 10px;")
                      )),
                      column(6, wellPanel(
                        h4("🔊 Alarm"),
                        tags$audio(src = file.path(DEMO_BASE_URL, "alarm_demo.wav"), type = "audio/wav", controls = TRUE, style = "width: 100%;"),
                        tags$img(src = file.path(IMAGE_BASE_URL, "alarm_demo_spec.png"), class = "static-viz spec-img", style="margin-top: 10px;")
                      ))
                    ),
                    br(),
                    actionButton("start_btn", "Start Classification", class = "btn-primary btn-lg", style="width: 60%;")
          )
      ),
      
      # 2. MAIN CLASSIFICATION UI
      div(id = "main_ui", style = "display: none;",
          div(id = "progress_container", div(id = "progress_bar")),
          
          fluidRow(
            # Visuals Column (Appears FIRST on mobile)
            column(width = 7,
                   wellPanel(class = "full-height-panel",
                             uiOutput("audio_player_ui"),
                             
                             # Spectrogram with Playhead
                             div(class = "viz-container",
                                 uiOutput("spec_img_ui"),
                                 div(id = "spec_playhead", class = "playhead")
                             ),
                             
                             # Waveform with Playhead
                             div(class = "viz-container",
                                 uiOutput("wave_img_ui"),
                                 div(id = "wave_playhead", class = "playhead")
                             )
                   )
            ),
            
            # Buttons Column (Appears SECOND on mobile)
            column(width = 5,
                   wellPanel(class = "full-height-panel",
                             h4(textOutput("file_info"), style = "text-align: center; font-weight: bold; color: #555;"),
                             hr(),
                             actionButton("btn_squawk", "Squawk", class = "btn-success btn-classify"),
                             actionButton("btn_alarm", "Alarm", class = "btn-info btn-classify"),
                             actionButton("btn_other", "Other Vocalisation", class = "btn-warning btn-classify"),
                             actionButton("btn_noise", "Noise / Background", class = "btn-danger btn-classify"),
                             actionButton("btn_unknown", "Unknown", class = "btn-secondary btn-classify"),
                             hr(),
                             fluidRow(
                               column(6, actionButton("btn_prev", "← Previous", class = "btn-outline-secondary btn-classify")),
                               column(6, actionButton("btn_skip", "Skip →", class = "btn-light btn-classify"))
                             ),
                             hr(),
                             checkboxInput("show_highlight", "Show Expert Highlight", value = FALSE)
                   )
            )
          )
      ),
      
      # 3. COMPLETION UI
      div(id = "completion_ui", style = "display: none;",
          wellPanel(style = "max-width: 600px; margin: 10vh auto; text-align: center; padding: 40px;",
                    uiOutput("final_score_display"),
                    hr(),
                    actionButton("restart_app", "Try Again", class = "btn-primary btn-lg", icon = icon("sync"))
          )
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
