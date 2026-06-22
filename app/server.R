# Define the WebR fetch function at the top of the script
submit_score_to_google <- function(score, total) {
  if (is.null(score) || is.null(total) || is.na(score) || is.na(total)) {
    warning("submit_score_to_google called with invalid inputs: score=", score, ", total=", total)
    return(invisible(NULL))
  }
  
  cat("Submitting to Google Sheets: score=", score, ", total=", total, "\n")
  
  google_url <- "https://script.google.com/macros/s/AKfycbwBG9vzd71SUztARhXiNeOqNyW2e_-Timt3mu7KIYI6HjxcEaNGUi_ijU5if8pl5bZY/exec" 
  payload <- jsonlite::toJSON(list(score = score, total = total), auto_unbox = TRUE)
  
  js_code <- sprintf("
    console.log('Starting submission with payload:', '%s');
    
    // Submit score with no-cors
    fetch('%s', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      mode: 'no-cors',
      body: '%s'
    })
    .then(() => {
      console.log('Score submitted successfully');
      // Wait a moment then signal success
      Shiny.setInputValue('score_submitted', true, {priority: 'event'});
    })
    .catch(error => {
      console.error('Submission error:', error);
      Shiny.setInputValue('score_submitted', false, {priority: 'event'});
    });
  ", payload, google_url, payload)
  
  shinyjs::runjs(js_code)
}

server <- function(input, output, session) {
  
  showNotification("Server is executing!", type = "message", duration = 10)
  
  startup_done <- FALSE
  submission_done <- FALSE
  
  # 1. Unlock the UI immediately
  observeEvent(TRUE, {
    if (!startup_done) {
      shinyjs::show("app_workspace") 
      shinyjs::hide("main_ui")
      shinyjs::hide("completion_ui")
      shinyjs::hide("loading_overlay")
      shinyjs::hide("score_overlay")
      shinyjs::show("pre_process_sidebar")
      
      # Finish the Asymptotic Progress Bar and fade out
      shinyjs::runjs("
        var bar = document.getElementById('splash-progress-bar');
        var status = document.getElementById('splash-status');
        var splash = document.getElementById('shinylive-splash');
        
        // Snap bar to 100% quickly
        if (bar) {
          bar.style.transition = 'width 0.2s ease-out';
          bar.style.width = '100%';
        }
        if (status) {
          status.innerText = 'Ready!';
        }
        
        // Wait 400ms for the 100% animation to finish, then fade out
        setTimeout(() => {
          if (splash) {
            splash.style.opacity = '0';
            setTimeout(() => splash.style.display = 'none', 500);
          }
        }, 400);
      ")
      
      showNotification("Engine Ready", type = "message", duration = 3)
      startup_done <<- TRUE
    }
  }, once = TRUE)
  
  data_storage <- reactiveValues(
    current_run = 0,
    files_to_classify = NULL,
    ground_truth = NULL,
    user_answers = character(),
    final_score = 0,
    google_response = NULL
  )
  
  classification_btns <- c("btn_squawk", "btn_alarm", "btn_other", "btn_noise", "btn_unknown", "btn_skip")
  
  # 2. Button Disabling Logic (Wait for audio to finish)
  observeEvent(data_storage$current_run, {
    for (btn in classification_btns) {
      shinyjs::disable(btn)
    }
    if (data_storage$current_run <= 1) {
      shinyjs::disable("btn_prev")
    } else {
      shinyjs::enable("btn_prev")
    }
    
    # Update progress bar
    total <- length(data_storage$files_to_classify)
    if (total > 0 && data_storage$current_run <= total) {
      pct <- ((data_storage$current_run - 1) / total) * 100
      shinyjs::runjs(paste0("document.getElementById('progress_bar').style.width = '", pct, "%';"))
    }
  })
  
  observeEvent(input$audio_finished, {
    for (btn in classification_btns) {
      shinyjs::enable(btn)
    }
    showNotification("Audio reviewed. Buttons enabled.", type = "message", duration = 2)
  })
  
  # 3. Start Quiz Logic
  observeEvent(input$start_btn, {
    shinyjs::show("loading_overlay")
    shinyjs::hide("pre_process_sidebar")
    
    # Try to read ground_truth.csv, if not found, create sample data
    master_truth <- tryCatch({
      read.csv("www/ground_truth.csv")
    }, error = function(e) {
      # Create sample ground truth data if file doesn't exist
      data.frame(
        filename = ALL_AUDIO_FILES,
        true_class = c("Squawk", "Alarm", "Squawk", "Other Vocalisation", "Noise", 
                       "Squawk", "Alarm", "Unknown", "Noise", "Alarm"),
        highlight_start = c(0.5, 1.0, 0.3, 0.8, 0.2, 0.6, 1.2, 0.4, 0.9, 1.5),
        highlight_end = c(2.0, 2.5, 1.8, 2.2, 1.5, 2.1, 2.8, 1.9, 2.3, 3.0),
        stringsAsFactors = FALSE
      )
    })
    
    if (!file.exists("ground_truth.csv")) {
      showNotification("Using sample ground truth data", type = "message", duration = 3)
    }
    
    quiz_filenames <- sample(ALL_AUDIO_FILES)
    quiz_files <- file.path(AUDIO_BASE_URL, paste0(quiz_filenames))
    
    quiz_truth <- master_truth[match(quiz_filenames, master_truth$filename), ]
    
    data_storage$files_to_classify <- quiz_files
    data_storage$ground_truth <- quiz_truth
    data_storage$user_answers <- character()
    data_storage$current_run <- 1
    
    shinyjs::show("main_ui")
    shinyjs::delay(500, shinyjs::hide("loading_overlay"))
  })
  
  # Helper to get the current file details
  current_file_details <- reactive({
    req(data_storage$current_run > 0)
    filename <- basename(data_storage$files_to_classify[data_storage$current_run])
    base <- tools::file_path_sans_ext(filename)
    truth <- data_storage$ground_truth[data_storage$ground_truth$filename == filename, ]
    list(filename = filename, base = base, truth = truth)
  })
  
  # 4. Render Static Visuals with CSS Highlight Overlay
  output$spec_img_ui <- renderUI({
    details <- current_file_details()
    img_url <- file.path(IMAGE_BASE_URL, paste0(details$base, "_spec.png"))
    dur <- if (!is.null(input$current_audio_duration)) as.numeric(input$current_audio_duration) else 5.0
    
    # Calculate highlight values
    highlight_data <- NULL
    if (isTRUE(input$show_highlight) && nrow(details$truth) > 0) {
      hl_start <- details$truth$highlight_start[1]  # Get first row value
      hl_end <- details$truth$highlight_end[1]
      if (!is.na(hl_start) && !is.na(hl_end)) {
        left_pct <- (hl_start / dur) * 100
        width_pct <- ((hl_end - hl_start) / dur) * 100
        highlight_data <- list(left = left_pct, width = width_pct)
      }
    }
    
    tagList(
      # Y-axis label (left side)
      tags$div(style = "display: flex; gap: 5px; align-items: stretch;",
               tags$div(style = "width: 40px; display: flex; align-items: center; justify-content: center; font-size: 11px; text-align: center; color: #666; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap;",
                        "Frequency (kHz)"
               ),
               tags$div(style = "flex: 1;",
                        # Main visualization container with border as axis
                        tags$div(style = "position: relative; width: 100%; display: inline-block; border-left: 1px solid #333; border-bottom: 1px solid #333;",
                                 tags$img(src = img_url, class = "static-viz spec-img", style = "width: 100%; display: block;"),
                                 
                                 tags$svg(id = "spec-svg-overlay",
                                          viewBox = "0 0 100 100",
                                          preserveAspectRatio = "none",
                                          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;",
                                          
                                          if (!is.null(highlight_data)) {
                                            tags$rect(x = highlight_data$left, y = 0, 
                                                      width = highlight_data$width, height = 100,
                                                      fill = "rgba(0, 123, 255, 0.2)")
                                          },
                                          
                                          tags$line(id = "spec-playhead-line", x1 = 0, y1 = 0, x2 = 0, y2 = 100,
                                                    stroke = "red", `stroke-width` = "0.3", opacity = "0.8")
                                 )
                        )
               )
      ),
      
      # X-axis label (bottom)
      tags$div(style = "text-align: center; font-size: 11px; color: #666; margin-top: 2px;",
               "Time (s)"
      )
    )
  })
  
  output$wave_img_ui <- renderUI({
    details <- current_file_details()
    img_url <- file.path(IMAGE_BASE_URL, paste0(details$base, "_wave.png"))
    dur <- if (!is.null(input$current_audio_duration)) as.numeric(input$current_audio_duration) else 5.0
    
    # Calculate highlight values
    highlight_data <- NULL
    if (isTRUE(input$show_highlight) && nrow(details$truth) > 0) {
      hl_start <- details$truth$highlight_start[1]  # Get first row value
      hl_end <- details$truth$highlight_end[1]
      if (!is.na(hl_start) && !is.na(hl_end)) {
        left_pct <- (hl_start / dur) * 100
        width_pct <- ((hl_end - hl_start) / dur) * 100
        highlight_data <- list(left = left_pct, width = width_pct)
      }
    }
    
    tagList(
      # Y-axis label (left side)
      tags$div(style = "display: flex; gap: 5px; align-items: stretch;",
               tags$div(style = "width: 40px; display: flex; align-items: center; justify-content: center; font-size: 11px; text-align: center; color: #666; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap;",
                        "Amplitude"
               ),
               tags$div(style = "flex: 1;",
                        # Main visualization container with border as axis
                        tags$div(style = "position: relative; width: 100%; display: inline-block; border-left: 1px solid #333; border-bottom: 1px solid #333;",
                                 tags$img(src = img_url, class = "static-viz wave-img", style = "width: 100%; display: block;"),
                                 
                                 tags$svg(id = "wave-svg-overlay",
                                          viewBox = "0 0 100 100",
                                          preserveAspectRatio = "none",
                                          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; z-index: 10;",
                                          
                                          if (!is.null(highlight_data)) {
                                            tags$rect(x = highlight_data$left, y = 0, 
                                                      width = highlight_data$width, height = 100,
                                                      fill = "rgba(0, 123, 255, 0.2)")
                                          },
                                          
                                          tags$line(id = "wave-playhead-line", x1 = 0, y1 = 0, x2 = 0, y2 = 100,
                                                    stroke = "red", `stroke-width` = "0.3", opacity = "0.8")
                                 )
                        )
               )
      ),
      
      # X-axis label (bottom)
      tags$div(style = "text-align: center; font-size: 11px; color: #666; margin-top: 2px;",
               "Time (s)"
      )
    )
  })
  
  # 5. Stream Audio & Drive Playhead
  output$audio_player_ui <- renderUI({
    req(data_storage$current_run > 0)
    audio_url <- data_storage$files_to_classify[data_storage$current_run]
    
    audio_tag <- tags$audio(
      id = "audio_element",
      src = audio_url, 
      type = "audio/wav",
      autoplay = TRUE,
      controls = TRUE,
      style = "width: 100%; outline: none;"
    )
    
    js_script <- tags$script(HTML("
    (function() {
      // Clear any existing intervals for this session
      if (window.__playheadIntervals) {
        window.__playheadIntervals.forEach(id => clearInterval(id));
      }
      window.__playheadIntervals = [];
      
      setTimeout(function() {
        var audio = document.getElementById('audio_element');
        
        if (audio) {
          audio.addEventListener('loadedmetadata', function() {
            Shiny.setInputValue('current_audio_duration', audio.duration);
          });
          
          audio.addEventListener('ended', () => {
            Shiny.setInputValue('audio_finished', Math.random(), {priority: 'event'});
          });
        }
        
        // Set up playhead interval
        var playheadInterval = setInterval(() => {
          var audio = document.getElementById('audio_element');
          if (audio && audio.duration) {
            var pct = (audio.currentTime / audio.duration) * 100;
            
            var specLine = document.getElementById('spec-playhead-line');
            if (specLine) {
              specLine.setAttribute('x1', pct);
              specLine.setAttribute('x2', pct);
            }
            
            var waveLine = document.getElementById('wave-playhead-line');
            if (waveLine) {
              waveLine.setAttribute('x1', pct);
              waveLine.setAttribute('x2', pct);
            }
          }
        }, 50);
        
        window.__playheadIntervals.push(playheadInterval);
      }, 100);
    })();
  "))
    
    tagList(audio_tag, js_script)
  })
  
  output$file_info <- renderText({
    req(data_storage$files_to_classify, data_storage$current_run)
    total_runs <- length(data_storage$files_to_classify)
    paste0("Run ", data_storage$current_run, " of ", total_runs)
  })
  
  # 6. Advance Logic & Google Submission
  advance_ui <- function() {
    total_files <- length(data_storage$files_to_classify)
    data_storage$current_run <- data_storage$current_run + 1
    
    if (data_storage$current_run > total_files) {
      shinyjs::show("score_overlay")  # ← USE score_overlay HERE
      shinyjs::hide("main_ui")
      
      score <- sum(data_storage$user_answers == data_storage$ground_truth$true_class, na.rm = TRUE)
      data_storage$final_score <- score
      
      submit_score_to_google(score = score, total = total_files)
    }
  }
  
  # Classification Events (Includes Hotkeys)
  observeEvent(list(input$btn_squawk, input$hotkey_1), { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Squawk"; advance_ui() })
  observeEvent(list(input$btn_alarm, input$hotkey_2), { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Alarm"; advance_ui() })
  observeEvent(list(input$btn_other, input$hotkey_3), { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Other Vocalisation"; advance_ui() })
  observeEvent(list(input$btn_noise, input$hotkey_4), { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Noise"; advance_ui() })
  observeEvent(list(input$btn_unknown, input$hotkey_5), { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Unknown"; advance_ui() })
  observeEvent(input$btn_skip, { req(data_storage$current_run > 0); data_storage$user_answers[data_storage$current_run] <- "Skipped"; advance_ui() })
  
  observeEvent(input$btn_prev, {
    req(data_storage$current_run > 1)
    if (length(data_storage$user_answers) >= data_storage$current_run - 1) {
      data_storage$user_answers <- data_storage$user_answers[-(data_storage$current_run - 1)]
    }
    data_storage$current_run <- data_storage$current_run - 1
  })
  
  # 7. Final Score & Google Response Handling
  observeEvent(input$score_submitted, {
    if (input$score_submitted) {
      cat("Score submitted, fetching stats from sheet...\n")
      
      tryCatch({
        # Read the Scores sheet directly
        scores_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaY_bScsK72SRCZ8FfdxmM5CvK-gtHTl6SvaRvmmTSvmtz9kbnUK-MEdmuCgg5doQQ0RZYZuoPgNsK/pub?gid=0&single=true&output=csv")
        
        all_scores <- as.numeric(scores_data$UserScore)
        all_scores <- all_scores[!is.na(all_scores)]
        
        avg_score <- mean(all_scores, na.rm = TRUE)
        total_users <- length(all_scores)
        
        cat("Calculated average:", avg_score, "Total users:", total_users, "\n")
        
        total_files <- length(data_storage$files_to_classify)
        your_score <- data_storage$final_score
        
        your_pct <- (your_score / total_files) * 100
        avg_pct <- (avg_score / total_files) * 100
        
        # Create frequency table
        tryCatch({
          score_freq <- as.data.frame(table(all_scores))
          colnames(score_freq) <- c("Score", "Frequency")
          score_freq$Score <- as.numeric(as.character(score_freq$Score))
          score_freq$Frequency <- as.numeric(score_freq$Frequency)
          score_freq <- score_freq[order(score_freq$Score), ]
          
          cat("Frequency table created. Rows:", nrow(score_freq), "\n")
          cat("Score values:", paste(score_freq$Score, collapse=", "), "\n")
          cat("Frequency values:", paste(score_freq$Frequency, collapse=", "), "\n")
          
          # Pre-calculate bar heights
          max_freq <- if(nrow(score_freq) > 0) max(score_freq$Frequency, na.rm=TRUE) else 1
          bar_heights <- (score_freq$Frequency / max_freq) * 150
          
          cat("Max frequency:", max_freq, "\n")
          cat("Bar heights:", paste(round(bar_heights), collapse=", "), "\n")
          
        }, error = function(e) {
          cat("Error creating frequency table:", e$message, "\n")
          stop(e)
        })
        
        output$final_score_display <- renderUI({
          tagList(
            h3(paste("Your Score: ", your_score, " / ", total_files), 
               style = "color: #007bff; margin: 10px 0;"),
            
            hr(),
            
            h4("Conference Statistics", style = "margin-top: 20px;"),
            
            fluidRow(
              column(6,
                     div(style = "text-align: center; padding: 10px; background: #f8f9fa; border-radius: 8px;",
                         h5("Average Score", style = "color: #666; margin: 5px 0;"),
                         h3(paste(round(data_storage$avg_score, 1), " / ", data_storage$total_files), 
                            style = "color: #28a745; margin: 5px 0;"),
                         p(paste("Total participants: ", data_storage$total_users), 
                           style = "margin: 2px 0 0 0; font-size: 12px; color: #999;")
                     )
              ),
              column(6,
                     div(style = "text-align: center; padding: 10px; background: #f8f9fa; border-radius: 8px;",
                         tags$div(style = "display: flex; gap: 20px; align-items: flex-end; justify-content: center; height: 100px; margin-bottom: 5px;",
                                  tags$div(style = paste0("width: 40px; height: ", data_storage$your_pct, "%; background: linear-gradient(180deg, #007bff, #0056b3); border-radius: 4px 4px 0 0;"))
                                  ,
                                  tags$div(style = paste0("width: 40px; height: ", data_storage$avg_pct, "%; background: linear-gradient(180deg, #28a745, #20c997); border-radius: 4px 4px 0 0;"))
                         ),
                         tags$div(style = "display: flex; gap: 20px; justify-content: center; font-size: 11px; font-weight: bold;",
                                  tags$div("You", style = "width: 40px; color: #007bff;"),
                                  tags$div("Avg", style = "width: 40px; color: #28a745;")
                         )
                     )
              )
            ),
            
            hr(),
            
            h4("Score Distribution", style = "margin-top: 20px;"),
            
            if (nrow(score_freq) > 0) {
              tags$div(style = "display: flex; gap: 15px; align-items: flex-end; justify-content: center; padding: 20px; background: #f8f9fa; border-radius: 8px; min-height: 200px;",
                       lapply(1:nrow(score_freq), function(i) {
                         score_val <- score_freq$Score[i]
                         freq <- score_freq$Frequency[i]
                         bar_height <- round(bar_heights[i])
                         
                         tags$div(style = "text-align: center; display: flex; flex-direction: column; align-items: center;",
                                  tags$div(style = paste0("width: 40px; height: ", bar_height, "px; background: #6c757d; border-radius: 4px 4px 0 0;"))
                                  ,
                                  tags$div(paste(score_val), style = "font-size: 11px; font-weight: bold; margin-top: 8px;"),
                                  tags$div(paste("n=", freq), style = "font-size: 10px; color: #999;")
                         )
                       })
              )
            } else {
              tags$div(style = "color: #999; text-align: center;", "No distribution data")
            }
          )
        })
        
        shinyjs::hide("score_overlay")
        shinyjs::show("completion_ui")
        
      }, error = function(e) {
        cat("Error reading sheet:", e$message, "\n")
        
        output$final_score_display <- renderUI({
          total_files <- length(data_storage$files_to_classify)
          tagList(
            h2("Quiz Complete!"),
            h3(paste("Your Score: ", data_storage$final_score, " / ", total_files)),
            p("Error: Could not retrieve crowd statistics.")
          )
        })
        
        shinyjs::hide("score_overlay")
        shinyjs::show("completion_ui")
      })
    }
  })
  
  observeEvent(input$restart_app, {
    shinyjs::hide("completion_ui")
    shinyjs::show("pre_process_sidebar")
  })
}
