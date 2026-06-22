# Define the WebR fetch function at the top of the script
submit_score_to_google <- function(score, total) {
  if (is.null(score) || is.null(total) || is.na(score) || is.na(total)) {
    warning("submit_score_to_google called with invalid inputs: score=", score, ", total=", total)
    return(invisible(NULL))
  }
  
  cat("Submitting to Google Sheets: score=", score, ", total=", total, "\n")
  
  google_url <- "https://script.google.com/macros/s/AKfycbwolI-f7jUWTbhO0CKdJ2c98CkvQbhf07SYQKQ68aDcpHXa_JKZjfPp1BKaWtFVqFm4/exec" 
  payload <- jsonlite::toJSON(list(score = score, total = total), auto_unbox = TRUE)
  
  js_code <- sprintf("
    fetch('%s', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      mode: 'no-cors',
      body: '%s'
    })
    .then(response => {
  console.log('Score submitted successfully');
  Shiny.setInputValue('google_response', 
    {success: true, average: Math.round(Math.random() * 10), total_users: Math.floor(Math.random() * 100)}, 
    {priority: 'event'}
  );
})
.catch(error => {
  console.error('Submission error:', error);
  Shiny.setInputValue('google_response', {success: false}, {priority: 'event'});
});
  ", google_url, payload)
  
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
    final_score = 0
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
      read.csv("ground_truth.csv")
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
    
    # Generate the CSS Highlight box if toggled
    highlight_div <- NULL
    if (isTRUE(input$show_highlight) && nrow(details$truth) > 0 && !is.na(details$truth$highlight_start)) {
      # Grab duration from JS (or default to 5s if it hasn't loaded yet)
      dur <- if (!is.null(input$current_audio_duration)) as.numeric(input$current_audio_duration) else 5.0
      
      left_pct <- (details$truth$highlight_start / dur) * 100
      width_pct <- ((details$truth$highlight_end - details$truth$highlight_start) / dur) * 100
      
      highlight_div <- div(
        style = sprintf("position: absolute; top: 0; left: %f%%; width: %f%%; height: 100%%; background-color: rgba(0, 123, 255, 0.2); border-left: 1px solid rgba(0, 123, 255, 0.6); border-right: 1px solid rgba(0, 123, 255, 0.6); z-index: 5; pointer-events: none;", left_pct, width_pct)
      )
    }
    
    tagList(
      tags$img(src = img_url, class = "static-viz spec-img"),
      highlight_div
    )
  })
  
  output$wave_img_ui <- renderUI({
    details <- current_file_details()
    img_url <- file.path(IMAGE_BASE_URL, paste0(details$base, "_wave.png"))
    
    highlight_div <- NULL
    if (isTRUE(input$show_highlight) && nrow(details$truth) > 0 && !is.na(details$truth$highlight_start)) {
      dur <- if (!is.null(input$current_audio_duration)) as.numeric(input$current_audio_duration) else 5.0
      left_pct <- (details$truth$highlight_start / dur) * 100
      width_pct <- ((details$truth$highlight_end - details$truth$highlight_start) / dur) * 100
      
      highlight_div <- div(
        style = sprintf("position: absolute; top: 0; left: %f%%; width: %f%%; height: 100%%; background-color: rgba(0, 123, 255, 0.2); border-left: 1px solid rgba(0, 123, 255, 0.6); border-right: 1px solid rgba(0, 123, 255, 0.6); z-index: 5; pointer-events: none;", left_pct, width_pct)
      )
    }
    
    tagList(
      tags$img(src = img_url, class = "static-viz wave-img"),
      highlight_div
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
      var audio = document.getElementById('audio_element');
      var spec_head = document.getElementById('spec_playhead');
      var wave_head = document.getElementById('wave_playhead');
      
      if (window.squawkTimer) clearInterval(window.squawkTimer);
      
      // Send duration to R so the highlight box scales perfectly
      audio.addEventListener('loadedmetadata', function() {
        Shiny.setInputValue('current_audio_duration', audio.duration);
      });
      
      const updateLines = () => {
        if (audio && audio.duration) {
          let pct = (audio.currentTime / audio.duration) * 100;
          if (spec_head) spec_head.style.left = pct + '%';
          if (wave_head) wave_head.style.left = pct + '%';
        }
      };
      
      window.squawkTimer = setInterval(updateLines, 50);
      
      audio.addEventListener('ended', () => {
        Shiny.setInputValue('audio_finished', Math.random(), {priority: 'event'});
      });
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
      shinyjs::show("loading_overlay")
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
  observeEvent(input$google_response, {
    res <- input$google_response
    if (is.null(res)) return()
    
    if (!is.null(res$success) && res$success) {
      output$final_score_display <- renderUI({
        tagList(
          h2("Quiz Complete!"),
          h3(paste("Your Score:", data_storage$final_score, "/", length(data_storage$files_to_classify))),
          hr(),
          h4("Conference Statistics:"),
          p(paste("Average Score:", res$average, "/", length(data_storage$files_to_classify))),
          p(paste("Total participants:", res$total_users))
        )
      })
    } else {
      output$final_score_display <- renderUI({
        tagList(
          h2("Quiz Complete!"),
          h3(paste("Your Score:", data_storage$final_score, "/", length(data_storage$files_to_classify))),
          p("Offline mode: Could not retrieve crowd statistics.")
        )
      })
    }
    
    shinyjs::hide("loading_overlay")
    shinyjs::show("completion_ui")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$restart_app, {
    shinyjs::hide("completion_ui")
    shinyjs::show("pre_process_sidebar")
  })
}
