# build_app.R
library(shinylive)

message("Exporting Shinylive app...")
shinylive::export("app", "squawk-spot")

# index_path <- "squawk-spot/index.html"
# html_content <- readLines(index_path, warn = FALSE)
# html_string <- paste(html_content, collapse = "\n")
# 
# # Inject CSS before </head>
# splash_css <- "
# <style>
#   body { background-color: #f4f7f6; margin: 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
#   #shinylive-splash { position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f4f7f6; z-index: 9999; transition: opacity 0.5s ease-out; pointer-events: auto; }
#   #shinylive-splash.fade-out { opacity: 0; pointer-events: none; }
#   .splash-progress-container { width: 80%; max-width: 400px; height: 8px; background-color: #e9ecef; border-radius: 4px; overflow: hidden; margin-bottom: 20px; }
#   #splash-progress-bar { width: 0%; height: 100%; background: linear-gradient(90deg, #007bff, #0056b3); border-radius: 4px; }
#   .splash-text { color: #333; font-size: 1.5rem; font-weight: bold; }
#   .splash-subtext { color: #666; font-size: 1rem; margin-top: 10px; }
# </style>
# "
# 
# html_string <- sub("</head>", paste(splash_css, "</head>"), html_string, fixed = TRUE)
# 
# # Inject HTML right after <body> opening tag (find first occurrence and insert after)
# html_string <- gsub(
#   "(<body[^>]*>)",
#   paste0('\\1\n<div id="shinylive-splash">\n',
#          '  <div class="splash-progress-container"><div id="splash-progress-bar"></div></div>\n',
#          '  <div class="splash-text">Loading Squawk-Spot...</div>\n',
#          '  <div class="splash-subtext" id="splash-status">Downloading audio engine...</div>\n',
#          '</div>'),
#   html_string
# )
# 
# # Inject initialization script before </body>
# splash_init_script <- "
# <script>
#   window.splashReady = false;
#   document.addEventListener('DOMContentLoaded', function() {
#     var bar = document.getElementById('splash-progress-bar');
#     var status = document.getElementById('splash-status');
#     if (bar) {
#       setTimeout(() => { 
#         bar.style.transition = 'width 2s ease-out';
#         bar.style.width = '60%'; 
#       }, 100);
#       setTimeout(() => {
#         bar.style.transition = 'width 5s ease-out';
#         bar.style.width = '90%';
#         if(status) status.innerText = 'Extracting environment...';
#       }, 2200);
#     }
#     window.splashReady = true;
#   });
#   
#   // Listen for Shiny to be ready
#   window.addEventListener('shiny:connected', function() {
#     var splash = document.getElementById('shinylive-splash');
#     if (splash) {
#       var bar = document.getElementById('splash-progress-bar');
#       if (bar) {
#         bar.style.transition = 'width 0.2s ease-out';
#         bar.style.width = '100%';
#       }
#       splash.classList.add('fade-out');
#       setTimeout(() => { splash.style.display = 'none'; }, 500);
#     }
#   });
# </script>
# "
# 
# html_string <- sub("</body>", paste(splash_init_script, "</body>"), html_string, fixed = TRUE)
# 
# writeLines(html_string, index_path)
message("Splash screen successfully injected!")