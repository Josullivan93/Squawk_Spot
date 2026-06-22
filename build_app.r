# build_app.R
library(shinylive)

# 1. Export the app
message("Exporting Shinylive app...")
shinylive::export("app", "squawk-spot")

# 2. Define the Splash Screen strings
splash_css <- "
  <style>
    body { background-color: #f4f7f6; margin: 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
    #shinylive-splash { position: absolute; top: 0; left: 0; width: 100vw; height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f4f7f6; z-index: 9999; transition: opacity 0.5s ease-out; }
    .splash-progress-container { width: 80%; max-width: 400px; height: 8px; background-color: #e9ecef; border-radius: 4px; overflow: hidden; margin-bottom: 20px; box-shadow: inset 0 1px 3px rgba(0,0,0,0.1); }
    #splash-progress-bar { width: 0%; height: 100%; background: linear-gradient(90deg, #007bff, #0056b3); border-radius: 4px; transition: width 0.5s ease-out; }
    .splash-text { color: #333; font-size: 1.5rem; font-weight: bold; }
    .splash-subtext { color: #666; font-size: 1rem; margin-top: 10px; }
  </style>
</head>
"

splash_html <- "
<body>
  <div id='shinylive-splash'>
    <div class='splash-progress-container'><div id='splash-progress-bar'></div></div>
    <div class='splash-text'>Loading Squawk-Spot...</div>
    <div class='splash-subtext' id='splash-status'>Downloading audio engine...</div>
  </div>
  <script>
    document.addEventListener('DOMContentLoaded', function() {
      var bar = document.getElementById('splash-progress-bar');
      var status = document.getElementById('splash-status');
      if (bar) {
        setTimeout(() => { bar.style.width = '20%'; }, 100);
        setTimeout(() => {
          bar.style.transition = 'width 20s cubic-bezier(0.1, 0.5, 0.1, 1)';
          bar.style.width = '90%';
          if(status) status.innerText = 'Extracting environment...';
        }, 600);
      }
    });
  </script>
"

# 3. Read the generated index.html
index_path <- "squawk-spot/index.html"
html_content <- readLines(index_path, warn = FALSE)
html_string <- paste(html_content, collapse = "\n")

# 4. Inject the CSS right before </head> and the HTML right after <body>
html_string <- sub("</head>", splash_css, html_string, fixed = TRUE)
html_string <- sub("<body>", splash_html, html_string, fixed = TRUE)

# 5. Save it back
writeLines(html_string, index_path)
message("Splash screen successfully injected!")