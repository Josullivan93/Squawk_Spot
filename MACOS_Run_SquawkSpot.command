#!/bin/zsh

# 1. Set working directory to script location
cd "$(dirname "$0")"

# 2. Find Rscript
RSCRIPT_PATH=$(which Rscript)
if [ -z "$RSCRIPT_PATH" ]; then
    RSCRIPT_PATH="/usr/local/bin/Rscript"
fi

# 3. Launch App with Synchronized Payload
# We use isFALSE() here too so the R code is 1:1 with your .bat file
$RSCRIPT_PATH --quiet --no-save -e "source('renv/activate.R'); if(isFALSE(renv::status()\$synchronized)) { message('--- Updating Library ---'); renv::restore(prompt=FALSE) }; shiny::runApp(getwd(), launch.browser = TRUE)"

# 4. Conditional Pause (Only stay open if R exits with an error)
if [ $? -ne 0 ]; then
    echo ""
    echo "[ERROR] The app closed unexpectedly."
    echo "Press any key to close this window..."
    read -n 1
fi