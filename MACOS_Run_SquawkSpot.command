#!/bin/zsh

# 1. Navigate to the directory where this script is located
cd "$(dirname "$0")"

# 2. Find Rscript
# On Mac, R is usually in /usr/local/bin or found via 'which'
RSCRIPT_PATH=$(which Rscript)

if [ -z "$RSCRIPT_PATH" ]; then
    # Fallback for standard CRAN installation if not in PATH
    RSCRIPT_PATH="/usr/local/bin/Rscript"
fi

# 3. Launch the app with renv activation
$RSCRIPT_PATH --quiet --no-save -e "source('renv/activate.R'); if(!renv::status()\$synchronized) { message('--- Updating Library ---'); renv::restore(prompt=FALSE) }; shiny::runApp(getwd(), launch.browser = TRUE)"

# Keep the terminal window open to see errors
echo "App session closed. Press any key to exit."
read -n 1