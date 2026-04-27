@echo off
setlocal enabledelayedexpansion

:: 1. Move to the app directory
cd /d "%~dp0"

:: 2. Find Rscript.exe
:: First, check if it's in the System PATH
where Rscript >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    set R_BIN=Rscript
) else (
    :: If not in PATH, search the default Program Files directory for the latest version
    for /f "delims=" %%i in ('dir /b /s "C:\Program Files\R\Rscript.exe" 2^>nul') do set R_BIN="%%i"
)

:: 3. Check if R was found
if not defined R_BIN (
    echo [ERROR] R was not found.
    pause
    exit /b
)

:: 4. Run the app with renv activation
:: We source renv/activate.R before running the app to lock in the correct libraries
:: We run renv::status to confirm renv is correct and correct with restore if needed
%R_BIN% --quiet --no-save -e "source('renv/activate.R'); if(!renv::status()$synchronized) { renv::restore(prompt=FALSE) }; shiny::runApp(getwd(), launch.browser = TRUE)"

pause >nul