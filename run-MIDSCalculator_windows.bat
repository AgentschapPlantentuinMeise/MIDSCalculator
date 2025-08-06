@echo off
setlocal

REM === Step 1: Check if Rscript is in PATH ===
where Rscript >nul 2>&1
if %ERRORLEVEL%==0 (
    set "RSCRIPT=Rscript"
    goto :launch
)

REM === Step 2: Check for cached path ===
if exist "r_path.txt" (
    set /p RSCRIPT=<r_path.txt
    if exist "%RSCRIPT%" goto :launch
    echo Cached Rscript path is invalid: "%RSCRIPT%"
)

REM === Step 3: Ask user to provide Rscript path ===
set /p RPATH="Rscript not found. Please enter full path to Rscript.exe (e.g., C:\Program Files\R\R-4.4.0\bin\Rscript.exe): "
if not exist "%RPATH%" (
    echo The path "%RPATH%" does not exist or is not valid. Exiting.
    pause
    exit /b 1
)
set "RSCRIPT=%RPATH%"
echo %RSCRIPT% > r_path.txt
echo Path saved in r_path.txt for future use.

:launch
REM === Step 4: Launch Shiny App ===
cd /d "%~dp0"
"%RSCRIPT%" app.R

endlocal