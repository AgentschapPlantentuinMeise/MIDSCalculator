# Installation
The latest self-contained WINDOWS installer (generated using Inno Setup) can be found here: https://drive.google.com/drive/folders/1ioRhHIvdYI88yoPTsYLG_k5-8n-05CiP Note that this installer will install (a more recent) version of R if it is not available on your system already. The app currently works with R 4.2.0. It may function with older and newer versions of R, but this cannot be guaranteed.

Alternatively, you can download (or clone) the contents of this repository and run the code yourself using your local R instance. Running the `app.R` file should launch the app in your browser. Required R packages should automatically be installed if not present already during the first launch of the app. 

For quicklaunch, a .bat file is available for Windows (`run_MIDSCalculator_windows.bat`) and a .sh file for Linux systems (`run_MIDSCalculator_linux.sh`). Executing these files after cloning/downloading this repository and installing R should cause the Rshiny app to launch in your browser. You may be prompted once to paste the path to your R installation.

Due to the large memory requirements this app may pose, calculating scores for millions of specimens, a web-hosted version of this app is currently not available.
