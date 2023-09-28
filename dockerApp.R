################################################################################
# This script is the same as the app.R script but with a few different options #
# passed to runApp so that running the app from inside a docker container is   #
# easier.                                                                      #
################################################################################

library(shiny)
folder_address = paste0(getwd(),"/src/Shiny_MIDS/MIDScalcApp.R")
runApp(folder_address, launch.browser=F, host='0.0.0.0', port=3000)
