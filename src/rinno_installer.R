library(RInno)
library(ini)

config = read.ini("config.ini")

source("src/packages.R")

#fix bad R version numbers in two functions
#loads updated functions in current environment only

body(code_section)[[4]][[3]][[2]][[2]][[2]] <- substitute(
  stringr::str_extract(readLines("https://cran.rstudio.com/bin/windows/base/",
                                 warn = F), "[1-4]\\.[0-9]+\\.[0-9]+")
  )
body(code_section)[[4]][[3]][[3]][[2]] <- substitute(
  stringr::str_extract(readLines("https://cran.rstudio.com/bin/windows/base/old/", 
                                 warn = F), "[1-4]\\.[0-9]+\\.[0-9]+")
)
body(get_R)[[4]][[3]][[2]][[2]][[3]] <- substitute(
  stringr::str_extract("[1-4]\\.[0-9]+\\.[0-9]+")
)
body(get_R)[[6]][[2]] <- substitute(
  latest_R_version[1] == R_version
)
body(get_R)[[5]][[3]][[2]][[3]] <- substitute(
  stringr::str_extract("[1-4]\\.[0-9]+\\.[0-9]+")
)

#set the app name
name = "MIDSCalculator"

#copy installer default and required files to app's directory
copy_installation(overwrite=F)
file.remove("default.ico")
file.remove("setup.ico")

#download R installer if not already present in app's directory
get_R()

#create a BAT file that will launch the shiny app
create_bat(app_name = name,
           app_dir = getwd())

#create a config file for the app
create_config(app_name = name,
              pkgs = packages,
              pkgs_path = "bin")

#remove any temporary biocase files that may have been left
#after an aborted calculation
if (dir.exists("src/Shiny_MIDS/temp_biocase")) {
  unlink("src/Shiny_MIDS/temp_biocase",
         recursive = T)
}

#create an ISS file that declares how the installer should be built
start_iss(app_name = name) %>%
  
  # C-like directives
  directives_section(include_R = T,
                     app_version = config$app$version,
                     publisher = "Meise Botanic Garden", 
                     main_url = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator") %>%
  
  # Setup Section
  setup_section(app_dir = getwd(),
                setup_icon  = "src/Shiny_MIDS/www/icon2.ico",
                dir_out = "RInno_installer",
                pub_url = "https://www.plantentuinmeise.be", 
                sup_url = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator/issues",
                upd_url = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator",
                privilege = "lowest",
                info_before = "infobefore.txt",
                info_after = "infoafter.txt") %>%
  
  # Languages Section
  languages_section() %>%
  
  # Tasks Section
  tasks_section(desktop_icon = T) %>%
  
  # Files Section
  files_section(app_dir = getwd(),
                user_browser = "chrome") %>%
  
  # Icons Section
  icons_section(app_desc = paste0("The MIDSCalculator app allows users to calculate MIDS levels for",
                                  " datasets of natural history specimens. Currently, only Darwin Core",
                                  " Archives as produced by GBIF are supported."),
                app_dir = getwd(),
                app_icon = "src/Shiny_MIDS/www/icon2.ico",
                prog_menu_icon = T,
                desktop_icon   = T) %>%
  gsub("commonprograms","autoprograms",.) %>%
  gsub("commondesktop","autodesktop",.) %>%
  
  # Execution & Pascal code to check registry during installation
  # If the user has R, don't give them an extra copy
  # If the user needs R, give it to them
  run_section() %>%
  code_section() %>%
  
  # Write the Inno Setup script
  writeLines(paste0(name,".iss"))

#compile the installer based on the ISS file
compile_iss()
#include version nr in the installer filename
file.rename("RInno_installer/setup_MIDSCalculator.exe",
            paste0("RInno_installer/setup_MIDSCalculator ",
                   config$app$version,
                   ".exe"))
