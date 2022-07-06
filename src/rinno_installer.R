library(RInno)

source("packages.R")

trace(RInno::code_section,edit=T) #replace 1-3 with 1-4
trace(get_R,edit=T) #replace 1-3 with 1-4

name = "MIDSCalculator"

copy_installation()
get_R()
create_bat(app_name = name,
           app_dir = getwd())
create_config(app_name = name,
              pkgs = packages,
              pkgs_path = "bin")
start_iss(app_name = name) %>%
  
  # C-like directives
  directives_section(app_version = "0.2",
                     publisher   = "Meise Botanic Garden", 
                     main_url    = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator") %>%
  
  # Setup Section
  setup_section(app_dir = getwd(),
                setup_icon  = "src/Shiny_MIDS/www/icon.ico",
                dir_out = "RInno_installer",
                pub_url     = "https://www.plantentuinmeise.be", 
                sup_url     = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator/issues",
                upd_url     = "https://github.com/AgentschapPlantentuinMeise/MIDSCalculator") %>%
  
  # Languages Section
  languages_section() %>%
  
  # Tasks Section
  tasks_section(desktop_icon = T) %>%
  
  # Files Section
  files_section(app_dir = getwd(),
                user_browser = "chrome") %>%
  
  # Icons Section
  icons_section(app_desc       = paste0("The MIDSCalculator app allows users to calculate MIDS levels for",
                                        " datasets of natural history specimens. Currently, only Darwin Core",
                                        " Archives as produced by GBIF are supported."),
                app_dir = getwd(),
                app_icon       = "src/Shiny_MIDS/www/icon.ico",
                prog_menu_icon = T,
                desktop_icon   = T) %>%
  
  # Execution & Pascal code to check registry during installation
  # If the user has R, don't give them an extra copy
  # If the user needs R, give it to them
  run_section() %>%
  code_section() %>%
  
  # Write the Inno Setup script
  writeLines(paste0(name,".iss"))
compile_iss()
