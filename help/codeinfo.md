# Code
Different scripts can be found in the /src folder.

### Two scripts to calculate MIDS levels based on a a given JSON schema
`parse_json_schema.R`  
* 2 functions:
  * read_json_mids_criteria(file): uses the MIDS sections of the JSON schema and returns criteria per MIDS level
  * read_json_unknownOrMissing(file): uses the unknownOrMissing section of the JSON schema and returns a list of these values
* no need to run this separately, is loaded in MIDS-calc.R

`MIDS-calc.R`
* given a JSON schema and a dataset, this script calculates for each record which criteria are met and the MIDS level 

### Package checker
`packages.R`
* required packages (excluding imported packages) for the app. These will be auto-installed when running the app if not present and are also used to build the installer.

### Installer generator
`rinno_installer.R`
* this script is not used by the app. It is run separately to generate an installer using RInno setup. [More info](/help/rinno_installer.md).

### Shiny app
Code for the Shiny app can be found in the /src/Shiny_MIDS folder.

`MIDScalcApp.R`
* main code for the app

/src/Shiny_MIDS/R folder contains 4 modules:
  * `CloseTabModule.R`
    * allows to close results tabs
  * `InteractiveSchemaModule.R`
    * allows to edit the MIDS implementation interactively 
  * `ResultsModule.R` 
    * calculates MIDS levels and which criteria are met
    * opens a tab showing the results for each analysis
    * allows to export results to csv 
  * `ViewImplementationModule.R`
    * allows to visualize the MIDS implementation 
