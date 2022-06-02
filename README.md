# MIDS-Lynn

Repository for the code written during the internship

## Developing a calculator of the digitization levels of digital specimens

Minimum Information about a Digital Specimen (MIDS) is a data standard which aims to describe four different levels of digitization status for biological and geological specimens from natural history collections, based on the availability of data in digital records of these specimens. With this standard, users of collection data, as well as funders and collection curators will have a better overview of how comprehensively digitized a collection or any other set of specimens really is. The standard is currently still under development by a Task Group of the global Biodiversity Information Standards (“TDWG”) organization. While properties for the different levels have been proposed, no definite implementation of MIDS has been made yet. 

In this internship, we plan to construct an app that allows users to calculate MIDS scores for each record in a submitted dataset. As the MIDS standard is agnostic to the data model of the sourced data, this app will include a schema (JSON or XML) mapping the sourced data to the different MIDS properties. This schema will be editable through the app’s interface and may facilitate testing variations in the implementation of MIDS itself, aiding further development of the standard. We will focus on the commonly used Darwin Core Archive as a data standard for the sourced data, but the schema will have to take into account variations in how this standard is implemented by different collections. The app will be developed using the R programming language and will include visualizations and analyses of the calculated MIDS scores. Depending on the required dynamics, the app may make use of R shiny or a HTML5 interface. Tidyverse packages or direct SQL statements will be used to query the data.

## Scripts
parse_json_schema.R    
* 2 functions:
  * read_json_mids_criteria(file): uses the MIDS sections of the JSON schema and returns criteria per MIDS level
  * read_json_unknownOrMissing(file): uses the unknownOrMissing section of the JSON schema and returns a list of these values
* no need to run this separately, is loaded in MIDS-calc.R

MIDS-calc.R
* given a JSON schema and a dataset, this script calculates for each record which criteria are met and the MIDS level 
## Data
### Datasets
occurrence.txt
* GBIF Occurrence Download https://doi.org/10.15468/dl.vguer4
* Meise Botanic Garden Herbarium (BR) + Malvaceae
* Used for initial testing

### Schemas
firstschema.json
* First draft (07/21)
 
secondschema.json
* Second draft (02/22)
  * informationWithheld currently not used
  * unknownOrMissing only values for which midsAchieved is False
  * don't specify midsAchieved for MIDS conditions
  * some properties updated

secondschema_conditions_same_level.json
* Second draft (02/22) with some small changes:
  * Placed all conditions on the same nesting level
  * Removed verbatimCoordinates
  * Apply "Unknown" in unknownOrMissing to all
  * Added empty string to unknownOrMissing

## RInno
* Follow instructions on https://github.com/ficonsulting/RInno
    * If Windows version not supported (64bit), install with installr following instructions here https://github.com/ficonsulting/RInno/issues/118#issuecomment-460094226
    * For R versions > 3, you have to edit two functions following https://github.com/ficonsulting/RInno/issues/152#issuecomment-681009752 or install the fork from github: brandonerose/RInno
* So to create the installer:
```
setwd("#repopath/src")
require(RInno)
trace(RInno::code_section,edit=T) #replace 1-3 with 1-4
trace(get_R,edit=T) #replace 1-3 with 1-4
create_app(app_name = "MIDSCalculator",pkgs = c("shiny","shinyBS","ggplot2","DT","shinyjs","sortable","dplyr","data.table","purrr","magrittr","jsonlite"),include_R = T)
#depending on the versions of your installed packages, an update may be requested
compile_iss()
```
* Then launch the .exe in the default dir of RInno_installer (probably as admin is required)