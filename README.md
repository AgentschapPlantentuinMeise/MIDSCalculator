# MIDS-Lynn

Repository for code written during the internship of Lynn Delgat

## Towards the MIDS data standard: developing a Shiny app to calculate digitization levels of natural history specimens

Minimum Information about a Digital Specimen (MIDS) is a data standard which aims to describe four different levels of digitization status for biological and geological specimens from natural history collections, based on the availability of data in digital records of these specimens. With this standard, users of collection data, as well as funders and collection curators will have a better overview of how comprehensively digitized a collection or any other set of specimens really is. The standard is currently still under development by a Task Group of the global Biodiversity Information Standards (“TDWG”) organization. While properties for the different levels have been proposed, no definite implementation of MIDS has been made yet. 

In this internship, a Shiny app is constructed which allows users to calculate MIDS scores for each record in a submitted dataset. We focus on the commonly used Darwin Core Archive as a data standard for the sourced data. As the MIDS standard is agnostic to the data model of the sourced data, the app includes a JSON schema mapping the sourced data to the different MIDS properties. This schema is editable through the app’s interface and may facilitate testing variations in the implementation of MIDS itself, aiding further development of the standard. 

## Code
Code can be found in the /src folder.

### Two scripts to calculate MIDS levels based on a a given JSON schema
parse_json_schema.R    
* 2 functions:
  * read_json_mids_criteria(file): uses the MIDS sections of the JSON schema and returns criteria per MIDS level
  * read_json_unknownOrMissing(file): uses the unknownOrMissing section of the JSON schema and returns a list of these values
* no need to run this separately, is loaded in MIDS-calc.R

MIDS-calc.R
* given a JSON schema and a dataset, this script calculates for each record which criteria are met and the MIDS level 

### Shiny app
Code for the Shiny app can be found in the /src/Shiny_MIDS folder.

MIDScalcApp.R
* main code for the app

/src/Shiny_MIDS/R folder contains 4 modules:
  * CloseTabModule.R
    * allows to close results tabs
  * InteractiveSchemaModule.R
    * allows to edit the MIDS implementation interactively 
  * ResultsModule.R 
    * calculates MIDS levels and which criteria are met
    * opens a tab showing the results for each analysis
    * allows to export results to csv 
  * ViewImplementationModule.R
    * allows to visualize the MIDS implementation 

## Data
### Datasets
occurrence.txt
* GBIF Occurrence Download https://doi.org/10.15468/dl.vguer4
* Meise Botanic Garden Herbarium (BR) + Malvaceae
* Used for initial testing

test_dataset.txt
* Artificial dataset to test MIDS calculations

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

## Reports
Testing_datasets.Rmd shows the results of the MIDS calculator script on several datasets.

## Session info
Code was run with the following packages and versions:

```
> sessionInfo()
R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_Belgium.1252  LC_CTYPE=English_Belgium.1252    LC_MONETARY=English_Belgium.1252
[4] LC_NUMERIC=C                     LC_TIME=English_Belgium.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] magrittr_2.0.1     purrr_0.3.4        data.table_1.14.2  dplyr_1.0.7        jsonlite_1.7.3    
 [6] RColorBrewer_1.1-2 sortable_0.4.5     shinyjs_2.1.0      DT_0.21            ggplot2_3.3.5     
[11] shinyBS_0.61.1     shiny_1.7.1       

loaded via a namespace (and not attached):
 [1] bslib_0.3.1       tidyselect_1.1.1  xfun_0.29         learnr_0.10.1     colorspace_2.0-2 
 [6] vctrs_0.3.8       generics_0.1.1    htmltools_0.5.2   yaml_2.2.1        utf8_1.2.2       
[11] XML_3.99-0.8      rlang_0.4.12      jquerylib_0.1.4   later_1.3.0       pillar_1.6.4     
[16] glue_1.6.0        withr_2.4.3       DBI_1.1.2         lifecycle_1.0.1   stringr_1.4.0    
[21] munsell_0.5.0     gtable_0.3.0      fontawesome_0.2.2 htmlwidgets_1.5.4 evaluate_0.14    
[26] labeling_0.4.2    knitr_1.37        fastmap_1.1.0     crosstalk_1.2.0   httpuv_1.6.5     
[31] markdown_1.1      fansi_1.0.2       Rcpp_1.0.8        xtable_1.8-4      promises_1.2.0.1 
[36] scales_1.1.1      cachem_1.0.6      farver_2.1.0      mime_0.12         digest_0.6.29    
[41] stringi_1.7.6     grid_4.0.3        rprojroot_2.0.2   tools_4.0.3       sass_0.4.0       
[46] tibble_3.1.6      crayon_1.4.2      pkgconfig_2.0.3   ellipsis_0.3.2    rsconnect_0.8.25 
[51] xml2_1.3.3        assertthat_0.2.1  rmarkdown_2.11    R6_2.5.1          compiler_4.0.3   
```


