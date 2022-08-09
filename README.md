# MIDSCalculator

Repository for the MIDSCalculator app, written mostly by @LynnDelgat during an internship at Meise Botanic Garden.


## Minimum Information about a Digital Specimen

Minimum Information about a Digital Specimen (MIDS) is a data standard which aims to describe four different levels of digitization status for biological and geological specimens from natural history collections, each level requiring certain information elements (referred to as MIDS elements from here on) to be digitally available. With this standard, users of collection data, as well as funders and collection curators will have a better overview of how comprehensively digitized a collection or any other set of specimens really is. The standard is currently still under development by a Task Group of the global Biodiversity Information Standards (“TDWG”) organization. While MIDS elements for digitization levels 0 and 1 have been specified, MIDS elements for levels 2 and 3 are still under discussion.
For more details: https://github.com/tdwg/mids


## MIDSCalculator App
### Introduction
The MIDSCalculator is a Shiny app which allows users to calculate MIDS scores for each record in a submitted dataset, and explore the results. 

#### Mapping MIDS elements: use of a JSON schema

As the MIDS standard is agnostic to the data model of the sourced data, the app uses a JSON schema to map MIDS elements to properties of the sourced data. A schema mapping to GBIF annotated DwC data is included in the app. Other JSON schemas can be uploaded.  In addition the schema is editable through the app’s interface. 

### Installation

The latest self-contained windows installer (generated using Inno Setup) can be found here: https://drive.google.com/drive/folders/1ioRhHIvdYI88yoPTsYLG_k5-8n-05CiP

### Submit data

 On this page a zipped GBIF annotated Darwin Core Archive or a comma or tab separated occurrence file can be uploaded (max 5GB). In addition, the MIDS implementation can be specified and viewed. To specify the MIDS implementation you can either choose the default schema (included in the app) or upload a schema from file. It is also possible to choose to edit this schema interactively. The interactive editing opens in a pop-up window, where in a first tab, MIDS elements can be added, removed, or moved to another MIDS level. In addition, mappings can be removed or added by clicking the "edit" icon of a MIDS element. In a second tab, the Unknown or Missing section of the schema can be edited, i.e. new properties and new values can be added. This interactively edited schema can be saved to file (JSON). The schema (be it default, custom or interactive) can be viewed by clicking the eye icon, which opens a human-friendly visualization of the MIDS schema, so that it is not necessary to read the JSON file to be able to understand the specifics of the MIDS schema used. Once a dataset and a MIDS implementation have been chosen, calculations can be started by clicking "Start MIDS score calculations".

### Results

The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. The MIDS element plot can be clicked to get more details on the results of the mappings of that element. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code /taxonomic group/ collection date. The filename of the dataset is shown, as well as the used MIDS implementation, to make the provenance of the calculations clear.'
 


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
* GBIF Occurrence Download [10.15468/dl.e8jnan](http://doi.org/10.15468/dl.e8jnan) can be found as a zip file in the data folder for quick testing.

### Schemas
* [DwC-GBIF_schema.json](https://github.com/AgentschapPlantentuinMeise/MIDSCalculator/blob/main/data/schemas/DwC-GBIF_schema.json): This schema is based on the most recent MIDS specification for levels 0 and 1. As levels 2 and 3 are still under discussion, the schema offers a basic interpretation of several potential properties.

## Session info
Code was run with the following packages and versions:

```
> sessionInfo()
R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_Belgium.1252  LC_CTYPE=English_Belgium.1252   
[3] LC_MONETARY=English_Belgium.1252 LC_NUMERIC=C                    
[5] LC_TIME=English_Belgium.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] shinybusy_0.3.1    RColorBrewer_1.1-3 jsonlite_1.8.0     magrittr_2.0.1     purrr_0.3.4       
 [6] data.table_1.14.2  dplyr_1.0.8        sortable_0.4.5     shinyjs_2.1.0      DT_0.23           
[11] ggplot2_3.3.5      shinyBS_0.61.1     shiny_1.7.2       

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8.3      assertthat_0.2.1  rprojroot_2.0.3   digest_0.6.29     utf8_1.2.2       
 [6] mime_0.12         R6_2.5.1          evaluate_0.15     pillar_1.6.4      rlang_1.0.2      
[11] rstudioapi_0.13   fontawesome_0.3.0 jquerylib_0.1.4   learnr_0.10.1     rmarkdown_2.14   
[16] labeling_0.4.2    stringr_1.4.0     htmlwidgets_1.5.4 munsell_0.5.0     compiler_4.0.3   
[21] httpuv_1.6.5      xfun_0.29         pkgconfig_2.0.3   htmltools_0.5.2   sourcetools_0.1.7
[26] tidyselect_1.1.2  tibble_3.1.6      fansi_1.0.3       crayon_1.5.1      withr_2.5.0      
[31] later_1.3.0       grid_4.0.3        xtable_1.8-4      gtable_0.3.0      lifecycle_1.0.1  
[36] DBI_1.1.3         scales_1.2.0      cli_3.2.0         stringi_1.7.6     cachem_1.0.6     
[41] farver_2.1.0      promises_1.2.0.1  bslib_0.4.0       ellipsis_0.3.2    generics_0.1.3   
[46] vctrs_0.4.1       tools_4.0.3       glue_1.6.0        markdown_1.1      crosstalk_1.2.0  
[51] rsconnect_0.8.27  fastmap_1.1.0     yaml_2.2.1        colorspace_2.0-3  memoise_2.0.1    
[56] knitr_1.39        sass_0.4.1       
```

## Installer creation using RInno
* Follow instructions on https://github.com/ficonsulting/RInno
    * If Windows version not supported (64bit), install with installr following instructions here https://github.com/ficonsulting/RInno/issues/118#issuecomment-460094226
* To create the installer, run rinno_installer.R.
    * For R versions > 3, you have to edit two functions following https://github.com/ficonsulting/RInno/issues/152#issuecomment-681009752 or install the fork from github: brandonerose/RInno