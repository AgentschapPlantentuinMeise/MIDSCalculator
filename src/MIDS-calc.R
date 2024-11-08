library(dplyr)
library(data.table)
#library(purrr)
library(magrittr)
library(xml2)

calculate_mids <- function(gbiffile, 
                           jsonfile, 
                           jsontype = "file", 
                           jsonlist = NULL,
                           config,
                           session = NULL) {

  # Get data ----------------------------------------------------------------
  
  #get unknown or missing values
  if (jsontype == "file"){
    list_UoM <- read_json_unknownOrMissing(jsonfile)
  }
  else if (jsontype == "list"){
    list_UoM <- jsonlist[["UoM"]]
  }
  
  #get list of used properties
  if (jsontype == "file"){
    list_props <- read_json_mids_criteria(jsonfile, out = "properties")
  }
  else if (jsontype == "list"){
    list_props <- jsonlist[["properties"]]
  }
  
  #add other needed/interesting properties
  list_extra_props <- c("[dwc:Occurrence]dwc:datasetKey",
                        "[dwc:Occurrence]dwc:countryCode",
                        "[dwc:Occurrence]dwc:kingdom",
                        "[dwc:Occurrence]dwc:phylum", 
                        "[dwc:Occurrence]dwc:class",
                        "[dwc:Occurrence]dwc:order", 
                        "[dwc:Occurrence]dwc:family",
                        "[dwc:Occurrence]dwc:subfamily",
                        "[dwc:Occurrence]dwc:genus")
  
  # import from zipped DWC archive
  # and set unknown or missing values that apply to all to NA
  select_props = unique(c(list_props, list_extra_props))
  
  gbif_dataset = parse_data_file(filename = gbiffile,
                                 config = config,
                                 select_props = select_props,
                                 uom = list_UoM$all,
                                 session = session)
  
  #add missing columns with all values as NA
  list_props %<>% unique()
  missing <- c(list_props)[!c(list_props) %in% colnames(gbif_dataset)]
  gbif_dataset[, missing] <- NA
  
  # change unknown or missing values for specific columns to NA
  for (i in 1:length(list_UoM)){
    colname <- names(list_UoM[i])
    if (colname %in% names(gbif_dataset)){
      gbif_dataset %<>%
        mutate("{colname}" := na_if(gbif_dataset[[colname]], list_UoM[[i]]))
    }
  }
  
  gbif_dataset_mids <- gbif_dataset
  gbif_dataset <- NULL
    
  # Define criteria ---------------------------------------------------------
  
  # Get list of criteria
  if (jsontype == "file"){
    list_criteria <- read_json_mids_criteria(jsonfile)
  }
  else if (jsontype == "list"){
    list_criteria <- jsonlist[["criteria"]]
  }
  
  # Check if separate MIDS conditions are met -------------------------------
  
  #For each MIDS condition in the list, check if the criteria for that condition 
  #are TRUE or FALSE and add the results in a new column
  for (j in 1:length(list_criteria)){
    midslevel <- names(list_criteria[j])
    midscrit <- list_criteria[[j]]
    for (i in 1:length(midscrit)){
      columnname = paste0(midslevel,  names(midscrit[i]))
      gbif_dataset_mids %<>%
        mutate("{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
    }
  }
  
  # Calculate MIDS level ----------------------------------------------------
  
  #For each MIDS level, the conditions of that level and of lower levels all need to be true
  gbif_dataset_mids %<>%
    mutate(MIDS_level = case_when(
      apply(gbif_dataset_mids[ , grep("mids[0-3]", names(gbif_dataset_mids)), with = FALSE], MARGIN = 1, FUN = all) ~ 3,
      apply(gbif_dataset_mids[ , grep("mids[0-2]", names(gbif_dataset_mids)), with = FALSE], MARGIN = 1, FUN = all) ~ 2,
      apply(gbif_dataset_mids[ , grep("mids[0-1]", names(gbif_dataset_mids)), with = FALSE], MARGIN = 1, FUN = all) ~ 1,
      apply(gbif_dataset_mids[ , grep("mids0", names(gbif_dataset_mids)), with = FALSE], MARGIN = 1, FUN = all) ~ 0,
      TRUE ~ -1
    ))
  return(list(results = gbif_dataset_mids, missing = missing))
}
