library(dplyr)
library(data.table)
library(purrr)
library(magrittr)

# Parameters --------------------------------------------------------------
# zippath <- "data/0176996-210914110416597.zip"
# jsonpath <- "data/schemas/secondschema_conditions_same_level.json"
# occpath <- "data/occurrence.txt"

# Load functions ----------------------------------------------------------
# source(file = "src/parse_json_schema.R")

calculate_mids <- function(gbiffile = zippath, jsonfile = jsonpath, jsontype = "file", jsonlist = NULL) {

  # Get data ----------------------------------------------------------------
  
  #from occurence.txt file
  # gbif_dataset <- fread(occpath, encoding = "UTF-8", colClasses = "character")
  
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
  list_props <- c(list_props, "datasetKey", "countryCode", "class", "order", "family", "subfamily", "genus")
  
  # import from zipped DWC archive
  # and set unknown or missing values that apply to all to NA
  gbif_dataset <- fread(unzip(gbiffile, "occurrence.txt"), 
                        encoding = "UTF-8", na.strings = list_UoM$all, 
                        select = list_props)
  
  # change unknown or missing values for specific columns to NA
  for (i in 1:length(list_UoM)){
    colname <- names(list_UoM[i])
    if (colname %in% names(gbif_dataset)){
      gbif_dataset %<>%
        mutate("{colname}" := na_if(gbif_dataset[[colname]], list_UoM[[i]]))
    }
  }
  
  # Get metadata (from zipped DWC archive) ------------------------------------------------------------
  
  #which datasets have records that don't have the modified property
  keys <- filter(gbif_dataset, is.na(modified)) %>% .$datasetKey %>% unique()
  #get paths for those datasets
  if (!is_empty(keys)){
    filenames <- paste0("dataset/", keys, ".xml")}
  
  #read xml files to get publication date out of metadata
  pubdate <- data.table(datasetKey=character(), pubdate=character())
  if(exists("filenames")){
    for (file in filenames){
      filename <- tools::file_path_sans_ext(basename(file))
      #extract pubdate, if it is not found it returns an emtpy list
      trydate <- XML::xmlRoot(XML::xmlParse(
        xml2::read_xml(unzip(gbiffile, file, exdir = tempfile()), 
                       encoding = "UTF-8"))) %>%
        XML::xmlElementsByTagName("pubDate", recursive = TRUE) 
      #if there is a date, add it to the list
      if(length(trydate) != 0){
        date <- 
          trydate %>%
          .[[1]] %>% 
          XML::xmlValue() %>% 
          trimws()
      
      } else {date <- NA}
      pubdate <- rbind(pubdate, list(filename, date))
    }
  }
  
  # Add modified metadata to the dataset ------------------------------------
  
  gbif_dataset_mids <- left_join(gbif_dataset, pubdate, by = "datasetKey")
  gbif_dataset <- NULL
  
  gbif_dataset_mids %<>%
    mutate(modified = case_when( 
      is.na(modified) ~ pubdate,
      TRUE ~ as.character(modified)))
  
  #don't need pubdate column anymore
  gbif_dataset_mids$pubdate <- NULL
  
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
    midsname <- names(list_criteria[j])
    midscrit <- list_criteria[[j]]
    for (i in 1:length(midscrit)){
      columnname = paste0(midsname,  names(midscrit[i]))
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
  return(gbif_dataset_mids)
}
