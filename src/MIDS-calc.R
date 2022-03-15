library(dplyr)
library(data.table)
library(purrr)
library(magrittr)

# Load functions ----------------------------------------------------------
source(file = "src/parse_json_schema.R")

# Parameters --------------------------------------------------------------
zippath <- "data/0176996-210914110416597.zip"
# occpath <- "data/occurrence.txt"

# Get data ----------------------------------------------------------------

#from occurence.txt file
# gbif_dataset <- fread(occpath, encoding = "UTF-8", colClasses = "character")

#get unknown or missing values
list_UoM <- read_json_unknownOrMissing()

# import from zipped DWC archive
# and set unknown or missing values that apply to all to NA
gbif_dataset <- fread(unzip(zippath, "occurrence.txt"), 
                      encoding = "UTF-8", na.strings = list_UoM$all)

# change unknown or missing values for specific columns to NA
for (i in 1:length(list_UoM)){
  colname <- names(list_UoM[i])
  if (colname %in% names(gbif_dataset)){
    gbif_dataset %<>%
      mutate("{colname}" := na_if(gbif_dataset[[colname]], list_UoM[[i]]))
  }
}


# Get metadata (from zipped DWC archive) ------------------------------------------------------------

#Get filenames of metadata files
filenames <- unzip(zippath, list = TRUE)$Name %>% 
  grep("dataset/", ., value = TRUE)

#read xml files to get publication date out of metadata
pubdate <- data.table(datasetKey=character(), pubdate=character())
for (file in filenames){
  filename <- tools::file_path_sans_ext(basename(file))
  #extract pubdate, if it is not found it returns an emtpy list
  trydate <- XML::xmlRoot(XML::xmlParse(
    xml2::read_xml(unzip(zippath, file, exdir = tempfile()), 
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


# Add modified metadata to the dataset ------------------------------------

gbif_dataset_w_metadata <- left_join(gbif_dataset, pubdate, by = "datasetKey")

gbif_dataset_w_metadata %<>%
  mutate(modified = case_when( 
    is.na(modified) ~ pubdate,
    TRUE ~ as.character(modified)))


# Define criteria ---------------------------------------------------------

# Get list of criteria
list_criteria <- read_json_mids_criteria()


# Check if separate MIDS conditions are met -------------------------------

gbif_dataset_conditions <- gbif_dataset_w_metadata

#For each MIDS condition in the list, check if the criteria for that condition 
#are TRUE or FALSE and add the results in a new column
for (j in 1:length(list_criteria)){
  midsname <- names(list_criteria[j])
  midscrit <- list_criteria[[j]]
  for (i in 1:length(midscrit)){
    columnname = paste0(midsname,  names(midscrit[i]))
    gbif_dataset_conditions %<>%
      mutate("{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
  }
}

# Calculate MIDS level ----------------------------------------------------

#For each MIDS level, the conditions of that level and of lower levels all need to be true
gbif_dataset_mids <- gbif_dataset_conditions %>%
  mutate(mids_level = case_when(
    apply(gbif_dataset_conditions[ , grep("mids3|mids2|mids1|mids0", names(gbif_dataset_conditions)), with = FALSE], MARGIN = 1, FUN = all) ~ 3,
    apply(gbif_dataset_conditions[ , grep("mids2|mids1|mids0", names(gbif_dataset_conditions)), with = FALSE], MARGIN = 1, FUN = all) ~ 2,
    apply(gbif_dataset_conditions[ , grep("mids1|mids0", names(gbif_dataset_conditions)), with = FALSE], MARGIN = 1, FUN = all) ~ 1,
    apply(gbif_dataset_conditions[ , grep("mids0", names(gbif_dataset_conditions)), with = FALSE], MARGIN = 1, FUN = all) ~ 0
  ))

# Summary -----------------------------------------------------------------

#MIDS levels
gbif_dataset_mids %>%
  group_by(mids_level) %>%
  tally()  %>%
  mutate(perc = n / sum(n) *100)

#MIDS achieved per condition
n_rows <- nrow(gbif_dataset_conditions)
gbif_dataset_conditions[ , grep("mids", names(gbif_dataset_conditions)), with = FALSE] %>% 
  map(~{(sum(.x, na.rm = TRUE) / n_rows)*100})


# Export ------------------------------------------------------------------

fwrite(gbif_dataset_mids, file="data/processed/mids_output.csv")
