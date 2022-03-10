library(dplyr)
library(data.table)
library(purrr)

# Load functions ----------------------------------------------------------
source(file = "src/parse_json_schema.R")


# Parameters --------------------------------------------------------------

zippath <- "data/0176996-210914110416597.zip"
occpath <- "data/occurrence.txt"

# Get data ----------------------------------------------------------------

#from occurence.txt file
gbif_dataset <- fread(occpath, encoding = "UTF-8", colClasses = "character")


#from zipped DWC archive
gbif_dataset <- fread(unzip(zippath, "occurrence.txt"), 
                      encoding = "UTF-8", colClasses = "character")


# Get metadata (from zipped DWC archive) ------------------------------------------------------------

#Get filenames of metadata files
filenames <- unzip(zippath, list = TRUE)$Name %>% 
                  grep("dataset/", ., value = TRUE)

#read xml files to get publication date out of metadata
pubdate <- list()
for (file in filenames){
  filename <- tools::file_path_sans_ext(basename(file))
  #extract pubdate, if it is not found it returns an emtpy list
  trydate <- XML::xmlRoot(XML::xmlParse(
              xml2::read_xml(unzip(zippath, file, exdir = tempfile()), 
              encoding = "UTF-8"))) %>%
              XML::xmlElementsByTagName("pubDate", recursive = TRUE) 
  #if there is a date, add it to the list
  if(length(trydate) != 0){
    pubdate[[filename]] <- 
        trydate %>%
        .[[1]] %>% 
        XML::xmlValue() %>% 
        trimws()
  } else {pubdate[[filename]] <- ""}
}

# Define criteria ---------------------------------------------------------

# Get list of criteria
list_criteria <- read_json_criteria()


# Check if separate MIDS conditions are met -------------------------------

gbif_dataset_conditions <- gbif_dataset

#For each MIDS condition in the list, check if the criteria for that condition 
#are TRUE or FALSE and add the results in a new column
for (j in 1:length(list_criteria)){
  midsname <- names(list_criteria[j])
  midscrit <- list_criteria[[j]]
  for (i in 1:length(midscrit)){
    columnname = paste0(midsname,  names(midscrit[i]))
    gbif_dataset_conditions <- mutate(gbif_dataset_conditions, 
                          "{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
    #If modified is false, we look if there is a date in the metadata
    if (names(midscrit[i]) == "Modified") {
      for (nrec in 1:nrow(gbif_dataset_conditions)){
        if (gbif_dataset_conditions[[columnname]][nrec] == FALSE) {
          #assign the date from the metadata to the modified column
          gbif_dataset_conditions$modified[nrec] <- pubdate[[gbif_dataset_conditions$datasetKey[nrec]]]
          #test again if the criteria are met
          gbif_dataset_conditions <- mutate(gbif_dataset_conditions, 
                                            "{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
        }
      }
     
    }
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

write.csv(gbif_dataset_mids, file="data/processed/mids_output.csv")
