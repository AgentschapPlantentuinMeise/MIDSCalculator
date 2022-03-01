library(dplyr)
library(data.table)

# Load functions ----------------------------------------------------------
source(file = "src/parse_json_schema.R")

# Get data ----------------------------------------------------------------

gbif_dataset <- fread("data/occurrence.txt", encoding = "UTF-8", colClasses = "character")


# Define criteria ---------------------------------------------------------

# Get list of criteria
list_criteria <- read_json_criteria()


# Check if separate MIDS conditions are met -------------------------------

gbif_dataset_conditions <- gbif_dataset

for (j in 1:length(list_criteria)){
  midsname <- names(list_criteria[j])
  midscrit <- list_criteria[[j]]
  for (i in 1:length(midscrit)){
    columnname = paste0(midsname,  names(midscrit[i]))
    gbif_dataset_conditions <- mutate(gbif_dataset_conditions, 
                                      "{columnname}" := !!rlang::parse_expr(midscrit[[i]]))
  }
}

# Calculate MIDS level ----------------------------------------------------

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
