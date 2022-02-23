library(dplyr)
library(data.table)


# Get data ----------------------------------------------------------------

gbif_dataset <- fread("data/occurrence.txt", encoding = "UTF-8")


# Define criteria ---------------------------------------------------------

# Get list of criteria
list_criteria <- read_json_criteria()

#level zero
crits_mids_zero <- "!is.null(catalogNumber) & !is.null(institutionCode) & !is.null(modified)"
#crits_mids_zero <- list_criteria$mids0 doesn't work yet because of subcond with xpath instead op property

#level one
crits_mids_one <- list_criteria$mids1 #works
  
#level two
crits_mids_two <- "(is.na(countryCode) & !is.na(locality)|
                  (!is.na(decimalLatitude)&!is.na(decimalLongitude)))&
                  (!is.na(recordedBy)|!is.na(recordedByID))&
                  !is.na(eventDate)|!is.na(year)|!is.na(verbatimEventDate)"
#crits_mids_two <- list_criteria$mids2 doesn't work yet because of the empty property under deposited + verbatimCoordinates and country are not columns of the gbif dataset + error "x character string is not in a standard unambiguous format"

#level three
crits_mids_three <- list_criteria$mids3 #works


# Calculate MIDS level ----------------------------------------------------

gbif_dataset_mids <- gbif_dataset %>%
  mutate(mids_level = case_when(
        !!rlang::parse_expr(crits_mids_three) & !!rlang::parse_expr(crits_mids_two) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 3,
        !!rlang::parse_expr(crits_mids_two) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 2,
        !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 1,
        !!rlang::parse_expr(crits_mids_zero) ~ 0
  ))

# Summary -----------------------------------------------------------------

gbif_dataset_mids %>%
  group_by(mids_level) %>%
  tally()  %>%
  mutate(perc = n / sum(n) *100)
