library(dplyr)
library(data.table)


# Get data ----------------------------------------------------------------

gbif_dataset <- fread("data/occurrence.txt", encoding = "UTF-8")


# Define criteria ---------------------------------------------------------

#level zero
crits_mids_zero <- "!is.null(catalogNumber) & !is.null(institutionCode) & !is.null(modified)"

#level one
#also needs materialtype and objecttype
crits_mids_one <- "!is.null(scientificName)|!is.null(organismName)|!is.null(vernacularName) &
                    !is.null(preparations)"

#level two minimal
#current: (country and locality,  or coordinates) + collector + collection date (year at the least)
crits_mids_two <- "(is.na(countryCode) & !is.na(locality)|
                  (!is.na(decimalLatitude)&!is.na(decimalLongitude)))&
                  (!is.na(recordedBy)|!is.na(recordedByID))&
                  !is.na(eventDate)|!is.na(year)|!is.na(verbatimEventDate))"

#level two strict
#including most elements under consideration as required
crits_mids_two_strict <- "!is.na(continent) &
                          !is.na(countryCode) &
                          !is.na(stateProvince) &
                          !is.na(county) &
                          !is.na(locality) &
                          !is.na(decimalLatitude) &
                          !is.na(decimalLongitude) &
                          (!is.na(recordedBy)|!is.na(recordedByID))&
                          !is.na(recordNumber) &
                          !is.na(collectionID) &
                          !is.na(typeStatus) &
                          !is.na(eventDate)"

#level three
crits_mids_three <- "!is.na(na_if(mediaType, '')) & is.na(issue)" #mediatype contains empty strings



# Calculate MIDS level ----------------------------------------------------

gbif_dataset_mids <- gbif_dataset %>%
  mutate(mids_level = case_when(
        !!rlang::parse_expr(crits_mids_three) & !!rlang::parse_expr(crits_mids_two) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 3,
        !!rlang::parse_expr(crits_mids_two) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 2,
        !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 1,
        !!rlang::parse_expr(crits_mids_zero) ~ 0
  ))

gbif_dataset_mids <- gbif_dataset_mids %>%
  mutate(mids_level_strict = case_when(
    !!rlang::parse_expr(crits_mids_three) & !!rlang::parse_expr(crits_mids_two_strict) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 3,
    !!rlang::parse_expr(crits_mids_two_strict) & !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 2,
    !!rlang::parse_expr(crits_mids_one) & !!rlang::parse_expr(crits_mids_zero) ~ 1,
    !!rlang::parse_expr(crits_mids_zero) ~ 0
  ))

# Summary -----------------------------------------------------------------

gbif_dataset_mids %>%
  group_by(mids_level) %>%
  tally()  %>%
  mutate(perc = n / sum(n) *100)

gbif_dataset_mids %>%
  group_by(mids_level_strict) %>%
  tally()  %>%
  mutate(perc = n / sum(n) *100)
