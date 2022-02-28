
library(dplyr)
library(purrr)


file = "data/schemas/firstschema.json"

schema <- jsonlite::read_json(file)
schema_new <-
  jsonlite::read_json(file.path("data",
                                "schemas",
                                "secondschema_conditions_same_level.json"))
list_UoM
# Create list of unknownOrMissing values ----------------------------------

# just return this part of the schema, with midsAchieved is False
# purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE)

# Emulate the same output as Lynn

# prop names
purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
  keep(., imap_lgl(., ~!is_null(pluck(.x,"property")))) %>% 
  map_chr(~pluck(.x,"property"))

# prop values

purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
  keep(., imap_lgl(., ~!is_null(pluck(.x,"property")))) %>% 
  map_chr(~pluck(.x,"value"))


# all
purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
  keep(., imap_lgl(.,~is_null(pluck(.x,"property")))) %>% 
  map_chr(~pluck(.x,"value"))


# wrap in function
# extract_unknownOrMissing <- function(all= TRUE, property_present = FALSE){
#   negate = property_present # do we need to switch our is_null statement?
#   
#   purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
#     keep(., map_lgl(imap_lgl(.,is_null(pluck(.x,"property")))),~ifelse(negate,!.x,.x)) %>% 
#     map_chr(~pluck(.x,"value"))
#   
#   if(!all){
#     out_list <- list(purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
#            keep(., map_lgl(imap_lgl(.,is_null(pluck(.x,"property")))),~ifelse(negate,!.x,.x)) %>% 
#            map_chr(~pluck(.x,"value")))
#     names(out_list) <- 
#       purrr::keep(pluck(schema,"unknownOrMissing"),~.x$midsAchieved == FALSE) %>% 
#       keep(., map_lgl(imap_lgl(.,is_null(pluck(.x,"property")))),~ifelse(negate,!.x,.x)) %>% 
#       map_chr(~pluck(.x,"property"))
#   }
#   
# }



# seperate out mids elements only -----------------------------------------


# For every condition, read all properties, conditions should be collapsed with
# an AND, and properties with an OR



## get all conditions for a level ------------------------------------------

## get all properties for a single condition -------------------------------

#second mids level, second condition, first group of properties, get the values for properties
keep(schema_new,stringr::str_starts(names(schema_new),"mids")) %>% 
  pluck(2,2,1,"property")

# function to collapse a number of strings with a certain logical operator that
# is recoded to dplyr syntax
collapse_with_operator <- function(string_to_collapse,operator) {
  paste(string_to_collapse,
        collapse = dplyr::recode(
          operator,
          AND = "&",
          OR = "|",
          NOT = "!"
        ))
}

extract_group_of_operators <-
  function(schema,
           mids_level_index,
           condition_index,
           group_index) {
    keep(schema, stringr::str_starts(names(schema), "mids")) %>%
      pluck(mids_level_index, condition_index, group_index)
  }



group_of_properties <- 
  keep(schema_new,stringr::str_starts(names(schema_new),"mids")) %>% 
  pluck(2,2,1)

collapse_with_operator(string_to_collapse = pluck(group_of_properties, "property"),
                         operator = pluck(group_of_properties, "operator"))

collapse_with_operator(
  extract_group_of_operators(schema_new, 3, 2, 1)$property,
  extract_group_of_operators(schema_new, 3, 2, 1)$operator
)


# iterate over the mids level statements ----------------------------------



# return just the indexes -------------------------------------------------


