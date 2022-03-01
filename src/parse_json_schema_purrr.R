# Script to demonstrate the usage of purrr for parse_json_schema.R to simplify
# and avoid some nexted for loops

# @PietrH, this is a draft, DO NOT MERGE


# load libraries ----------------------------------------------------------


library(dplyr)
library(purrr)


# load input data ---------------------------------------------------------


schema_new <-
  jsonlite::read_json(file.path(
    "data",
    "schemas",
    "secondschema_conditions_same_level.json"
  ))


# Create list of unknownOrMissing values ----------------------------------

# inspect unkown of missing values as they currently are
# list_UoM

# just return this part of the schema, with midsAchieved is False
purrr::keep(pluck(schema, "unknownOrMissing"), ~ .x$midsAchieved == FALSE)

# We could generate the same output as Lynn, some examples:

# prop names
purrr::keep(pluck(schema, "unknownOrMissing"), ~ .x$midsAchieved == FALSE) %>%
  keep(., imap_lgl(., ~ !is_null(pluck(.x, "property")))) %>%
  map_chr(~ pluck(.x, "property"))

# prop values

purrr::keep(pluck(schema, "unknownOrMissing"), ~ .x$midsAchieved == FALSE) %>%
  keep(., imap_lgl(., ~ !is_null(pluck(.x, "property")))) %>%
  map_chr(~ pluck(.x, "value"))


# all
purrr::keep(pluck(schema, "unknownOrMissing"), ~ .x$midsAchieved == FALSE) %>%
  keep(., imap_lgl(., ~ is_null(pluck(.x, "property")))) %>%
  map_chr(~ pluck(.x, "value"))


# wrap in function, incomplete.
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

# get just the mids statements part of the schema
mids_statements <-
  keep(schema_new, stringr::str_starts(names(schema_new), "mids"))


# For every condition, read all properties, conditions should be collapsed with
# an AND, and properties with an OR



## get all conditions for a level ------------------------------------------

## get all properties for a single condition -------------------------------

# second mids level, second condition, first group of properties, get the values
# for properties
keep(schema_new, stringr::str_starts(names(schema_new), "mids")) %>%
  pluck(2, 2, 1, "property")

# function to collapse a number of strings with a certain logical operator that
# is recoded to dplyr syntax
collapse_with_operator <- function(string_to_collapse, operator) {
  # capture a missing operator
  operator <- ifelse(is_null(operator), "no_operator", operator)
  ifelse(
    operator == "NOT",
    paste0("!", string_to_collapse),
    paste(
      string_to_collapse,
      collapse = dplyr::recode(
        operator,
        AND = "&",
        OR = "|",
        NOT = "!",
        no_operator = ""
      )
    )
  )
}

# function to collapse properties and return the operator in a single object.
extract_group_of_operators <-
  function(schema,
           mids_level_index,
           condition_index,
           group_index) {
    keep(schema, stringr::str_starts(names(schema), "mids")) %>%
      pluck(mids_level_index, condition_index, group_index)
  }


# Using pluck to extract specific elements
group_of_properties <-
  keep(schema_new, stringr::str_starts(names(schema_new), "mids")) %>%
  pluck(2, 2, 1)

collapse_with_operator(
  string_to_collapse = pluck(group_of_properties, "property"),
  operator = pluck(group_of_properties, "operator")
)

# Example of the above functions in action
collapse_with_operator(
  extract_group_of_operators(schema_new, 3, 2, 1)$property,
  extract_group_of_operators(schema_new, 3, 2, 1)$operator
)


# iterate over the mids level statements ----------------------------------

# we need the number of mids levels
seq_along(mids_statements)
# we need the number of conditions per mids level
seq_along(mids_statements) %>%
  map(~ seq_along(pluck(mids_statements, .x)))
# we need the number of subconditions per condition
map(seq_along(mids_statements), function(n) {
  map(pluck(map(
    seq_along(mids_statements), ~ seq_along(pluck(mids_statements, .x))
  ), n), ~ seq_along(pluck(mids_statements, n, .x)))
})
# then we finally need the number of properties per subcondition


# function to extract statements for specific mids level
extract_mids_statements <-
  function(mids_level, schema = schema_new) {
    crossed <- cross(list(
      mids_level_index = c(mids_level + 1), # use index instead of level
      # these are just filler variables, and should either be set high, or
      # calculated exactly as above.
      condition_index = c(1:5),
      group_index = c(1:2)
    )) %>%
      map(~ invoke(extract_group_of_operators, ., schema = schema)) %>%
      # drop empty elements, so if number of conditions and groups is too high,
      # we drop empty conditions here.
      compact()
    # generate all property statements

    map(
      seq_along(crossed),
      function(i) {
        collapse_with_operator(
          imap(crossed, ~ pluck(crossed, .y, "property"))[[i]],
          imap(crossed, ~
          pluck(crossed, .y, "operator"))[[i]]
        )
      }
    )
  }

# above function in action
extract_mids_statements(3)

# return just the indexes -------------------------------------------------

# example of just returning indexes of conditions of all mids statements.
seq_along(mids_statements) %>%
  map(~ seq_along(pluck(mids_statements, .x)))
