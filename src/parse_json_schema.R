library(dplyr)
library(rlang)
library(jsonlite)


schema <- read_json("data/schemas/firstschema.json")


# works on one mids level at a time for now

crits_mids_one <- ""
#Loop through conditions, these should be all be true (&)
n_cond <- length(names(schema$mids2))
for (i in 1:n_cond) 
  {condition_name = names(schema$mids2)[i]
  #open brackets before the condition
  if (i == 1){crits_mids_one <- paste0(crits_mids_one, "(")}
  # Loop trough subconditions, one of these should be true (|)
  n_subcond = length(schema$mids2[[condition_name]]) 
  for (k in 1:n_subcond){
    #open brackets before the subcondition
    if (k == 1){crits_mids_one <- paste0(crits_mids_one, "(")}
    # get the contents (properties etc) of a single subcondition
    subcondition <- schema$mids2[[condition_name]][[k]] 
    # Loop trough properties
    n_prop <- length(subcondition$property)
    for (j in 1 : n_prop){
      prop <- subcondition$property[[j]]
      print(c(prop, subcondition$midsAchieved)) #placeholder
      #open the brackets before the first property
      if (j == 1){crits_mids_one <- paste0(crits_mids_one, "(")}
      #if there's isn't a NOT operator, add "!" so the property is not null/na
      if ("operator" %in% names(subcondition) && subcondition$operator != "NOT" | !("operator" %in% names(subcondition))){
        crits_mids_one <- paste0(crits_mids_one, "!")}
      #add the property (must be not null, or not na, check later how to formulate exactly)
      crits_mids_one <- paste0(crits_mids_one, "is.null(", prop, ")")
      #if there is a operator and it is not the last property, then add the matching operator to the string
      if ("operator" %in% names(subcondition) & j != n_prop){
        if (subcondition$operator == "OR"){crits_mids_one <- paste0(crits_mids_one, " | ")}
        if (subcondition$operator == "AND"){crits_mids_one <- paste0(crits_mids_one, " & ")}
      }
      #close the brackets for the last property
      if (j == n_prop){crits_mids_one <- paste0(crits_mids_one, ")")}
    }
    #Add | between subconditions
    if (k != n_subcond){crits_mids_one <- paste0(crits_mids_one, " | ")}
    #close brackets after subcondition
    else {crits_mids_one <- paste0(crits_mids_one, ")")}
  }
  #Add & between conditions
  if (i != n_cond){crits_mids_one <- paste0(crits_mids_one, " & ")}
  #close brackets after condition
  else {crits_mids_one <- paste0(crits_mids_one, ")")}
}

