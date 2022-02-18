library(dplyr)
library(rlang)
library(jsonlite)


schema <- read_json("data/schemas/firstschema.json")


# works on one level at a time for now
crits_mids_one <- ""
c <- 0
for (condition_name in names(schema$mids1))
  {eval_tidy(condition_name)  
  c <- c + 1
  # get the contents of a single condition
  condition <- schema$mids1[[condition_name]][[1]]
  c2 <- 0
  if(condition$midsAchieved){
    # if midsAchieved == TRUE, then we should take these properties into account
    n <- length(condition$property)
    for (prop in condition$property){
      c2 <- c2+1
      print(c(prop, condition$midsAchieved)) #placeholder
      #do something with the properties (write to criteria?)
      if (c2 == 1){crits_mids_one <- paste0(crits_mids_one, "(")}
      crits_mids_one <- paste0(crits_mids_one, "!is.null(", prop, ")")
      #and check operator
      if ("operator" %in% names(condition) & c2 != n){
        if (condition$operator == "OR"){crits_mids_one <- paste0(crits_mids_one, " | ")}
        if (condition$operator == "AND"){crits_mids_one <- paste0(crits_mids_one, " & ")}
      }
      if (c2 == n){crits_mids_one <- paste0(crits_mids_one, ")")}
    }
  }
  else {
    # if midsAchieved == FALSE, then we check that these properties are not true
    for (prop in condition$property){
      print(c(prop, condition$midsAchieved)) #placeholder
      #check restriction
      #do something with the properties (write to criteria?)
      crits_mids_one <- paste0(crits_mids_one, "is.null(", prop, ")")
    }
  }
  n <- length(names(schema$mids1))
  if (c != n){crits_mids_one <- paste0(crits_mids_one, " & ")}
}

