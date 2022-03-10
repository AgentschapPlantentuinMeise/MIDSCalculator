library(jsonlite)

read_json_criteria <- function(file = "data/schemas/secondschema_conditions_same_level.json") {


  # Read schema -------------------------------------------------------------
  
  schema <- read_json(file)
  
  
  # Create list of unknownOrMissing values ----------------------------------
  
  list_UoM <- list()
  #Loop trough the values
  n_values <- length(schema$unknownOrMissing)
  for (l in 1:n_values) {
    value = schema$unknownOrMissing[[l]]$value
    #only take into account values which do not count for mids
    if (schema$unknownOrMissing[[l]]$midsAchieved == FALSE) {
      #check if there is a property, otherwise it relates to all properties
      if ("property" %in% names(schema$unknownOrMissing[[l]])){
        prop <- schema$unknownOrMissing[[l]]$property
      } else {
        prop <- "all"
      }
      #add to list
      if (prop %in% names(list_UoM)){
        list_UoM[[prop]] <- append(list_UoM[[prop]], value)
      } else {
        list_UoM[prop] <- as.list(value)
        }
    }
  }
  
  
  # Construct criteria for mids levels --------------------------------------
  
  #only use mids sections
  midsschema <- schema[grep("mids", names(schema))]
  list_criteria <- list()
  #Loop trough sections
  for (sect_index in seq_along(names(midsschema))){
    #Get the contents of a section
    section <- midsschema[[sect_index]] 
    #Loop through conditions
    for (cond_index in seq_along(names(section))){ 
      crits <- ""
      condition_name = names(section)[cond_index]
      # Loop trough subconditions, one of these should be true (|)
      for (subcond_index in seq_along(section[[condition_name]])){
        # get the contents (properties etc) of a single subcondition
        subcondition <- section[[condition_name]][[subcond_index]] 
        #if operator is NOT, inverse all the criteria
        if ("operator" %in% names(subcondition) && subcondition$operator == "NOT"){
          crits <- paste0(crits, "!")}
        #open brackets before the subcondition
        if (subcond_index == 1){crits <- paste0(crits, "(")}
        # Loop trough properties
        for (prop_index in seq_along(subcondition$property)){
          prop <- subcondition$property[[prop_index]]
          if (is.null(prop)){break} #if there is no property, exit this loop iterating over props, still need to check later what to do when there is no property
          #open the brackets before the first property
          if (prop_index == 1){crits <- paste0(crits, "(")}
          #open the brackets before each property
          crits <- paste0(crits, "(")
          #add the property (must be not null, or not na, check later how to formulate exactly)
          crits <- paste0(crits, "!is.null(", prop, ")")
          #add restrictions from unknown or missing list
          for (group in names(list_UoM)){
              if (group == "all") {
                for (value in list_UoM[[group]]) {crits <- paste0(crits, " & ", prop, " != '", value, "'")}
              }
              else if (group == prop) {
                for (value in list_UoM[[group]]) {crits <- paste0(crits, " & ", prop, " != '", value, "'")}
              }
          }
          #close the brackets after each property
          crits <- paste0(crits, ")")
          #close the brackets for the last property
          if (prop_index == length(subcondition$property)){crits <- paste0(crits, ")")}
          #if there is a operator and it is not the last property, then add the matching operator to the string
          #currently does not work if there are subconditions without property! needs to be fixed 
          else if ("operator" %in% names(subcondition)){
            if (subcondition$operator == "OR"){crits <- paste0(crits, " | ")}
            if (subcondition$operator == "AND"){crits <- paste0(crits, " & ")}
          }
        }
        #Add | between subconditions
        if (subcond_index != length(section[[condition_name]])){crits <- paste0(crits, " | ")}
        #close brackets after subcondition
        else {crits <- paste0(crits, ")")}
      }
      #create nested list with criteria for each condition of each mids level
      list_criteria[[names(midsschema[sect_index])]][[condition_name]] <- crits
    }
  }

  return(list_criteria)
}