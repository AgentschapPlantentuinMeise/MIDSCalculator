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
  
  list_criteria <- list()
  #Loop trough sections
  n_sect <- length(names(schema))
  for (m in 1:n_sect){
    if (!grepl("mids", names(schema[m]), fixed = TRUE)){next} #only continue for "mids" sections
    #Get the contents of a section
    section <- schema[[m]] 
    #Loop through conditions
    n_cond <- length(names(section))
    for (i in 1:n_cond){ 
      crits <- ""
      condition_name = names(section)[i]
      # Loop trough subconditions, one of these should be true (|)
      n_subcond = length(section[[condition_name]]) 
      for (k in 1:n_subcond){
        #open brackets before the subcondition
        if (k == 1){crits <- paste0(crits, "(")}
        # get the contents (properties etc) of a single subcondition
        subcondition <- section[[condition_name]][[k]] 
        # Loop trough properties
        n_prop <- length(subcondition$property)
        for (j in 1 : n_prop){
          prop <- subcondition$property[[j]]
          if (is.null(prop)){break} #if there is no property, exit this loop iterating over props, still need to check later what to do when there is no property
          #open the brackets before the first property
          if (j == 1){crits <- paste0(crits, "(")}
          #open the brackets before each property
          crits <- paste0(crits, "(")
          #if there's isn't a NOT operator, add "!" so the property is not null/na
          if ("operator" %in% names(subcondition) && subcondition$operator != "NOT" || !("operator" %in% names(subcondition))){
            crits <- paste0(crits, "!")}
          #add the property (must be not null, or not na, check later how to formulate exactly)
          crits <- paste0(crits, "is.null(", prop, ")")
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
          #if there is a operator and it is not the last property, then add the matching operator to the string
          #currently does not work if there are subconditions without property! needs to be fixed 
          if ("operator" %in% names(subcondition) & j != n_prop){
            if (subcondition$operator == "OR"){crits <- paste0(crits, " | ")}
            if (subcondition$operator == "AND"){crits <- paste0(crits, " & ")}
          }
          #close the brackets for the last property
          if (j == n_prop){crits <- paste0(crits, ")")}
        }
        #Add | between subconditions
        if (k != n_subcond){crits <- paste0(crits, " | ")}
        #close brackets after subcondition
        else {crits <- paste0(crits, ")")}
      }
      #create nested list with criteria for each condition of each mids level
      list_criteria[[names(schema[m])]][[condition_name]] <- crits
    }
  }

  return(list_criteria)
}