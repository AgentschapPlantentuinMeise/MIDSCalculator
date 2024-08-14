library(jsonlite)
library(yaml)
library(dplyr)

read_json_unknownOrMissing <- function(schema = default_schema, type = "file",config) {


  # Read schema -------------------------------------------------------------
  if (type == "sssom") {
    schema = parse_sssom(config = config)
  } else if (type == "file"){
    schema <- read_json(schema)
  } else if (type == "interactive") {
    schema <- parse_json(schema)
  }
  
  # Create list of unknownOrMissing values ----------------------------------
  
  list_UoM <- list()
  #Loop trough the values
  n_values <- length(schema$unknownOrMissing)
  for (l in 1:n_values) {
    value = schema$unknownOrMissing[[l]]$value[[1]]
    #only take into account values which do not count for mids
    if (schema$unknownOrMissing[[l]]$midsAchieved == FALSE) {
      #check if there is a property, otherwise it relates to all properties
      if ("property" %in% names(schema$unknownOrMissing[[l]])){
        prop <- schema$unknownOrMissing[[l]]$property[[1]]
      } else {
        prop <- "all"
      }
      #add to list
      if (prop %in% names(list_UoM)){
        list_UoM[[prop]] <- append(list_UoM[[prop]], value)
      } else {
        list_UoM[[prop]] <- value
        }
    }
  }
  return(list_UoM)
}
  
read_json_mids_criteria <- function(schema = default_schema,
                                    outtype = "criteria", type = "file",config) {
  
  # Read schema -------------------------------------------------------------
  if (type == "sssom") {
    schema = parse_sssom(config = config)
  } else if (type == "file"){
    schema <- read_json(schema)
  } else if (type == "interactive") {
    schema <- parse_json(schema)
  }
  # Construct criteria for mids levels --------------------------------------
  #only use mids sections
  midsschema <- schema[grep("mids", names(schema))]
  list_criteria <- list()
  list_props <- character()
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
          #make a list of properties
          list_props <- append(list_props, prop)
          #add the property is not na
          crits <- paste0(crits, "!is.na(", prop, ")")
          #if there is a operator and it is not the last property, then add the matching operator to the string
          #currently does not work if there are subconditions without property! needs to be fixed 
          if (prop_index != length(subcondition$property) & "operator" %in% names(subcondition)){
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
  if (outtype == "criteria"){
    return(list_criteria)
  }
  else if (outtype == "properties"){
    return(list_props)
  }
}

parse_sssom <- function(tsv=NULL,yml=NULL,config) {
  
  ##read the sssom tsv and yml files provided in config.ini
  #use function arguments to maybe later include support for 
  #loading them through the UI
  
  if (is.null(tsv)) {
    tsv = fread(paste0("../../",config$app$sssom_tsv),
                encoding="UTF-8",
                quote="",
                colClasses='character')
  }
  if (is.null(yml)) {
    yml = read_yaml(paste0("../../",config$app$sssom_yml),
                    readLines.warn=F)
  }
  
  #The schema is hardcoded for GBIF-annotated DwC-Archives
  #so exclude terms from extensions such as GBIF multimedia or Auddubon
  #tsv %<>%
  #  filter(`sssom:object_category`=="dwc:Occurrence")
  
  #initiate a schema with metadata from the yml file
  newschema = list()
  newschema$schemaName = yml$mapping_set_title
  newschema$schemaVersion = yml$mapping_set_version
  newschema$date = yml$mapping_date
  newschema$schemaType = "SSSOM-converted"
  
  #unknownOrMissing section based on RegexRemoval
  if (!is.null(tsv$`semapv:RegexRemoval`)) {
    #split the |-separated values to exclude
    #data.table syntax
    unknown_or_missing <- tsv[, .(object_id = `sssom:object_id`,
                                  object_category = `sssom:object_category`,
                                  RegexRemoval = unlist(tstrsplit(`semapv:RegexRemoval`, 
                                                                  "\\|"))), 
                              by = .(`sssom:object_id`,`sssom:object_category`)]
    
    #list unique values to be excluded
    check_all = count(unknown_or_missing,RegexRemoval) %>%
      filter(!is.na(RegexRemoval))
    
    #values to globally be excluded (across all mappings)
    check_all_globals = filter(check_all,n==dim(tsv)[1])
    
    #values to exclude only for specific mappings
    check_all_specifics = filter(check_all,n<dim(tsv)[1])
    
    #list mappings for which specific values should be excluded
    #omit the namespace as the schema does not use it
    check_all_specifics %<>%
      left_join(select(unknown_or_missing,
                       RegexRemoval,
                       object_id,
                       object_category),
                by=c("RegexRemoval"="RegexRemoval")) %>%
      filter(!duplicated(object_id)) %>%
      mutate(object_id = paste0("[",object_category,"]",object_id)) %>%
      select(-object_category)
      #mutate(object_id = sub(".*:","",object_id))
    
    #inititate unknownOrMissing section
    newschema$unknownOrMissing = list()
    
    #add global excluded values
    for (i in 1:dim(check_all_globals)[1]) {
      newschema$unknownOrMissing[[i]] = list(value = check_all_globals$RegexRemoval[i],
                                             midsAchieved = F)
    }
    
    #add specific excluded values
    for (j in 1:dim(check_all_specifics)[1]) {
      i = i + 1
      newschema$unknownOrMissing[[i]] = list(value = check_all_specifics$RegexRemoval[j],
                                             midsAchieved = F,
                                             property = check_all_specifics$object_id[j])
    }
  }
  
  #Add mids levels criteria
  for (i in 0:3) {
    #initiate mids level
    level = paste0("mids",i)
    newschema[[level]] = list()
    
    #all mappings for this mids level
    mids_crits = tsv %>%
      filter(`sssom:subject_category`==level)
    
    #unique MIDS elements per level
    #without namespace
    mids_elements = mids_crits %>%
      filter(!duplicated(`sssom:subject_id`)) %>%
      mutate(subject_id = sub(".*:","",`sssom:subject_id`))
    
    #for each element in this level, add mappings
    for (j in 1:dim(mids_elements)[1]) {
      #initiate element
      newschema[[level]][[mids_elements$subject_id[j]]] = list()
      
      #mappings for this element
      mids_crits_element = mids_crits %>%
        filter(`sssom:subject_id` == mids_elements$`sssom:subject_id`[j])
      
      #step counter for each set of mappings (with the same operator) per element
      k = 1
      
      ##narrowmatch -> OR operator
      narrowmatch = filter(mids_crits_element,
                           `sssom:predicate_id`=="skos:narrowMatch") %>%
        mutate(`sssom:object_id` = paste0("[",
                                          `sssom:object_category`,
                                          "]",
                                          `sssom:object_id`)) %>%
        pull(`sssom:object_id`) %>%
        as.list()
      if (length(narrowmatch)>0) {
        #because of the extension filter previously, some narrowMatches
        #can now be exactMatch instead
        newschema[[level]][[mids_elements$subject_id[j]]][[k]] = list(
          property = narrowmatch,
          operator = "OR"
        )
        k = k + 1
      }
      
      ##exact matches
      exactmatch = filter(mids_crits_element,
                          `sssom:predicate_id`=="skos:exactMatch") %>%
        mutate(`sssom:object_id` = paste0("[",
                                          `sssom:object_category`,
                                          "]",
                                          `sssom:object_id`)) %>%
        pull(`sssom:object_id`)
      if (length(exactmatch)>0) {
        newschema[[level]][[mids_elements$subject_id[j]]][[k]] = list(
          property = exactmatch
        )
        k = k + 1
      }
      
      #intersectionOf -> AND operator
      #which ones can be found in object_match_field
      intersectionof = filter(mids_crits_element,
                              `sssom:predicate_id`=="owl:intersectionOf",
                              `sssom:object_match_field`!="")
      if (dim(intersectionof)[1]>0) {
        for (l in 1:dim(intersectionof)[1]) {
          #note that the spaces + | delimitation is hardcoded here!
          intersects = strsplit(intersectionof$`sssom:object_match_field`[l],
                                split=" | ",
                                fixed=T)[[1]]
          for (m in 1:length(intersects)) {
            match_field = mids_crits_element %>%
              filter(`sssom:object_id`==intersects[m]) %>%
              slice_head()
            intersects[m] = paste0("[",
                                   match_field$`sssom:object_category`,
                                   "]",
                                   intersects[m])
          }
          intersects %<>%
            as.list()
          newschema[[level]][[mids_elements$subject_id[j]]][[k]] = list(
            property = intersects,
            operator = "AND"
          )
          k = k + 1
        }
      }
    }
  }
  #print(newschema)
  return(newschema)
}