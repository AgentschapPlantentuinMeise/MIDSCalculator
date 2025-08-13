# Run the correct function depending on data format
parse_data_file <- function(filename,
                            config,
                            select_props,
                            uom,
                            session) {
  # Darwin Core Archive (zipped)
  if (config$app$format == "dwc-a") {
    return(parse_dwc_archive(filename,
                             config,
                             select_props,
                             uom))
  }
  
  # Simple Darwin Core csv file
  if (config$app$format == "simple_dwc") {
    return(parse_dwc(filename,
                     select_props,
                     uom))
  }
  
  # ABCD zipped XMLs TBD
  if (config$app$format == "biocase") {
    return(parse_biocase_archive(filename,
                                 config,
                                 select_props,
                                 uom,
                                 session))
  }
}

# Function to read parts of a data file of a dwc archive
read_data_from_dwca_file <- function(filename, #path to the zip file
                                     meta, #meta.xml already read
                                     namespaces, #namespaces read from yml
                                     select_props, #props to import, derived from schema
                                     uom, #unknown/missing values for all props
                                     extension = NULL) { #the extension category
  # xpath to the core or extension node
  if (!is.null(extension)) {
    xpath = paste0("//extension[@rowType='",
                   extension,"']")
  } else {
    xpath = "//core"
  }
  
  # Retrieve all terms from the node and their attributes
  core_terms = meta %>%
    xml_find_all(paste0(xpath,
                        "/field")) %>%
    xml_attrs() %>%
    tibble(t = .) %>%
    unnest_wider(t)
  
  # Replace the full URI with the short namespace
  for (i in 1:dim(namespaces)[1]) {
    core_terms %<>%
      mutate(term = gsub(namespaces$uri[i],
                         paste0(namespaces$name[i],":"),
                         term,
                         fixed=T))
    # Replace this too in the extension rowType
    if (!is.null(extension)) {
      extension %<>% gsub(namespaces$uri[i],
                       paste0(namespaces$name[i],":"),
                       .,
                       fixed=T)
    }
  }
  
  # category namespace and id attribute name
  if (is.null(extension)) {
    category = "[dwc:Occurrence]"
    idname = "/id"
  } else {
    category = paste0("[",extension,"]")
    idname = "/coreid"
  }
  
  # index of the id field, for joining later
  id_index = meta %>%
    xml_find_all(paste0(xpath,idname)) %>%
    xml_attr("index") %>%
    as.numeric()
  
  # filter the properties of the core or extension
  core_select_props = select_props %>%
    tibble(data = .) %>%
    filter(grepl(category,data,fixed=T)) %>%
    pull(data) %>%
    gsub(category,"",.,fixed=T)
  
  # return null if no props from the schema are in this core/extension
  if (length(core_select_props) == 0) {
    return(NULL)
  }
  
  # find col index of all terms in the csv file that are not needed
  # also include the id field even if it's not mapped in the schema
  # add 1 as indexing in xml starts at 0, at 1 in R
  drop = core_terms %>%
    filter(!term%in%core_select_props,
           !is.na(index),
           index!=id_index) %>%
    pull(index) %>%
    as.numeric() %>%
    map(~ .x + 1) %>%
    unlist()
  
  if (length(drop) == dim(core_terms)[1]) {return(NULL)}
  
  # set names to replace the colnames of the csv file
  # only indexed fields (not with default value)
  newnames = core_terms %>%
    filter(!is.na(index),
           term%in%core_select_props) %>%
    pull(term) %>%
    paste0(category,.) 
  
  # add the id as a term for joining if not mapped to a
  # selected term
  
  # first identify the colname of the id (if listed in meta.xml)
  id_name = core_terms %>%
    mutate(term = paste0(category,term)) %>%
    filter(index==id_index) %>%
    pull(term)

  # add id if not listed in the xml or not a selected property
  # for the mids calculation
  if (length(id_name)==0||!id_name%in%newnames) {
    newnames %<>%
      c("id",.)
  }
  # read the data file from the zipped archive
  # take parameters from the meta.xml
  # replace some values with NA based on UoM
  unzipped_file = xml_find_all(meta,
                               paste0(xpath,"/files/location")) %>% 
    xml_text()
  core_data <- fread(unzip(filename, 
                           unzipped_file), 
                     encoding = xml_find_all(meta,xpath) %>% 
                       xml_attr("encoding"), 
                     sep = xml_find_all(meta,xpath) %>% 
                       xml_attr("fieldsTerminatedBy") %>% 
                       gsub("\\\\t","\t",.),
                     na.strings = uom, 
                     quote = xml_find_all(meta,xpath) %>% 
                       xml_attr("fieldsEnclosedBy"),
                     colClasses = 'character',
                     drop = drop)
  #delete the unzipped file after reading it
  file.remove(unzipped_file)
  # replace csv file colnames with full namespaces name
  colnames(core_data) = newnames
  
  # set default values from the term list, if any
  if ("default"%in%colnames(core_terms)) {
    defaults = core_terms %>%
      filter(!is.na(default))
    
    for (i in 1:dim(defaults)[1]) {
      newname = paste0(category,defaults$term[i])
      core_data[[newname]] = defaults$default[i]
    }
  }
  
  #collapse multiple values per coreid in extensions
  if (!is.null(extension)) {
    core_data %<>%
      summarise(
        across(everything(), 
               ~ { i <- match(TRUE, !is.na(.x), nomatch = NA_integer_); .x[i] }),
        .by = all_of("id")
      )
  }
  
  return(core_data)
}

# function to read an parse a dwc arcive (zipped)
parse_dwc_archive <- function(filename,
                              config,
                              select_props,
                              uom) {
  # read the meta.xml and strip the namespace for easier xpath
  meta = read_xml(unzip(filename,"meta.xml"))
  meta %>% xml_ns_strip()
  file.remove("meta.xml")
  # load namespaces of dwc, dc, ac... from the sssom yaml curie map
  ymlpath = list.files(paste0("../../data/sssom/",
                              config$app$standard,
                              "/",
                              config$app$discipline),
                       pattern = "*.yml",
                       full.names = T)
  
  namespaces = read_yaml(ymlpath,
                         readLines.warn = F) %>%
    pluck("curie_map") %>%
    enframe(name="name",value="uri")
  
  # read the data from the occurrence core
  data = read_data_from_dwca_file(filename = filename,
                                  meta = meta,
                                  namespaces = namespaces,
                                  select_props = select_props,
                                  uom = uom)
  
  # define the id on which to join with any extensions
  # +1 because xml indexing starts at 0, R at 1
  core_id = meta %>%
    xml_find_all("//core/id") %>%
    xml_attr("index") %>%
    as.numeric()
  core_id = colnames(data)[core_id+1]
  
  # check each extension and try to process it
  # occurrence extensions to the occurrence core are skipped
  # if any terms were found in an extension, they're joined into the core tibble
  len = 1
  step = 1
  while (len != 0) {
    node = xml_find_all(meta,paste0("(//extension)[",step,"]"))
    len = node %>% xml_length()
    extension_type = node %>% xml_attr("rowType")
    if (len > 0 && extension_type != "http://rs.tdwg.org/dwc/terms/Occurrence") {
      temp_data = read_data_from_dwca_file(filename = filename,
                                           meta = meta,
                                           namespaces = namespaces,
                                           select_props = select_props,
                                           uom = uom,
                                           extension = extension_type)
      if (!is.null(temp_data)) {
        extension_id = node %>%
          xml_find_all("//id") %>%
          xml_attr("index") %>%
          as.numeric()
        extension_id = colnames(temp_data)[extension_id+1]
        
        data = left_join(data,
                         temp_data,
                         by=setNames(extension_id,core_id))
      }
    }
    step = step + 1
  }
  return(select(data,-any_of("id")))
}

parse_dwc <- function(filename,
                      select_props,
                      uom) {
  
  gbif_dataset <- fread(filename, 
                        encoding = "UTF-8", 
                        na.strings = uom, 
                        quote="",
                        colClasses = 'character',
                        select = select_props)
}

parse_biocase_archive <- function(filename,
                                  config,
                                  select_props,
                                  uom,
                                  session) {
  # Read the lookup table from abcd term URI to its xpath
  xpath_mapper = fread("../../data/formats/abcd_xpaths.csv")
  
  # Remove the category namespace for matching on URI with abcd namespace only
  select_props %<>%
    gsub(".*]","",.)
  
  # List the mappings at the metadata level
  xpaths_meta = xpath_mapper %>%
    filter(uri%in%select_props,
           !grepl("/Units/",xpath,fixed=T)) %>%
    mutate(xpath = paste0("/",gsub("/","/abcd:",xpath,fixed=T)))
  
  # List the mappings at the Unit (specimen) level
  # Modify the xpath to always include the UnitGUID 
  #so to keep the connection between the term value and the specimen id
  xpaths_unit = xpath_mapper %>%
    filter(uri%in%select_props,
           grepl("/Units/",xpath,fixed=T),
           uri!="abcd:UnitGUID") %>%
    mutate(xpath = paste0("//abcd:DataSets/abcd:DataSet/abcd:Units/abcd:Unit/abcd:UnitGUID|/",
                          gsub("/","/abcd:",xpath,fixed=T)))
  
  # List all xml files in the archive
  unzip(filename,exdir = "temp_biocase")
  filelist = list.files("temp_biocase",
                        pattern = "*.xml",
                        full.names = T)
  
  # initiate a list for all results (per file)
  for (i in 1:length(filelist)) {
    # Read the xml file, strip the default namespaces
    file = read_xml(filelist[i])
    file %>% xml_ns_strip()
    
    # find all guids of specimens in this file
    guids = file %>%
      xml_find_all("//abcd:DataSets/abcd:DataSet/abcd:Units/abcd:Unit/abcd:UnitGUID") %>%
      xml_text()
    
    # set a new tibble to contain all the data
    newdf = tibble(`abcd:UnitGUID` = guids)
    if (exists("session")&!is.null(session)) {
      update_modal_spinner(
        paste0("Calculating MIDS results: ",
               i,
               " out of ",
               length(filelist),
               " XML files."), 
        session = session)
    } else {print(i)}
    
    # For each of the xpaths, find the corresponding values (if any)
    for (k in 1:dim(xpaths_unit)[1]) {
      val = xml_find_all(file,xpaths_unit$xpath[k])
      
      # if there are more results than just the GUIDs, link them to the
      #right guid and store them in pre-allocated lists 
      # to later join in the results tibble
      if (length(val) > length(guids)) {
        spec_guids = vector(length = length(val) - length(guids),
                            mode = "character")
        spec_values = vector(length = length(val) - length(guids),
                             mode = "character")
        step = 1
        for (j in 1:length(val)) {
          if (xml_name(val[[j]]) == "UnitGUID") {
            currentguid = val[[j]] %>%
              xml_text()
          } else if (spec_guids[1] == "" || 
                     spec_guids[step-1] != currentguid) {
            spec_guids[step] = currentguid
            spec_values[step] = val[[j]] %>% xml_text()
            step = step + 1
          }
        }
        # remove the unused cells from both vectors
        one_to_many = spec_guids%in%""
        spec_guids = spec_guids[!one_to_many]
        spec_values = spec_values[!one_to_many]
        newdf %<>% left_join(tibble(guid = spec_guids,
                                    !!xpaths_unit$uri[k] := spec_values),
                             by=setNames("guid",
                                         "abcd:UnitGUID"))
      }
    }
    # Concatenate the result list from each xml file into a single table
    if (!exists("resu")) {
      resu = newdf
    } else {
      resu = resu %>% 
        bind_rows(newdf)
    }
  }
  # set metadata level values
  # based on the last file read
  # metadata should be identical in each xml file in the archive
  for (i in 1:dim(xpaths_meta)[1]) {
    meta_value = xml_find_all(file,xpaths_meta$xpath[i]) %>%
      xml_text()
    if (length(meta_value) > 0) {
      resu %<>%
        mutate(!!xpaths_meta$uri[i] := meta_value)
    }
  }
  
  # add object_category to colnames for consistency in use by mids-calc
  oldcolnames = colnames(resu) %>%
    tibble(oldnames = .)
  
  tsvpath = list.files(paste0("../../data/sssom/",
                              config$app$standard,
                              "/",
                              config$app$discipline),
                       pattern = "*.tsv",
                       full.names = T)
  
  sssom = fread(tsvpath)
  
  oldcolnames %<>% 
    left_join(select(sssom,`sssom:object_id`,
                     `sssom:object_category`),
              by=setNames("sssom:object_id","oldnames")) %>%
    mutate(newnames = paste0("[",
                             `sssom:object_category`,
                             "]",
                             oldnames))
  colnames(resu) = oldcolnames$newnames
  unlink("temp_biocase",
         recursive = T)
  return(resu)
}