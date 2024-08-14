parse_data_file <- function(filename,
                            config,
                            select_props,
                            uom) {
  if (config$app$format == "dwc-a") {
    return(parse_dwc_archive(filename,
                             config,
                             select_props,
                             uom))
  }
  if (config$app$format == "simple_dwc") {
    return(parse_dwc(filename,
                     select_props,
                     uom))
  }
}

read_data_from_dwca_file <- function(filename,
                                     meta,
                                     namespaces,
                                     select_props,
                                     uom,
                                     extension = NULL) {
  if (!is.null(extension)) {
    xpath = paste0("//extension[@rowType='",
                   extension,"']")
    
  } else {
    xpath = "//core"
  }
  
  core_terms = meta %>%
    xml_find_all(paste0(xpath,
                        "/field")) %>%
    xml_attrs() %>%
    tibble(t = .) %>%
    unnest_wider(t)
  
  for (i in 1:dim(namespaces)[1]) {
    core_terms %<>%
      mutate(term = gsub(namespaces$uri[i],
                         paste0(namespaces$name[i],":"),
                         term,
                         fixed=T))
    if (!is.null(extension)) {
      extension %<>% gsub(namespaces$uri[i],
                       paste0(namespaces$name[i],":"),
                       .,
                       fixed=T)
    }
  }
  
  if (is.null(extension)) {
    category = "[dwc:Occurrence]"
    idname = "/id"
  } else {
    category = paste0("[",extension,"]")
    idname = "/coreid"
  }
  
  id_index = meta %>%
    xml_find_all(paste0(xpath,idname)) %>%
    xml_attr("index") %>%
    as.numeric()
  
  core_select_props = select_props %>%
    tibble(data = .) %>%
    filter(grepl(category,data,fixed=T)) %>%
    pull(data) %>%
    gsub(category,"",.,fixed=T)
  
  if (length(core_select_props) == 0) {
    return(NULL)
  }
  
  drop = core_terms %>%
    filter(!term%in%core_select_props,!is.na(index),index!=id_index) %>%
    pull(index) %>%
    as.numeric() %>%
    map(~ .x + 1) %>%
    unlist()
  
  core_data <- fread(unzip(filename, xml_find_all(meta,paste0(xpath,"/files/location")) %>% xml_text()), 
                     encoding = xml_find_all(meta,xpath) %>% xml_attr("encoding"), 
                     sep = xml_find_all(meta,xpath) %>% xml_attr("fieldsTerminatedBy") %>% gsub("\\\\t","\t",.),
                     na.strings = uom, 
                     quote = xml_find_all(meta,xpath) %>% xml_attr("fieldsEnclosedBy"),
                     colClasses = 'character',
                     drop = drop)
  
  if ("default"%in%colnames(core_terms)) {
    defaults = core_terms %>%
      filter(!is.na(default))
    
    for (i in 1:dim(defaults)[1]) {
      newname = gsub(".*:","",defaults$term[i])
      core_data[[newname]] = defaults$default[i]
    }
  }
  return(core_data)
}

parse_dwc_archive <- function(filename,
                              config,
                              select_props,
                              uom) {
  meta = read_xml(unzip(filename,"meta.xml"))
  meta %>% xml_ns_strip()
  
  namespaces = read_yaml(paste0("../../",config$app$sssom_yml),
                         readLines.warn = F) %>%
    pluck("curie_map") %>%
    enframe(name="name",value="uri")
  
  data = read_data_from_dwca_file(filename = filename,
                                  meta = meta,
                                  namespaces = namespaces,
                                  select_props = select_props,
                                  uom = uom)
  
  core_id = meta %>%
    xml_find_all("//core/id") %>%
    xml_attr("index") %>%
    as.numeric()
  
  core_id = colnames(data)[core_id+1]
  
  len = 1
  step = 1
  while (len != 0) {
    node = xml_find_all(meta,paste0("(//extension)[",step,"]"))
    len = node %>% xml_length()
    if (len > 0) {
      temp_data = read_data_from_dwca_file(filename = filename,
                                           meta = meta,
                                           namespaces = namespaces,
                                           select_props = select_props,
                                           uom = uom,
                                           extension = node %>% xml_attr("rowType"))
      if (!is.null(temp_data)) {
        data = left_join(data,temp_data,by=c(core_id))
      }
    }
    step = step + 1
  }
  return(data)
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

strip_termname <- function(str) {
  str %<>%
    gsub(".*:","",.)
  return(str)  
}