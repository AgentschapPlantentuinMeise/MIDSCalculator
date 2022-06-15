InteractiveSchemaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("interactiveschema"), "Edit interactively"),
    
    #css
    tags$style(
      HTML("
       .rank-list-container.custom-sortable {
         background-color: rgb(40, 116, 166, 0.8); color: white; font-size: 20px;
       }
       .custom-sortable .rank-list-item {
         background-color: #f5faff; color: #1A5276; font-size: 15px;
       }
       .ranklists {
         display: grid; grid-template-columns: 50% 50%; gap: 20px; padding: 20px;
       }",
       paste0('#', ns("interactivemodal")),".modal-dialog{
         width: 90%;
       }
       .rank-list-container.custom-sortable.unused {
         background-color: LightSlateGrey
       }
     ")
    ),
    
    #Interactive editing of MIDS implementation in modal window
    bsModal(ns("interactivemodal"), "Edit MIDS implementation interactively", ns("interactiveschema"), 
      downloadButton(ns("download"), "Download edited schema"),
      tabsetPanel(type = "tabs",
        tabPanel("Criteria",
           fluidRow(
             column(
               tags$h1(tags$span("MIDS criteria",
               actionButton(ns("info_criteria"), icon("info"),
                            style = "padding:5px 5px 20px 5px; font-size:40%; border-style: none"))),
               width = 12,
                 "Drag the MIDS elements to the desired MIDS level. Click the edit button to change the mappings of a MIDS element.",
                 br(),
                 div(class = "ranklists",
                   uiOutput(ns("crit")),
                   uiOutput(ns("crit2"))
                 ),
               div(
                 fluidPage(fluidRow(
                   column(6, offset = 3, 
                          textInput(ns("newElement"), "Enter a new MIDS element",
                                       value = "Enter text..."),
                          actionButton(ns("addElement"), "Add"))
                 ))
               )
             )
           ),
        ),
        tabPanel("Unknown or Missing values",
           fluidRow(
             column(
               tags$span(tags$h1("MIDS unknown or missing values",
               actionButton(ns("info_UoM"), icon("info"),
                            style = "padding:5px 5px 20px 5px; font-size:40%; border-style: none"))),
               width = 12,
               div(
                 class = "bucket-list-container default-sortable",
                 "Drag the unknown or missing values to the desired properties",
                 
                 div(
                   class = "ranklists",
                   uiOutput(ns("UoM")),
                   uiOutput(ns("UoM2"))
                 ),
                 fluidPage(fluidRow(
                   column(6, uiOutput(ns("newPropUoM"))),
                   column(6, textInput(ns("newValueUoM"), "Enter a new value",
                                       value = "Enter text..."))
                 )),
                 fluidPage(fluidRow(
                   column(6, actionButton(ns("addUoMprop"), "Add")),
                   column(6, actionButton(ns("addUoM"), "Add"))
                 ))
               )
             )
           )
        ))
    )
  )
}

InteractiveSchemaServer <- function(id, jsonschema, jsonUoM, disable) {
  moduleServer(id, function(input, output, module.session) {
    ns <- module.session$ns
    
    
    #enable/ disable view action button
    observe(
      if (disable() == TRUE){
        shinyjs::disable("interactiveschema")
      } else {
        shinyjs::enable("interactiveschema")
      })
    
    #show information on MIDS criteria
    observeEvent(input$info_criteria,{ 
      #show modal
      showModal(modalDialog(
        title = "MIDS criteria",
        "To reach a given MIDS level all MIDS elements must be met (AND),
                  and to meet a MIDS element one of its mappings (composed of properties) must be met (OR).",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })
    
    #show information on MIDS UoM
    observeEvent(input$info_UoM,{ 
      #show modal
      showModal(modalDialog(
        title = "MIDS unknown or missing values",
        "Certain values are known to represent unknown or missing values, 
        which should not count towards meeting a certain MIDS criterium.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })
    
    ## create initial list of elements and mappings from existing MIDS implementation schema
    initialcritlists <- reactive({v <- list()
    for (i in 1:length(jsonschema())){
      midslevel <- names(jsonschema()[i])
      #loop over existing MIDS elements
      for (j in 1:length(jsonschema()[[i]])){
        midscritname <- names(jsonschema()[[i]][j])
        props <- list()
        subcond <- strsplit(jsonschema()[[i]][[j]], split = "\\|")
        #loop over existing mappings
        for (k in seq_along(subcond)){
          props <- stringr::str_remove_all(subcond[[k]], "!is.na|\\(|\\)|\\ ")
          v[[midslevel]][[midscritname]] <- props
        }
      }
    }
    return(v)
    })
    
    #initialize trigger to keep track of changes to elements and/or mappings
    trigger <- reactiveValues(count=0)
    
    #update trigger when sorting is changed
    observeEvent(input$sorted, {trigger$count <- trigger$count + 1})
    
    #get new elements added by user
    UnusedElements <- reactiveValues()
    allAddedElements <- reactiveValues()
    observeEvent(input$addElement, {
      UnusedElements$el <- c(UnusedElements$el, input$newElement)
      allAddedElements$el <- c(allAddedElements$el, input$newElement)
      trigger$count <- trigger$count + 1
    })
    
    ##create list of elements and mapping based on input and added elements
    ranklevels <- reactive({
      if (is_empty(reactiveValuesToList(input)[["unused elements"]])){
        return(c("mids0", "mids1", "mids2", "mids3"))
      } else {
        return(c("mids0", "mids1", "mids2", "mids3", "unused elements"))
      }
    })
    addedcritlists <- eventReactive(trigger$count, {
      x <- list()
      for (i in 1:length(ranklevels())){
        midslevel <- ranklevels()[i]
        #get MIDS elements for a given mids level
        elements <- reactiveValuesToList(input)[[midslevel]]
        for (j in 1:length(elements)){
          #get mappings for each element
          valuesplit <- strsplit(elements[[j]], split = "\\\n")
          element <- trimws(valuesplit[[1]][1], "r")
          mappings <- valuesplit[[1]][-1]
          #check if there are new mappings to be added
          if (!is.null(newMappings[[element]])){
            mappings <- c(mappings, newMappings[[element]])
            #remove mappings when used
            newMappings[[element]] <- NULL
          }
          #check if there are mappings to be removed
          if (!is.null(removeMappings[[element]])){
            mappings <- mappings[!mappings %in% removeMappings[[element]]]
            #remove mappings when used
            removeMappings[[element]] <- NULL
          }
          x[[midslevel]][[element]] <- mappings
          #if element in UnusedElements, remove it
          if (element %in% UnusedElements$el){
            UnusedElements$el <- UnusedElements$el[!UnusedElements$el %in% element]}
        }
        #remove elements
        x[[midslevel]][[removeElements()]] <- NULL
      }
      #add newly added elements
      for (newEl in UnusedElements$el){
        x[["unused elements"]][[newEl]] <- ""
      }
      return(x)
    })

    ##use initial when nothing has been added yet, otherwise use added
    critlists <- reactive({
      if(trigger$count == 0){
        return(initialcritlists())}
      else {return(addedcritlists())}
    })

    ## create ranklists
    critranklists <- reactive({v <- list()
    for (i in 1:length(critlists())){
      if (!is_empty(critlists()[[i]])){
        midslevel <- names(critlists()[i])
        labels <- list()
        #loop over MIDS elements
        for (j in 1:length(critlists()[[i]])){
          elementname <- names(critlists()[[i]][j])
          mappings <- critlists()[[i]][[j]]
          #loop over mappings
          htmlprops <- list()
          for (k in seq_along(mappings)){
            htmlprops <- list(htmlprops,htmltools::tags$li(mappings[[k]]))
          }
          #it should not be possible to remove the last element from a MIDS level
          if (length(critlists()[[i]]) > 1 | midslevel == "unused elements"){
            labels[[j]] <- htmltools::tags$div(id=elementname, list(elementname,
                    actionButton(ns(paste0("edit", elementname)), icon("pencil-alt"), style = "padding:2px; font-size:90%; border-style: none"),
                    actionButton(ns(paste0("removeElement", elementname)), icon("trash"), style = "padding:2px; font-size:90%; border-style: none"), 
                    htmlprops))
          } else {
            labels[[j]] <- htmltools::tags$div(id=elementname, list(elementname,
                    actionButton(ns(paste0("edit", elementname)), icon("pencil-alt"), style = "padding:2px; font-size:90%; border-style: none"),
                    htmlprops))
          }
        }
        if (midslevel != "unused elements"){
          v[[i]] <- rank_list(toupper(midslevel), labels, ns(midslevel),
                              options = sortable_options(group = "midsElements", onSort = sortable_js_capture_input(input_id = ns("sorted"))),
                              class = c("default-sortable", "custom-sortable"))
        } else {
          v[[i]] <- rank_list(toupper(midslevel), labels, ns(midslevel),
                              options = sortable_options(group = "midsElements", onSort = sortable_js_capture_input(input_id = ns("sorted"))),
                              class = c("default-sortable", "custom-sortable", "unused"))
        }
        }
      }
      return(v)
      })

      output$crit <- renderUI({
        critranklists()[seq(1, length(critranklists()), 2)]
        })
      output$crit2 <- renderUI({
        critranklists()[seq(2, length(critranklists()), 2)]
      })
      
      #create "edit mappings" modal
      observe({
        for (i in 1:length(critlists())){
          local({
        if (!is_empty(critlists()[[i]])){
          #loop over MIDS elements
          for (j in 1:length(critlists()[[i]])){
            local({
            elementname <- names(critlists()[[i]][j])
            mappings <- critlists()[[i]][[j]]
            onclick(paste0("edit", elementname), showModal(modalDialog(
              title = "Edit mappings",
              uiOutput(ns(paste0("editmappings", elementname))),
              easyClose = TRUE,
              footer = NULL
            )))
            output[[paste0("editmappings", elementname)]] <- renderUI({
              x <- list()
              x <- list(x, lapply(mappings, FUN = function(mapping) list(
                mapping, actionButton(ns(paste0("remove", elementname, mapping)),
                                                icon("trash"),
                                                style = "padding:5px; font-size:70%; border-style: none"),
                                                br()))
              )
              x <- list(fluidRow(
                column(6, h4(paste0(elementname, ":")), x),
                column(6,
                       selectizeInput(ns(paste0("newMapping", elementname)),
                                      label = "Enter a new mapping",
                                      choices = readLines("www/DWCAcolumnnames.txt"),
                                      multiple = TRUE),
                       helpText("Select multiple properties at once if they must all be present (&)"),
                       actionButton(ns(paste0("addMapping", elementname)), "Add")
                )))
              return(x)
            })
            })
          }
        }
        })
      }
    })
  
    #create observers for adding mappings of initial elements, and get new mappings
    newMappings <- reactiveValues()
    observe(
        for (i in 1:length(initialcritlists())){
          local({
          #loop over MIDS elements
          for (j in 1:length(initialcritlists()[[i]])){
            local({
            elementname <- names(initialcritlists()[[i]][j])
            observeEvent(input[[paste0("addMapping", elementname)]], {
              newMappings[[elementname]] <- paste(input[[paste0("newMapping", elementname)]], collapse = "&")
              trigger$count <- trigger$count + 1
            })
            })
          }
          })
        })

    #create observers for adding mappings of added elements, and get new mappings
    observe(
      for (i in 1:length(allAddedElements$el)){
        local({
          elementname <- allAddedElements$el[[i]]
          observeEvent(input[[paste0("addMapping", elementname)]], {
            newMappings[[elementname]] <- paste(input[[paste0("newMapping", elementname)]], collapse = "&")
            trigger$count <- trigger$count + 1
          })
        })
    })

    #create observers for removing elements, and get elements to be removed 
    removeElements <- reactiveVal("nothing")
    observe(
      for (i in 1:length(critlists())){
        local({
          if (!is_empty(critlists()[[i]])){
            #loop over MIDS elements
            for (j in 1:length(critlists()[[i]])){
              local({
                elementname <- names(critlists()[[i]][j])
                observeEvent(input[[paste0("removeElement", elementname)]], {
                  removeElements(elementname)
                  trigger$count <- trigger$count + 1
                })
              })
            }
          }
        })
      })
    
    #create observers for removing mappings, and get mappings to be removed 
    removeMappings <- reactiveValues()
    observe(
      for (i in 1:length(critlists())){
        local({
          if (!is_empty(critlists()[[i]])){
            #loop over MIDS elements
            for (j in 1:length(critlists()[[i]])){
              local({
                elementname <- names(critlists()[[i]][j])
                mappings <- critlists()[[i]][[j]]
                for (mapping1 in mappings){
                  local({
                    mapping <- mapping1
                    observeEvent(input[[paste0("remove", elementname, mapping)]], {
                      removeMappings[[elementname]] <- mapping
                      trigger$count <- trigger$count + 1
                    })
                  })
                }
              })
            }
          }
        })
      })
    
    ## get inputs
    critinputs <- reactive({x <- list()
    #loop through mids levels
    midslevels <- names(critlists())
    for (i in 1:length(midslevels)){
      if (midslevels[i] != "unused elements"){
        #get MIDS elements for a given mids level
        elements <- req(reactiveValuesToList(input)[[midslevels[i]]])
        for (j in 1:length(elements)){
          #get mappings for each element
          valuesplit <- strsplit(elements[[j]], split = "\\\n")
          element <- trimws(valuesplit[[1]][1], "r")
          mappings <- valuesplit[[1]][-1]
          #don't use elements that have no mappings
          if (!is_empty(mappings)){
            if (any(grepl("&", mappings, fixed = TRUE))){
              #deal with &
              x[[midslevels[i]]][[element]][["subcondition1"]][["property"]] <- strsplit(mappings[grepl("&", mappings, fixed = TRUE)], "&")[[1]]
              x[[midslevels[i]]][[element]][["subcondition1"]][["operator"]] <- "AND"
              #separate those without &, if there are any
              if (any(!grepl("&", mappings, fixed = TRUE))){
                x[[midslevels[i]]][[element]][["subcondition2"]][["property"]] <- mappings[!grepl("&", mappings, fixed = TRUE)]
                x[[midslevels[i]]][[element]][["subcondition2"]][["operator"]] <- "OR"
              }
            } else if (any(grepl("!", mappings, fixed = TRUE))){
              #deal with !
              x[[midslevels[i]]][[element]][["subcondition1"]][["property"]] <- gsub("!", "", mappings[grepl("!", mappings, fixed = TRUE)])
              x[[midslevels[i]]][[element]][["subcondition1"]][["operator"]] <- "NOT"
              #separate those without !, if there are any
              if (any(!grepl("!", mappings, fixed = TRUE))){
                x[[midslevels[i]]][[element]][["subcondition2"]][["property"]] <- mappings[!grepl("!", mappings, fixed = TRUE)]
                x[[midslevels[i]]][[element]][["subcondition2"]][["operator"]] <- "OR"
              }
            } else if (length(mappings) > 1) {
              #no & or !
              x[[midslevels[i]]][[element]][["subcondition1"]][["property"]] <- mappings
              x[[midslevels[i]]][[element]][["subcondition1"]][["operator"]] <- "OR"
            } else {
            #no & or ! and only one property
              x[[midslevels[i]]][[element]][["subcondition1"]][["property"]] <- mappings
            }
            
          }
        }
      }
    }
    return(x)
    })
    
    ## get properties used in the schema
    usedproperties <- reactive(
      {x <- character()
      #loop through mids levels
      midslevels <- names(critlists())
      for (i in 1:length(midslevels)){
        #get MIDS elements for a given mids level
        elements <- reactiveValuesToList(input)[[midslevels[i]]]
        for (j in seq_along(elements)){
          #get mappings for each element
          valuesplit <- strsplit(elements[[j]], split = "\\\n")
          mappings <- gsub("!", "", flatten_chr(strsplit(valuesplit[[1]][-1], "&")))
          #don't use elements that have no mappings
          if (!is_empty(mappings)){
            x <- c(x, mappings)
          }
        }
      }
      return(x)
      })
    
    # Edit Unknown or Missing section -----------------------------------------
    
    ##update property selection
    #only update choices when navigating to this tab
    mcprops <- eventReactive(input$tabs == "2. Unknown or Missing",
                             {usedproperties()})
    output$newPropUoM <- renderUI({selectInput(ns("newPropUoM"), label = "Enter a new property",
                                               choices = sort(mcprops()))})
    
    ## get UoM values and properties from existing JSON schema
    UoMinitial <- reactive({v <- list()
    for (i in 1:length(jsonUoM())){
      v[[i]] <- rank_list(names(jsonUoM()[i]), jsonUoM()[[i]], ns(paste0("UoM", names(jsonUoM()[i]))), options = sortable_options(group = "midsUoM"),
                          class = c("default-sortable", "custom-sortable"))
    }
    return(v)
    })
    
    ## add UoM properties specified by user
    #get new UoM property from text input field
    newprop <- eventReactive(input$addUoMprop, {input$newPropUoM})
    ## also get previously submitted properties
    newprops <- reactiveValues(prev_bins = NULL)
    observeEvent(input$addUoMprop, {
      newprops$prev_bins <- c(newprops$prev_bins, input$newPropUoM)
    })
    #add rank list for each submitted property
    newPropUoM <- reactive({v <- list()
    for (i in 1:length(req(newprops$prev_bins))){
      #get value inside property
      value <- input[[paste0("UoM", newprops$prev_bins[i])]]
      #add rank list for each property
      v[[i]] <- rank_list(newprops$prev_bins[i], value, ns(paste0("UoM", newprops$prev_bins[i])), options = sortable_options(group = "midsUoM"),
                          class = c("default-sortable", "custom-sortable"))
    }
    return(v)
    })
    
    ## add UoM values specified by user (and keep existing values)
    existing <- eventReactive(input$addUoM, {input$UoMunused}) 
    new <- eventReactive(input$addUoM, {input$newValueUoM})
    UoMunused <- reactive({rank_list("Unused values", c(existing(), new()), ns("UoMunused"), options = sortable_options(group = "midsUoM"),
                                           class = c("default-sortable", "custom-sortable", "unused")) })
    
    #combine all ranklists
    UoMranklists <- reactive({
      if (input$addUoMprop == 0 & input$addUoM == 0){return(UoMinitial())}
      else if (input$addUoMprop == 0) {return( c(UoMinitial(), UoMunused()))}
      else if (input$addUoM == 0) {return( c(UoMinitial(), newPropUoM()))}
      else {return( c(UoMinitial(), newPropUoM(), UoMunused()))}
    })
    
    #render ranklists
    output$UoM <- renderUI({
      UoMranklists()[seq(1, length(UoMranklists()), 2)]
    })
    output$UoM2 <- renderUI({
      UoMranklists()[seq(2, length(UoMranklists()), 2)]
    })
    
    ## combine all UoM inputs from interatice schema
    UoMinputs <- reactive({
      x <- list()
      #get names of original properties from schema and of user specified properties
      properties <- c(names(jsonUoM()), newprops$prev_bins)
      #get values for each property
      for (j in 1:length(properties)){
        value <-  reactiveValuesToList(input)[paste0("UoM", properties[j])]
        #don't include empty properties
        if (rlang::is_empty(value[[1]])){next}
        if (properties[j] != "all"){
          x[["unknownOrMissing"]][[properties[j]]][["value"]] <- value[[1]]
          x[["unknownOrMissing"]][[properties[j]]][["midsAchieved"]] <- FALSE
          x[["unknownOrMissing"]][[properties[j]]][["property"]] <- properties[j]
        } else {
          all_props <- unlist(reactiveValuesToList(input)[paste0("UoM", "all")])
          for (n_prop in seq_along(all_props)){
            x[["unknownOrMissing"]][[paste0("all", n_prop)]][["value"]] <- all_props[[n_prop]]
            x[["unknownOrMissing"]][[paste0("all", n_prop)]][["midsAchieved"]] <- FALSE
          }
        }
      }
    return(x)
    })
    
    #get UoM from file as well
    UoMfile <- reactive({
      x <- list()
      for (j in 1:length(jsonUoM())){
        value <-  jsonUoM()[[j]]
        name <- names(jsonUoM()[j])
        #don't include empty properties
        if (rlang::is_empty(value[[1]])){next}
        if (name != "all"){
          x[["unknownOrMissing"]][[name]][["value"]] <- value[[1]]
          x[["unknownOrMissing"]][[name]][["midsAchieved"]] <- FALSE
          x[["unknownOrMissing"]][[name]][["property"]] <- name
        } else {
          all_props <- unlist(value)
          for (n_prop in seq_along(all_props)){
            x[["unknownOrMissing"]][[paste0("all", n_prop)]][["value"]] <- all_props[[n_prop]]
            x[["unknownOrMissing"]][[paste0("all", n_prop)]][["midsAchieved"]] <- FALSE
          }
        }
      }
      return(x)
    })
   
    #combine sections
    schema <- reactive({
      #if UoM section wasn't visited, fill this with the UoM from file
      if (is_empty(UoMinputs())){
        schema <- c(critinputs(), UoMfile())
      } else {
        schema <- c(critinputs(), UoMinputs())
      }
      return(schema)
    })
    
    #convert to json and add info for json file
    schematojson <- reactive({
      x <- list()
      x[["schemaname"]] <- "MIDS calculator interactive"
      x[["date"]] <- Sys.time()
      x[["schemaType"]] <- "MIDScalculator"
      return(toJSON(c(x, schema())))
    })
    
    #download json
    output$download <- 
      downloadHandler(
        filename = function() {
          "MIDS_schema.json"
        },
        content = function(file) {
          write(schematojson(), file)
        })
    
    #check if the edit actionbutton was clicked
    visited <- reactiveVal(FALSE)
    observe(
    if (input$interactiveschema > 0){visited(TRUE)}
    )
    
    return(list(interactivejson = reactive({schematojson()}), visited = reactive({visited()})))
    
   
  })
}