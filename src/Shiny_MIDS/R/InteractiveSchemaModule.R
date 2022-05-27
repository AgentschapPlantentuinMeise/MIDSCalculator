InteractiveSchemaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("interactiveschema"), "Edit interactively"),
    #Interactive editing of MIDS implementation in modal window
    bsModal(ns("id"), "Edit MIDS implementation interactively", ns("interactiveschema"), 
      tabsetPanel(type = "tabs",
        tabPanel("Criteria",
           fluidRow(
             column(
               tags$h1("MIDS criteria"),
               width = 12,
               div(
                 class = "bucket-list-container default-sortable",
                 "To reach a given MIDS level all MIDS elements must be met (AND),
                  and to meet a MIDS element one of its mappings (composed of properties) must be met (OR).",
                 br(),br(),
                 fluidPage(fluidRow(
                   column(6, textInput(ns("newElement"), "Enter a new MIDS element",
                                       value = "Enter text..."),
                          actionButton(ns("addElement"), "Add"))
                 )),
                 br(), br(),
                 "Drag the MIDS elements to the desired MIDS level. Click the edit button to change the mappings of a MIDS element.",
                 br(),
                 div(
                   class = "default-sortable bucket-list bucket-list-horizontal",
                   uiOutput(ns("crit"))
                 )
               )
             )
           ),
           fluidRow(
             column(
               width = 12,
               tags$b("Result"),
               column(
                 width = 12,
                 tags$p("input$midscriteria"),
                 verbatimTextOutput(ns("results_3"))
               )
             )
           )
        ),
        tabPanel("Unknown or Missing values",
           fluidRow(
             column(
               tags$h1("MIDS unknown or missing values"),
               width = 12,
               div(
                 class = "bucket-list-container default-sortable",
                 "Drag the unknown or missing values to the desired properties",
                 br(),br(),
                 fluidPage(fluidRow(
                   column(6, textInput(ns("UoMnewvalue"), "Enter a new value",
                                       value = "Enter text...")),
                   column(6, uiOutput(ns("UoMnewprop")))
                 )),
                 fluidPage(fluidRow(
                   column(6, actionButton(ns("addUoM"), "Add")),
                   column(6, actionButton(ns("addUoMprop"), "Add"))
                 )),
                 div(
                   class = "bucket-list-container default-sortable",
                   uiOutput(ns("UoMall")),
                   uiOutput(ns("UoMextra")),
                   uiOutput(ns("UoMunused"))
                 )
               )
             )
           ),
           fluidRow(
             column(
               width = 12,
               tags$b("Result"),
               column(
                 width = 12,
                 tags$p("input$midsUoM"),
                 verbatimTextOutput(ns("results_UoM"))
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
    
    ## create initial list of elements and mappings from existing MIDS implementation schema
    initialcritlists <- reactive({v <- list()
    for (i in 1:length(jsonschema)){
      midslevel <- names(jsonschema[i])
      #loop over existing MIDS elements
      for (j in 1:length(jsonschema[[i]])){
        midscritname <- names(jsonschema[[i]][j])
        props <- list()
        subcond <- strsplit(jsonschema[[i]][[j]], split = "\\|")
        #loop over existing mappings
        for (k in seq_along(subcond)){
          props <- stringr::str_remove_all(subcond[[k]], "!is.na|\\(|\\)|\\ ")
          v[[midslevel]][[midscritname]] <- props
        }
      }
    }
    return(v)
    })

    trigger <- reactiveValues(count=0)
    ## get MIDS element specified by user and create observer for editing mappings for that element
    newElements <- reactiveValues()
    observeEvent(input$addElement, {
      newElements$el <- c(newElements$el, input$newElement)
      observeEvent(input[[paste0("addMapping", input$newElement)]], {
        newMappings[[input$newElement]] <- paste(input[[paste0("newMapping", input$newElement)]], collapse = "&")
        trigger$count <- trigger$count + 1
      })
    })
    observeEvent(input$addElement, {
        trigger$count <- trigger$count + 1
    })

    ##create list of elements and mapping based on input and added elements
    addedcritlists <- eventReactive(trigger$count, {
      x <- list()
      for (i in 1:length(jsonschema)){
        midslevel <- names(jsonschema[i])
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
            # #remove mappings when used
            # removeMappings[[element]] <- NULL
          }
          x[[midslevel]][[element]] <- mappings
          #if element in newElements, remove it
          if (element %in% newElements$el){
            newElements$el <- newElements$el[!newElements$el %in% element]}
        }
      }
      #add newly added elements
      for (newEl in newElements$el){
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
        labels[[j]] <- htmltools::tags$div(id=elementname, list(elementname,
                                                                actionButton(ns(paste0("edit", elementname)), icon("pencil-alt"), style = "padding:2px; font-size:90%; border-style: none"), 
                                                                htmlprops))
      }
      v[[i]] <- rank_list(toupper(midslevel), labels, ns(midslevel),
                          options = sortable_options(group = "midsElements"))
    }
    return(v)
    })

    output$crit <- renderUI(critranklists())
    
    #create "edit mappings" modal
    observe({
      for (i in 1:length(critlists())){
        local({
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
    #create observers for removing mappings, and get mappings to be removed 
    removeMappings <- reactiveValues()
    observe(
      for (i in 1:length(critlists())){
        local({
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
        })
      })
    
    ## get inputs
    critinputs <- reactive({x <- list()
    #loop through mids levels
    midslevels <- names(critlists())
    for (i in 1:length(midslevels)){
      if (midslevels[i] != "unused elements"){
        #get MIDS elements for a given mids level
        elements <- reactiveValuesToList(input)[[midslevels[i]]]
        for (j in 1:length(elements)){
          #get mappings for each element
          valuesplit <- strsplit(elements[[j]], split = "\\\n")
          element <- valuesplit[[1]][1]
          mappings <- valuesplit[[1]][-1]
          #don't use elements that have no mappings
          if (!is_empty(mappings)){
            x[[midslevels[i]]][[element]] <- mappings
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
        for (j in 1:length(elements)){
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
    
    ## convert outputs to usable filters for mids calc
    midscalccrits <- reactive({x <- list()
    for (i in 1:length(critinputs())){
      for (j in 1:length(critinputs()[[i]])){
        subcond <- critinputs()[[i]][[j]]
        nasubconds <- list()
        for (k in 1:length(subcond)){
          #deal with !
          if (grepl("!", subcond[k], fixed = TRUE)){
            nasubcond <- paste0("is.na(", substring(subcond[k],2), ")")}
          #deal with &
          else if (grepl("&", subcond[k], fixed = TRUE)){
            nasubcond <- "("
            split <- strsplit(subcond[k], "&")[[1]]
            for (l in 1:length(split)){
              nasubcond <- paste0(nasubcond, "!is.na(", split[l], ") & ")
            }
            nasubcond <- paste0(substr(nasubcond, 1, nchar(nasubcond)-3), ")")
          }
          #others (no & or !):
          else {nasubcond <- paste0("!is.na(", subcond[k], ")")}
          #combine
          nasubconds <- c(nasubconds, nasubcond)
        }
        #collapse subconditions
        collsubcond <- paste(nasubconds, collapse = " | ")
        #create list again
        x[[names(critinputs())[i]]][[names(critinputs()[[i]][j])]] <- collsubcond
      }
    }
    return(x)
    })
    
    #show output
    output$results_3 <-
      renderPrint({
        jsonlist()
      })
    
    # Edit Unknown or Missing section -----------------------------------------
    
    ##update property selection
    #only update choices when navigating to this tab
    mcprops <- eventReactive(input$tabs == "2. Unknown or Missing",
                             {usedproperties()})
    output$UoMnewprop <- renderUI({selectInput(ns("UoMnewprop"), label = "Enter a new property",
                                               choices = sort(mcprops()))})
    
    ## add UoM values and properties from existing JSON schema
    UoMranklists <- reactive({v <- list()
    for (i in 1:length(jsonUoM)){
      v[[i]] <- rank_list(names(jsonUoM[i]), jsonUoM[[i]], ns(paste0("UoM", names(jsonUoM[i]))), options = sortable_options(group = "midsUoM"))
    }
    return(v)
    })
    output$UoMall <- renderUI(UoMranklists())
    
    ## add UoM properties specified by user
    #get new UoM property from text input field
    newprop <- eventReactive(input$addUoMprop, {input$UoMnewprop})
    ## also get previously submitted properties
    newprops <- reactiveValues(prev_bins = NULL)
    observeEvent(input$addUoMprop, {
      newprops$prev_bins <- c(newprops$prev_bins, input$UoMnewprop)
    })
    #add rank list for each submitted property
    UoMnewpropranklists <- reactive({v <- list()
    for (i in 1:length(req(newprops$prev_bins))){
      #get value inside property
      value <- input[[paste0("UoM", newprops$prev_bins[i])]]
      #add rank list for each property
      v[[i]] <- rank_list(newprops$prev_bins[i], value, ns(paste0("UoM", newprops$prev_bins[i])), options = sortable_options(group = "midsUoM"))
    }
    return(v)
    })
    output$UoMextra <- renderUI(UoMnewpropranklists())
    
    ## add UoM values specified by user (and keep existing values)
    existing <- eventReactive(input$addUoM, {input$UoMunused}) 
    new <- eventReactive(input$addUoM, {input$UoMnewvalue})
    output$UoMunused <- renderUI(rank_list("Unused values", c(existing(), new()), ns("UoMunused"), options = sortable_options(group = "midsUoM")))
    
    ## combine all UoM inputs
    UoMinputs <- reactive({x <- list()
    #get names of original properties from schema and of user specified properties
    properties <- c(names(jsonUoM), newprops$prev_bins)
    #get values for each property
    for (j in 1:length(properties)){
      value <-  reactiveValuesToList(input)[paste0("UoM", properties[j])]
      #don't include empty properties
      if (rlang::is_empty(value[[1]])){next}
      x[[properties[j]]] <- value[[1]]
    }
    return(x)
    })
    
    #show output
    output$results_UoM <-
      renderPrint(
        UoMinputs()
      )
    
    ## Combine interactive JSON section in 1 list
    jsonlist <- reactive({
      list <- list()
      list[["criteria"]] <- midscalccrits()
      list[["UoM"]] <- UoMinputs()
      #if UoM section wasn't visited, fill this with the UoM from file
      if (is_empty(list[["UoM"]])){list[["UoM"]] <- jsonUoM}
      list[["properties"]] <- usedproperties()
      return(list)
    })
    
   
  })
}