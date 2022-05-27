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
                 #verbatimTextOutput(ns("test")),
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
                   column(6, textInput("UoMnewvalue", "Enter a new value",
                                       value = "Enter text...")),
                   column(6, uiOutput("UoMnewprop"))
                 )),
                 fluidPage(fluidRow(
                   column(6, actionButton("addUoM", "Add")),
                   column(6, actionButton("addUoMprop", "Add"))
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

InteractiveSchemaServer <- function(id, jsonschema) {
  moduleServer(id, function(input, output, module.session) {
    ns <- module.session$ns
    
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
    #create observers for removing mappings of initial elements, and get mappings to be removed 
    removeMappings <- reactiveValues()
    observe(
      for (i in 1:length(initialcritlists())){
        local({
          #loop over MIDS elements
          for (j in 1:length(initialcritlists()[[i]])){
            local({
              elementname <- names(initialcritlists()[[i]][j])
              mappings <- initialcritlists()[[i]][[j]]
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
    
    
    #show output
    output$results_3 <-
      renderPrint({
        critlists()
      })

   
  })
}