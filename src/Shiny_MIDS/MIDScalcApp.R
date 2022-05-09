library(shiny)
library(shinyBS)
library(ggplot2)
library(DT)
library(shinyjs)
library(sortable)
source(file = "../parse_json_schema.R")
source(file = "../MIDS-calc.R")

#Increase upload limit to 5GB
options(shiny.maxRequestSize = 5000*1024^2)

# Define UI ----
ui <- 
  tagList(
  useShinyjs(),
  navbarPage(title=div(tags$img(height = 30, src = "Logo_MeiseBotanicGarden_rgb.jpg"), "Calculate MIDS scores"),
                 id = "tabs",
                 tabPanel("Submit data",
                          div(
                          br(), br(),
                          fluidRow(column(width = 6, offset = 3,
                          wellPanel(
                          h4("Submit dataset"),
                          fileInput("gbiffile", "Upload zipped GBIF annotated archive (max 5 GB)",
                                    accept = ".zip")))),
                          br(), 
                          fluidRow(column(width = 6, offset = 3,
                          wellPanel(
                          h4("Specify MIDS implementation"), br(),
                          radioButtons("jsonfile", label = NULL, 
                                       choiceNames = list("Use default", 
                                                          "Upload custom"),
                                       choiceValues = list("default", "custom")),
                          fileInput("customjsonfile", label = NULL,
                                    accept = ".json"),
                          fluidRow(column(5, actionButton("interactiveschema", "Edit interactively")),
                          column(7, ViewImplementationUI("viewcurrentschema")))))),
                          br(),br(),
                          actionButton("start", "Start MIDS score calculations"),
                          align="center")
                          ),
    #Interactive editing of MIDS implementation in modal window
    bsModal("id", "title", "interactiveschema", 
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
                     column(6, textInput("newElement", "Enter a new MIDS element",
                                         value = "Enter text...")),
                     column(6, selectizeInput("newMapping",
                                              label = "Enter a new mapping",
                                              choices = readLines("www/DWCAcolumnnames.txt"),
                                              multiple = TRUE),
                            helpText("Select multiple properties at once if they must all be present (&)"))
                   )),
                   fluidPage(fluidRow(
                     column(6, actionButton("addElement", "Add")),
                     column(6, actionButton("addMapping", "Add"))
                   )),
                   br(), br(),
                   "Drag the subconditions to the desired MIDS criterium, and the MIDS criteria to the desired MIDS level.",
                   br(),
                   div(
                     class = "default-sortable bucket-list bucket-list-horizontal",
                     uiOutput("crit"),
                     uiOutput("unused"),
                     uiOutput("extracrit")
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
                   verbatimTextOutput("results_3")
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
                     uiOutput("UoMall"),
                     uiOutput("UoMextra"),
                     uiOutput("UoMunused")
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
                   verbatimTextOutput("results_UoM")
                 )
               )
             )
          ))
    )
))

# Define server logic ----
server <- function(input, output, session) {

  #show and hide json upload button and start button
  observe({
    if (input$jsonfile == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
    if (is.null(input$gbiffile) | (input$jsonfile == "custom" & is.null(input$customjsonfile))){
      shinyjs::disable("start")} else {shinyjs::enable("start")}
  })
  

# Calculations ------------------------------------------------------------

  #calculate mids levels and criteria
  gbif_dataset_mids <- eventReactive(input$start, {
    if (input$interactiveschema == 0){
      withProgress(message = 'Calculating MIDS scores', value = 0, {
        calculate_mids(gbiffile = input$gbiffile$datapath, jsonfile = jsonpath())})
    }
    else if (input$interactiveschema > 0){
      withProgress(message = 'Calculating MIDS scores', value = 0, {
        calculate_mids(gbiffile = input$gbiffile$datapath, jsontype = "list", jsonlist = jsonlist())})
    }
  })
  
# Initialize MIDS implementation ------------------------------------------

  #get path to json schema
  jsonpath <- reactive({
    if (input$jsonfile == "default"){
      return("../../data/schemas/secondschema_conditions_same_level.json")}
    if (input$jsonfile == "custom"){
      return(input$customjsonfile$datapath)}
  })
  
  #json schema from file
  jsonschema <- reactive({ 
    read_json_mids_criteria(file = jsonpath(), outtype = "criteria")
  })
  
  #json UoM from file
  jsonUoM <- reactive({ 
    read_json_unknownOrMissing(file = jsonpath())
  })

# Show current MIDS implementation ----------------------------------------

  
  #view MIDS implementation in modal window
  observe(
  #show schema from interactive
  if (input$interactiveschema > 0){
    ViewImplementationServer("viewcurrentschema", jsonlist()[[1]], jsonlist()[[2]])
  #show schema from file
  } else {
    ViewImplementationServer("viewcurrentschema", jsonschema(), jsonUoM())
  })
    

# Edit JSON ---------------------------------------------------------------

# Edit Unknown or Missing section -----------------------------------------
  
  ##update property selection
  #only update choices when navigating to this tab
  mcprops <- eventReactive(input$tabs == "2. Unknown or Missing",
                {usedproperties()})
  output$UoMnewprop <- renderUI({selectInput("UoMnewprop", label = "Enter a new property",
                             choices = sort(mcprops()))})
  
  ## add UoM values and properties from existing JSON schema
  UoMranklists <- reactive({v <- list()
  for (i in 1:length(jsonUoM())){
    v[[i]] <- rank_list(names(jsonUoM()[i]), jsonUoM()[[i]], paste0("UoM", names(jsonUoM()[i])), options = sortable_options(group = "midsUoM"))
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
    v[[i]] <- rank_list(newprops$prev_bins[i], value, paste0("UoM", newprops$prev_bins[i]), options = sortable_options(group = "midsUoM"))
  }
  return(v)
  })
  output$UoMextra <- renderUI(UoMnewpropranklists())
  
  ## add UoM values specified by user (and keep existing values)
  existing <- eventReactive(input$addUoM, {input$UoMunused}) 
  new <- eventReactive(input$addUoM, {input$UoMnewvalue})
  output$UoMunused <- renderUI(rank_list("Unused values", c(existing(), new()), "UoMunused", options = sortable_options(group = "midsUoM")))
  
  ## combine all UoM inputs
  UoMinputs <- reactive({x <- list()
  #get names of original properties from schema and of user specified properties
  properties <- c(names(jsonUoM()), newprops$prev_bins)
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
    renderPrint(UoMinputs()
      )
  
# Edit JSON ---------------------------------------------------------------
  
  
# Edit criteria -----------------------------------------------------------
  
  ## add criteria from existing JSON schema
  critranklists <- reactive({v <- list()
  for (i in 1:length(jsonschema())){
    midslevel <- names(jsonschema()[i])
    midscritranks <- list()
    for (j in 1:length(jsonschema()[[i]])){
      midscritname <- names(jsonschema()[[i]][j])
      props <- list()
      subcond <- strsplit(jsonschema()[[i]][[j]], split = "\\|")
      for (k in 1:length(subcond)){
        props <- stringr::str_remove_all(subcond[[k]], "!is.na|\\(|\\)|\\ ")
      }
      midscritrank <- rank_list(midscritname, props, 
                        midscritname, options = sortable_options(group = "midsMappings"))
      midscritranks <- c(midscritranks, midscritrank)
    }
    v[[i]] <- rank_list(midslevel, midscritranks, midslevel,
                        options = sortable_options(group = "midsElements"))
  }
  return(v)
  })
  output$crit <- renderUI(critranklists())
  

  ## add mappings specified by user (and keep existing values)
  existingMappings <- eventReactive(input$addMapping, {input$unused}) 
  newMapping <- eventReactive(input$addMapping, {paste(input$newMapping, collapse = "&")})
  output$unused <- renderUI(rank_list("Unused mappings", c(existingMappings(), newMapping()), 
                          "unused", options = sortable_options(group = "midsMappings")))
  
  
  ## add MIDS elements specified by user
  #get new MIDS element from text input field
  newElement <- eventReactive(input$addElement, {input$newElement})
  # also get other elements still under unused elements
  unusedcrits <- eventReactive(input$addElement, {
    prevcrits <- list()
    previnput <- reactiveValuesToList(input)[["unusedcrit"]][reactiveValuesToList(input)[["unusedcrit"]]!=""]
    if (length(previnput) > 0){
    for (k in 1:length(previnput)){
      prevcrit <- strsplit(previnput[[k]], split = "\\\n\\\n")[[1]][1]
      prevcrits <- c(prevcrits, prevcrit)
    }
    return(c(newElement(), prevcrits))}
    else
    {return(newElement())}
  })
  #add rank list for each submitted and existing element (under Unused elements)
  newcritranklists <- reactive({v <- list()
  extracrits <- list()
  for (i in 1:length(req(unusedcrits()))){
    #get value inside criterium
    value <- input[[unusedcrits()[[i]]]]
    #rank list for each criterium
    extracrit <- rank_list(unusedcrits()[[i]], value, unusedcrits()[[i]], options = sortable_options(group = "midsMappings"))
    extracrits <- c(extracrits, extracrit)
  }
  v <- rank_list("Unused MIDS elements",
                 extracrits,
                 "unusedcrit",
                 options = sortable_options(group = "midsElements"))
  return(v)
  })
  output$extracrit <- renderUI(newcritranklists())
  
  
  ## get inputs
  critinputs <- reactive({x <- list()
  #loop through mids levels
  midslevels <- names(jsonschema())
  for (i in 1:length(midslevels)){
    #get MIDS elements for a given mids level
    critsubcond <- reactiveValuesToList(input)[[midslevels[i]]]
    critsubcond <- critsubcond[seq(1, length(critsubcond), 3)]
    #get mappings for each element
    for (j in 1:length(critsubcond)){
      valuesplit <- strsplit(gsub(" ", "", critsubcond[[j]]), split = "\\\n\\\n")
      crit <- valuesplit[[1]][1]
      subconds <- reactiveValuesToList(input)[[crit]]
       #don't use elements that have no mappings
      if (!is.null(subconds)){
        x[[midslevels[i]]][[crit]] <- subconds
      }
    }
  }
  return(x)
  })
  
  ## get properties used in the schema
  usedproperties <- reactive(
  {x <- character()
  #loop through mids levels
  midslevels <- names(jsonschema())
  for (i in 1:length(midslevels)){
    #get MIDS elements for a given mids level
    critsubcond <- reactiveValuesToList(input)[[midslevels[i]]]
    critsubcond <- critsubcond[seq(1, length(critsubcond), 3)]
    #get mappings for each element
    for (j in 1:length(critsubcond)){
      valuesplit <- strsplit(gsub(" ", "", critsubcond[[j]]), split = "\\\n\\\n")
      crit <- valuesplit[[1]][1]
      subconds <- reactiveValuesToList(input)[[crit]]
      #don't use elements that have no mappings
      if (!rlang::is_empty(subconds[1])){
        props <- gsub("!", "", flatten_chr(strsplit(subconds, split = "&")))
        x <- c(x, props)}
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
  
  ## Combine interactive JSON section in 1 list
  jsonlist <- reactive({
    list <- list()
    list[["criteria"]] <- midscalccrits()  
    list[["UoM"]] <- UoMinputs()
    #if UoM section wasn't visited, fill this with the UoM from file
    if (is_empty(list[["UoM"]])){list[["UoM"]] <- jsonUoM()}
    list[["properties"]] <- usedproperties() 
    return(list)
  })
  

  #show output
  output$results_3 <-
    renderPrint({
      jsonlist()  
    })


# Allow multiple results tabs --------------------------------------------

  #count how many times start is clicked
  startcounter <- reactiveValues(countervalue = 0)
  observeEvent(input$start, {
    startcounter$countervalue <- startcounter$countervalue + 1})
  
  #save all MIDS calculations
  allmidscalc <- reactiveValues(prev_bins = NULL)
  observe({
    allmidscalc$prev_bins[[startcounter$countervalue]] <- gbif_dataset_mids()
  })
  
  #save all MIDS implementations
  allschemas <- reactiveValues(prev_bins = NULL)
  observeEvent(input$start, {
    if (input$interactiveschema > 0){
      allschemas$prev_bins[[startcounter$countervalue]] <- jsonlist()[1:2]
    } else {
      allschemas$prev_bins[[startcounter$countervalue]] <- c(list("criteria" = jsonschema()), list("UoM" = jsonUoM()))
    }
  })

# Filters -----------------------------------------------------------------
  
  #Setting filters when new analysis is started
  observeEvent(input$start, {
    #reset rank filter when new dataset is provided 
    updateSelectInput(session, paste0("rank", startcounter$countervalue),
                    selected = "None")
    #update country filter with countries from the dataset
    updateSelectInput(session, paste0("country", startcounter$countervalue), label = "Filter on countrycode", 
                      choices = sort(unique(gbif_dataset_mids()$countryCode)))
    #update date filter with dates from the dataset
    updateSliderInput(session, paste0("date", startcounter$countervalue), label = "Filter on collection date", 
                      min = min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                      max = max(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                      value = c(min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                                max(gbif_dataset_mids()$eventDate, na.rm = TRUE)),
                      timeFormat = "%m/%d/%Y")
  })
  
  #update taxonomy filter when a taxonomic rank is chosen
  observeEvent(input[[paste0("rank", as.integer(gsub("Results", "", input$tabs)))]], {
    if (input[[paste0("rank", as.integer(gsub("Results", "", input$tabs)))]] != "None"){
      updateSelectInput(session, paste0("taxonomy", as.integer(gsub("Results", "", input$tabs))), label = "Filter on taxonomy",
                        choices = sort(unique(req(allmidscalc$prev_bins[[as.integer(gsub("Results", "", input$tabs))]])[[tolower(input[[paste0("rank", as.integer(gsub("Results", "", input$tabs)))]])]])))
      shinyjs::show(paste0("taxonomy", as.integer(gsub("Results", "", input$tabs))))
    }
    else {shinyjs::hide(paste0("taxonomy", as.integer(gsub("Results", "", input$tabs))))}
  })
  
  #apply filters if they are set
  gbif_dataset_mids_filtered <- reactive({
    req(allmidscalc$prev_bins[[as.integer(gsub("Results", "", input$tabs))]]) %>%
    {if (!is.null(input[[paste0("country", as.integer(gsub("Results", "", input$tabs)))]]) && input[[paste0("country", as.integer(gsub("Results", "", input$tabs)))]] != "All") {
        filter(., countryCode %in% input[[paste0("country", as.integer(gsub("Results", "", input$tabs)))]])} else {.}} %>%
    {if (req(input[[paste0("date", as.integer(gsub("Results", "", input$tabs)))]][1]) != 0) {
      filter(., eventDate >= input[[paste0("date", as.integer(gsub("Results", "", input$tabs)))]][1])} else {.}} %>%
    {if (req(input[[paste0("date", as.integer(gsub("Results", "", input$tabs)))]][2]) != 100) {
      filter(., eventDate <= input[[paste0("date", as.integer(gsub("Results", "", input$tabs)))]][2])} else {.}} %>%
    {if (input[[paste0("rank", as.integer(gsub("Results", "", input$tabs)))]] != "None" && !is.null(input[[paste0("taxonomy", as.integer(gsub("Results", "", input$tabs)))]]) && input[[paste0("taxonomy", as.integer(gsub("Results", "", input$tabs)))]] != "All"){
        filter(., .data[[tolower(input[[paste0("rank", as.integer(gsub("Results", "", input$tabs)))]])]] %in% input[[paste0("taxonomy", as.integer(gsub("Results", "", input$tabs)))]])} else {.}}
  })
  

# Calculate summarized results --------------------------------------------

  #create summary of MIDS levels
  midssum <- reactive({
    gbif_dataset_mids_filtered() %>% group_by(MIDS_level) %>% summarise(Number_of_records = n(), Percentage = round(n()/nrow(.)*100))
  })
  #create summary of MIDS criteria
  midscrit <- reactive({
    cbind.data.frame(
    names(gbif_dataset_mids_filtered()[ , grep("mids[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE]),
    gbif_dataset_mids_filtered()[ , grep("mids[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE] %>%
      map(~{sum(.x, na.rm = TRUE)}) %>% 
      as.numeric() , 
    gbif_dataset_mids_filtered()[ , grep("mids[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE] %>%  
      map(~{round((sum(.x, na.rm = TRUE) / nrow(gbif_dataset_mids_filtered()))*100)}) %>%
      as.numeric())  %>% 
    set_colnames(c("MIDS_criteria", "Number_of_records","Percentage")) 
  })
  

# Set up plots ------------------------------------------------------------

  #plot mids levels
  midsplot<-reactive({
    ggplot(midssum(), aes(x=MIDS_level, y=Percentage)) + 
    geom_bar(stat = "identity", fill = rgb(0.1,0.4,0.5)) +
    coord_cartesian(xlim = c(-1.5, 3.5)) +
    geom_text(data = subset(midssum(), Percentage >= 5), 
              aes(y = Percentage , label = Percentage),
                vjust = 1.25, colour = "white") +
    geom_text(data = subset(midssum(), Percentage < 5), 
              aes(y = Percentage , label = Percentage),
                vjust = -0.5, colour = "black") +
    labs(x = "MIDS level", y = "Percentage of records at specified MIDS level") +
    ggtitle("MIDS levels") +
    theme(plot.title = element_text(hjust = 0.5) , plot.margin = margin(1, 1, 2, 2, "cm")) 
  })
  
  #plot mids criteria
  midscritsplot<-reactive({
    ggplot(midscrit(), aes(x= MIDS_criteria, y=Percentage)) + 
      geom_bar(stat = "identity", fill = rgb(0.1,0.4,0.5)) + 
      coord_flip() + 
      geom_text(data = subset(midscrit(), Percentage >= 5), 
                aes(y = Percentage, label = Percentage), hjust = 1.25, colour = "white") +
      geom_text(data = subset(midscrit(), Percentage < 5), 
                aes(y = Percentage, label = Percentage), hjust = -0.5, colour = "black") +
      labs(x = "MIDS criteria", y = "Percentage of records that meet the criterium") +
      ggtitle("MIDS criteria") +
      theme(plot.title = element_text(hjust = 0.5) , plot.margin = margin(1, 1, 2, 2, "cm")) 
  })
  

# Create Results tabs -----------------------------------------------------

  #add new tab for each analysis
  observeEvent(input$start, {appendTab("tabs", 
      tabPanel(title = tags$span(paste0("Results", startcounter$countervalue, "    "),
                                 CloseTabUI(paste0("close", startcounter$countervalue))
                                 ), 
               value = paste0("Results", startcounter$countervalue), 
               sidebarLayout(
                 sidebarPanel(
                   helpText("Filter to view MIDS scores for part of the dataset"),
                   sliderInput(paste0("date", startcounter$countervalue), 
                               label = "Filter on collection date:",
                               min = 0, max = 100, value = c(0, 100)),
                   selectizeInput(paste0("country", startcounter$countervalue), 
                                  label = "Filter on countrycode",  
                                  choices = "Nothing yet",
                                  multiple = TRUE),
                   selectInput(paste0("rank", startcounter$countervalue), 
                               label = "Filter on the following taxonomic rank",
                               choices = c("None", "Class", "Order", "Family", "Subfamily", "Genus")),
                   selectizeInput(paste0("taxonomy", startcounter$countervalue), 
                                  label = "Filter on taxonomy", 
                                  choices = "Select a rank first",
                                  multiple = TRUE)
                 ),
                 mainPanel(
                    tabsetPanel(type = "tabs",
                        tabPanel("Plots",
                              plotOutput(paste0("midsplot_prev", startcounter$countervalue)),
                              plotOutput(paste0("midscritsplot", startcounter$countervalue))),
                        tabPanel("Summary tables", 
                              DT::dataTableOutput(paste0("summary", startcounter$countervalue)), 
                              br(),
                              DT::dataTableOutput(paste0("summarycrit", startcounter$countervalue))),
                        tabPanel("Record table", 
                              DT::dataTableOutput(paste0("table", startcounter$countervalue))),
                        tabPanel("Export csv",
                              br(), br(),
                              downloadButton(paste0("downloadData", startcounter$countervalue), "Download all"),
                              br(), br(),
                              downloadButton(paste0("downloadDataFiltered", startcounter$countervalue), "Download filtered dataset"))
                    ),
                  br(),
                  wellPanel(
                  fluidRow(
                    column(6,
                      helpText("Dataset:"),
                      verbatimTextOutput(paste0("Used_dataset", startcounter$countervalue))
                    ),
                    column(6,
                      helpText("MIDS implementation:"),
                      verbatimTextOutput(paste0("Used_MIDS_implementation", startcounter$countervalue)),
                      ViewImplementationUI(paste0("showschema", startcounter$countervalue))
                    )
                  ))
                )
               )
              )
  )
  })
  
  #show which dataset was used
  observe(
    output[[paste0("Used_dataset", startcounter$countervalue)]] <-
      renderText(tools::file_path_sans_ext(isolate(input$gbiffile)[[1]]))
  )
  
  #show basic info on which MIDS implementation was used
  observe(
  output[[paste0("Used_MIDS_implementation", startcounter$countervalue)]] <-
    renderText(
      if (isolate(input$jsonfile) == "default" & isolate(input$interactiveschema) == 0){
       return(paste("Default:", isolate(basename(jsonpath()))))}
      else if (isolate(input$jsonfile) == "custom" & isolate(input$interactiveschema) == 0){
       return(paste("Custom:", isolate(input$customjsonfile[[1]])))}
      else {return("Interactive")}
    )
  )
  
  #show complete MIDS implementation schema in modal window  
  observe(
  ViewImplementationServer(paste0("showschema", as.integer(gsub("Results", "", input$tabs))),
                           allschemas$prev_bins[[as.integer(gsub("Results", "", input$tabs))]][["criteria"]],
                           allschemas$prev_bins[[as.integer(gsub("Results", "", input$tabs))]][["UoM"]]
  ))

  #render all outputs for each results tab
  observe(
    for (count1 in 1:startcounter$countervalue){
      local({
        count <- count1
        #render plots
        output[[paste0("midsplot_prev", count)]] <- renderPlot(midsplot())
        output[[paste0("midscritsplot", count)]] <- renderPlot(midscritsplot())
        #render summaries
        output[[paste0("summary", count)]] <- 
            DT::renderDataTable(midssum(), rownames = FALSE, options = list(dom = 't'))
        output[[paste0("summarycrit", count)]] <- 
            DT::renderDataTable(midscrit(), rownames = FALSE, options = list(dom = 't'))
        #render table
        output[[paste0("table", count)]] <- 
            DT::renderDataTable(gbif_dataset_mids_filtered())
        #downloads
        output[[paste0("downloadData", count)]] <- 
            downloadHandler(
              filename = function() {
                paste0(tools::file_path_sans_ext(input$gbiffile), ".csv")
              },
              content = function(file) {
                write.csv(req(allmidscalc$prev_bins[[as.integer(gsub("Results", "", input$tabs))]]), file, row.names = FALSE)
              })
        output[[paste0("downloadDataFiltered", count)]] <- 
          downloadHandler(
            filename = function() {
              paste0(tools::file_path_sans_ext(input$gbiffile), ".csv")
            },
            content = function(file) {
              write.csv(gbif_dataset_mids_filtered(), file, row.names = FALSE)
            })
      })
    }
  )
  

# Close results tabs ------------------------------------------------------

    observe(
    CloseTabServer(paste0("close", as.integer(gsub("Results", "", input$tabs))), input$tabs, session)
    )
  
    #   #clear associated saved data
    #   allmidscalc$prev_bins[[i]] <- ""
    #   allschemas$prev_bins[[i]] <- ""

}

# Run the app ----
shinyApp(ui = ui, server = server)