library(shiny)
library(ggplot2)
library(DT)
library(shinyjs)
library(sortable)
source(file = "../parse_json_schema.R")
source(file = "../MIDS-calc.R")

#Increase upload limit to 5GB
options(shiny.maxRequestSize = 5000*1024^2)

# Define UI ----
ui <- navbarPage(title=div(tags$img(height = 30, src = "Logo_MeiseBotanicGarden_rgb.jpg"), "Calculate MIDS scores"),
                 useShinyjs(),
                 tabPanel("Submit data",
                          fileInput("gbiffile", "Upload zipped GBIF annotated archive (max 5 GB)",
                                    accept = ".zip"),
                          br(), 
                          radioButtons("jsonfile", label = NULL, 
                                       choiceNames = list("Use default schema", 
                                                          "Upload a custom JSON schema"),
                                       choiceValues = list("default", "custom")),
                          fileInput("customjsonfile", label = NULL,
                                    accept = ".json"),
                          br(),
                          actionButton("start", "Start MIDS score calculations")
                          ),
                 tabPanel("View JSON",
                          tabsetPanel( type = "tabs",
                                       tabPanel("MIDS criteria", verbatimTextOutput("json")),
                                       tabPanel("Unknown or missing values", verbatimTextOutput("jsonUoM"))
                          )
                          ),
                 navbarMenu("Edit JSON",
                  tabPanel("Criteria",
                          fluidRow(
                            column(
                              tags$h1("MIDS criteria"),
                              width = 12,
                              div(
                                class = "bucket-list-container default-sortable",
                                "Drag the properties to the desired MIDS level",
                                selectizeInput("critnewprop", 
                                               label = "Enter a new property you'd like to use here \n Select multiple values at once if they must all be true (&)",  
                                               choices = readLines("www/DWCAcolumnnames.txt"),
                                               multiple = TRUE),
                                actionButton("addcritprop", "Add"),
                                div(
                                  class = "default-sortable bucket-list bucket-list-horizontal",
                                  uiOutput("crit"),
                                  uiOutput("unused"),
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
                  tabPanel("Unknown or Missing",
                          fluidRow(
                            column(
                              tags$h1("MIDS unknown or missing values"),
                              width = 12,
                              div(
                                class = "bucket-list-container default-sortable",
                                "Drag the unknown or missing values to the desired properties",
                                textInput("UoMnewvalue", "Enter a new value you'd like to use here", 
                                          value = "Enter text..."),
                                actionButton("addUoM", "Add"),
                                textInput("UoMnewprop", "Enter a new property you'd like to use here", 
                                          value = "Enter text..."),
                                actionButton("addUoMprop", "Add"),
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
                        )),
                 
                 tabPanel("Results",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Filter to view MIDS scores for part of the dataset"),
                              sliderInput("date", 
                                          label = "Filter on collection date:",
                                          min = 0, max = 100, value = c(0, 100)),
                              selectizeInput("country", 
                                          label = "Filter on countrycode",  
                                          choices = "Nothing yet",
                                          multiple = TRUE),
                              selectInput("rank", 
                                          label = "Filter on the following taxonomic rank",
                                          choices = c("None", "Class", "Order", "Family", "Subfamily", "Genus")),
                              selectizeInput("taxonomy", 
                                          label = "Filter on taxonomy", 
                                          choices = "Select a rank first",
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Plots", plotOutput("midsplot"),
                                                   plotOutput("midscritsplot")),
                                          tabPanel("Summary tables", DT::dataTableOutput("summary"), 
                                                   br(),
                                                   DT::dataTableOutput("summarycrit")),
                                          tabPanel("Record table", 
                                                   DT::dataTableOutput("table"))
                              )
                            )
                          )
                          ),
                 tabPanel("Export csv",
                          downloadButton("downloadData", "Download all"),
                          br(), br(),
                          downloadButton("downloadDataFiltered", "Download filtered dataset"))
)

# Define server logic ----
server <- function(input, output, session) {

  #show and hide json upload button and start button
  observe({
    if (input$jsonfile == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
    if (is.null(input$gbiffile) | (input$jsonfile == "custom" & is.null(input$customjsonfile))){
      shinyjs::hide("start")} else {shinyjs::show("start")}
  })
  
  #get path to json schema
  jsonpath <- reactive({
    if (input$jsonfile == "default"){
      return("../../data/schemas/secondschema_conditions_same_level.json")}
    if (input$jsonfile == "custom"){
      return(input$customjsonfile$datapath)}
  })
  

# Calculations ------------------------------------------------------------

  #calculate mids levels and criteria
  gbif_dataset_mids <- eventReactive(input$start, {
    withProgress(message = 'Calculating MIDS scores', value = 0, {
      calculate_mids(gbiffile = input$gbiffile$datapath, jsonfile = jsonpath())
    })
  })
  

  #json schema
  jsonschema <- reactive({ 
    read_json_mids_criteria(file = jsonpath(), outtype = "criteria")
  })
  
  #json UoM
  jsonUoM <- reactive({ 
    read_json_unknownOrMissing(file = jsonpath())
  })
  

# View JSON ---------------------------------------------------------------

  #show json schema
  output$json <- renderPrint(jsonschema())
  
  #show json UoM
  output$jsonUoM <- renderPrint(jsonUoM())
  

# Edit JSON ---------------------------------------------------------------
  

# Edit Unknown or Missing section -----------------------------------------

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
                        midscritname, options = sortable_options(group = "midsproperties"))
      midscritranks <- c(midscritranks, midscritrank)
    }
    v[[i]] <- rank_list(midslevel, midscritranks, midslevel,
                        options = sortable_options(group = "midscriteria"))
  }
  return(v)
  })
  output$crit <- renderUI(critranklists())
  

  ## add properties specified by user (and keep existing values)
  existingcrit <- eventReactive(input$addcritprop, {input$unused}) 
  newcrit <- eventReactive(input$addcritprop, {paste(input$critnewprop, collapse = " & ")})
  output$unused <- renderUI(rank_list("Unused values", c(existingcrit(), newcrit()), 
                          "unused", options = sortable_options(group = "midsproperties")))
  
  
  ## get inputs
  critinputs <- reactive({x <- list()
  #get values for each criterium
  for (i in 1:length(jsonschema())){
    for (j in 1:length(jsonschema()[[i]])){
      value <-  reactiveValuesToList(input)[names(jsonschema()[[i]][j])]
     #don't include empty properties
      if (rlang::is_empty(value[[1]])){next}
      x[[names(jsonschema()[i])]][[names(jsonschema()[[i]][j])]] <- value[[1]]
    }
  }
  return(x)
  })
  
  #show output
  output$results_3 <-
    renderPrint(
      critinputs()  
    )

# Filters -----------------------------------------------------------------

  #Setting filters when new analysis is started
  observeEvent(input$start, {
    #reset rank filter when new dataset is provided 
    updateSelectInput(session, "rank",
                    selected = "None")
    #update country filter with countries from the dataset
    updateSelectInput(session, "country", label = "Filter on countrycode", 
                      choices = sort(unique(gbif_dataset_mids()$countryCode)))
    #update date filter with dates from the dataset
    updateSliderInput(session, "date", label = "Filter on collection date", 
                      min = min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                      max = max(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                      value = c(min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                                max(gbif_dataset_mids()$eventDate, na.rm = TRUE)),
                      timeFormat = "%m/%d/%Y")
  })
  
  #update taxonomy filter when a taxonomic rank is chosen
  observeEvent(input$rank, {
    if (input$rank != "None"){
      updateSelectInput(session, "taxonomy", label = "Filter on taxonomy",
                        choices = sort(unique(gbif_dataset_mids()[[tolower(input$rank)]])))
      shinyjs::show("taxonomy")
    }
    else {shinyjs::hide("taxonomy")}
  })

  #apply filters if they are set
  gbif_dataset_mids_filtered <- reactive({
    gbif_dataset_mids() %>%
    {if (!is.null(input$country) && input$country != "All") {
        filter(., countryCode %in% input$country)} else {.}} %>%
    {if (input$date[1] != min(gbif_dataset_mids()$eventDate, na.rm = TRUE)){
        filter(., eventDate >= input$date[1])} else {.}} %>%
    {if (input$date[2] != max(gbif_dataset_mids()$eventDate, na.rm = TRUE)) {
        filter(., eventDate <= input$date[2])} else {.}} %>%
    {if (input$rank != "None" && !is.null(input$taxonomy) && input$taxonomy != "All"){
        filter(., .data[[tolower(input$rank)]] %in% input$taxonomy)} else {.}}
  })
  

# MIDS calculation outputs ------------------------------------------------

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
  
  #plot mids levels
  output$midsplot<-renderPlot({
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
  output$midscritsplot<-renderPlot({
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
  
  #summary of mids levels
  output$summary <- DT::renderDataTable(
    midssum(), rownames = FALSE, options = list(dom = 't')
  )
  #summary of mids criteria
  output$summarycrit <- DT::renderDataTable(
    midscrit(), rownames = FALSE, options = list(dom = 't', pageLength = -1)
  )
  #records table with mids levels and criteria
  output$table <- DT::renderDataTable({
    gbif_dataset_mids_filtered()
  })
  
# Downloads ---------------------------------------------------------------

  #download csv of record table with mids levels
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$gbiffile), ".csv")
    },
    content = function(file) {
      write.csv(gbif_dataset_mids(), file, row.names = FALSE)
  })
  
  #download csv of filtered record table with mids levels
  output$downloadDataFiltered <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$gbiffile), "_filtered.csv")
    },
    content = function(file) {
      write.csv(gbif_dataset_mids_filtered(), file, row.names = FALSE)
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)