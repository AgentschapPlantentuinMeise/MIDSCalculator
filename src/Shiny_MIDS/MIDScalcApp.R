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
  tags$style(
    HTML("
        .modal {
          overflow:auto
        }
        .modal-backdrop {
          visibility: hidden !important;
        }
        .modal.in {
            background-color: rgba(0,0,0,0.5);
        }
     ")
  ),
  navbarPage(title=div(tags$img(style = "margin: 0px 25px 0px 0px", height = 20, 
                      src = "Logo_klein_BotanicGardenMeise_cmyk.png"), "Calculate MIDS scores"),
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
                          checkboxInput("editschema", "Edit interactively", 
                                        value = FALSE),
                          fluidRow(column(5, 
                                          InteractiveSchemaUI("interactive")
                                          ),
                          column(7, ViewImplementationUI("viewcurrentschema"))
                          )))),
                          br(),br(),
                          actionButton("start", "Start MIDS score calculations"),
                          align="center")
                          )
))

# Define server logic ----
server <- function(input, output, session) {
  
  #show and hide schema file upload and disable/enable start button
  observe({
    #hide schema upload when schema is default
    if (input$jsonfile == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
    #disable start when there is no input file, when interactive is chosen but not visited, or when custom upload is chosen but empty
    if (is.null(input$gbiffile) |
        (input$editschema == TRUE && jsonlist$visited() == FALSE) |
        (input$jsonfile == "custom" & is.null(input$customjsonfile))){
      shinyjs::disable("start")} else {shinyjs::enable("start")}
  })
    
  #disable "View MIDS implementation" when custom upload is chosen but empty, and when edit schema is chosen but not visited
  disableviewschema <- reactiveVal(FALSE)
  observe({
    if (input$jsonfile == "custom" & is.null(input$customjsonfile) | 
        (input$editschema == TRUE && jsonlist$visited() == FALSE)) {
      disableviewschema(TRUE)}
    else {
      disableviewschema(FALSE)}
    #never disable the view button on results tabs
    if (grepl("Results", input$tabs))
    {disableviewschema(FALSE)}
  })
  
  #disable "Edit MIDS implementation" if schema doesn't need to be edited and when custom upload is chosen but empty 
  disableinteractive <- reactiveVal(FALSE)
  observe({
    if ((input$jsonfile == "custom" & is.null(input$customjsonfile)) |
        input$editschema == FALSE){
      disableinteractive(TRUE)}
    else {
      disableinteractive(FALSE)}
  })


# Initialize MIDS implementation ------------------------------------------

  #get path to json schema
  jsonpath <- reactive({
    if (input$jsonfile == "default" | is.null(input$customjsonfile$datapath)){
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
  if (input$editschema == TRUE){
    ViewImplementationServer("viewcurrentschema", reactive(jsonlist$jsonlist()[[1]]), reactive(jsonlist$jsonlist()[[2]]), disableviewschema)
  #show schema from file
  } else {
    ViewImplementationServer("viewcurrentschema", jsonschema, jsonUoM, disableviewschema)
  })
    

# Edit MIDS implementation interactively ----------------------------------
  
  jsonlist <- InteractiveSchemaServer("interactive", jsonschema, jsonUoM, disableinteractive)
  
  
# Calculations ------------------------------------------------------------
  
  #calculate mids levels and criteria
  gbif_dataset_mids <- eventReactive(input$start, {
    if (input$editschema == FALSE){
      withProgress(message = 'Calculating MIDS scores', value = 0, {
        calculate_mids(gbiffile = input$gbiffile$datapath, jsonfile = jsonpath())})
    }
    else if (input$editschema == TRUE){
      withProgress(message = 'Calculating MIDS scores', value = 0, {
        calculate_mids(gbiffile = input$gbiffile$datapath, jsontype = "list", jsonlist = jsonlist$jsonlist() )})
    }
  })
  
  
# Allow multiple results tabs --------------------------------------------

  #count how many times start is clicked
  startcounter <- reactiveValues(countervalue = 0)
  observeEvent(input$start, {
    startcounter$countervalue <- startcounter$countervalue + 1})
  
  #save all MIDS calculations
  allmidscalc <- reactiveValues(prev_bins = NULL)
  observeEvent(input$start, {
    allmidscalc$prev_bins[[paste0("res", startcounter$countervalue)]] <- gbif_dataset_mids()
  })
  
  #save all MIDS implementations
  allschemas <- reactiveValues(prev_bins = NULL)
  observeEvent(input$start, {
    if (input$editschema == TRUE){
      allschemas$prev_bins[[paste0("res", startcounter$countervalue)]] <- jsonlist$jsonlist()[1:2]
    } else {
      allschemas$prev_bins[[paste0("res", startcounter$countervalue)]] <- c(list("criteria" = jsonschema()), list("UoM" = jsonUoM()))
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
  
  resulttabnr <- reactive({if (grepl("Results", input$tabs)) {as.integer(gsub("Results", "", input$tabs))}})
  
  #update taxonomy filter when a taxonomic rank is chosen
  observeEvent(input[[paste0("rank", resulttabnr())]], {
    if (input[[paste0("rank", resulttabnr())]] != "None"){
      #update taxonomy filter with values from the dataset
      updateSelectInput(session, paste0("taxonomy", resulttabnr()), label = "Filter on taxonomy",
          choices = sort(unique(req(allmidscalc$prev_bins[[paste0("res", resulttabnr())]])[[tolower(input[[paste0("rank", resulttabnr())]])]])))
      #only show taxonomy filter when a rank is chosen
      shinyjs::show(paste0("taxonomy", resulttabnr()))
    } else {shinyjs::hide(paste0("taxonomy", resulttabnr()))}
  })
  
  #apply filters if they are set
  gbif_dataset_mids_filtered <- reactive({
    req(allmidscalc$prev_bins[[paste0("res", resulttabnr())]]) %>%
    {if (!is.null(input[[paste0("country", resulttabnr())]]) && input[[paste0("country", resulttabnr())]] != "All") {
        filter(., countryCode %in% input[[paste0("country", resulttabnr())]])} else {.}} %>%
    {if (req(input[[paste0("date", resulttabnr())]][1]) != 0) {
      filter(., eventDate >= input[[paste0("date", resulttabnr())]][1])} else {.}} %>%
    {if (req(input[[paste0("date", resulttabnr())]][2]) != 100) {
      filter(., eventDate <= input[[paste0("date", resulttabnr())]][2])} else {.}} %>%
    {if (input[[paste0("rank", resulttabnr())]] != "None" && !is.null(input[[paste0("taxonomy", resulttabnr())]]) && input[[paste0("taxonomy", resulttabnr())]] != "All"){
        filter(., .data[[tolower(input[[paste0("rank", resulttabnr())]])]] %in% input[[paste0("taxonomy", resulttabnr())]])} else {.}}
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
                                  selected = "Select a rank first",
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
      if (isolate(input$jsonfile) == "default" & isolate(input$editschema) == FALSE){
       return(paste("Default:", isolate(basename(jsonpath()))))}
      else if (isolate(input$jsonfile) == "custom" & isolate(input$editschema) == FALSE){
       return(paste("Custom:", isolate(input$customjsonfile[[1]])))}
      else {return("Interactive")}
    )
  )
  
  #show complete MIDS implementation schema in modal window  
  observe(
  ViewImplementationServer(paste0("showschema", resulttabnr()),
                           allschemas$prev_bins[[paste0("res", resulttabnr())]][["criteria"]],
                           allschemas$prev_bins[[paste0("res", resulttabnr())]][["UoM"]],
                           disableviewschema
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
                write.csv(req(allmidscalc$prev_bins[[paste0("res", resulttabnr())]]), file, row.names = FALSE)
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
  

# Open results tab automatically when calculations are performed ----------

  observeEvent(input$start, {
    updateTabsetPanel(session, "tabs",
                      selected = paste0("Results", startcounter$countervalue))
  })

# Close results tabs ------------------------------------------------------

    #close tabs
    clear <- reactiveValues()
    observe({
      if (grepl("Results", input$tabs)){
        i <- gsub("Results", "", input$tabs)
        clear[[paste0("res", i)]] <- CloseTabServer(paste0("close", i), i, session)
      }
    }
    )
    
    #clear associated saved data
    observe(
      if (grepl("Results", input$tabs)){
      i <- gsub("Results", "", input$tabs)
        if (req(clear[[paste0("res", i)]]()$value) == "clear"){
          allmidscalc$prev_bins[[paste0("res", i)]] <- NULL
          allschemas$prev_bins[[paste0("res", i)]] <- NULL
        }
      }
    )

}

# Run the app ----
shinyApp(ui = ui, server = server)