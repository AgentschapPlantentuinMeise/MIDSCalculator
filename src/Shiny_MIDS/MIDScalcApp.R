#check if all packages are installed
source("../packages.R")
pkgLoad()

#Load libraries and source files
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
  #change style of modals to fix scroll bar behavior and backdrop of nested of modals
  tags$style(
    HTML("
        body {
          padding-right:0px !important;
        }
        .modal {
          overflow:auto;
        }
        .modal-open {
          overflow:auto;
        }
        .modal-backdrop {
          visibility: hidden !important;
        }
        .modal.in {
            background-color: rgba(0,0,0,0.5);
        }
     ")
  ),
  navbarPage(
   title=div(tags$img(style = "margin: 0px 25px 0px 0px", height = 20, 
                      src = "Logo_klein_BotanicGardenMeise_cmyk.png"),
             tags$span("Calculate MIDS scores",
                       actionButton("info",
                          icon("info"),
                          style = "padding:0px 5px 15px 5px; font-size:65%; border-style: none"
                          )), 
             ),
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
            radioButtons("jsonfiletype", label = NULL, 
                         choiceNames = list("Use default", 
                                            "Upload custom"),
                         choiceValues = list("default", "custom")),
            fileInput("customjsonfile", label = NULL, accept = ".json"),
            checkboxInput("editschema", "Edit interactively", value = FALSE),
            fluidRow(
            column(5, InteractiveSchemaUI("interactive")),
            column(7, ViewImplementationUI("viewcurrentschema"))
            )))),
            br(),br(),
            ResultsUI("start"),
            align="center")
            )
))

# Define server logic ----
server <- function(input, output, session) {
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  

# Show information about the app ------------------------------------------

  observeEvent(input$info,{ 
    #show modal
    showModal(modalDialog(
      title = "About",
      HTML(paste0(h4('Submit data'), '
        On this page a (zipped) GBIF annotated Darwin Core Archive can be uploaded. In addition, the MIDS implementation can be specified and viewed. To specify the MIDS implementation you can either choose the default schema (included in the app) or upload your own file. It is also possible to choose to edit this schema interactively. The interactive editing opens in a pop-up window, where in a first tab, MIDS elements can be added, removed, or moved to another MIDS level. In addition, mappings can be removed or added by clicking the "edit" icon of a MIDS element. In a second tab, the Unknown or Missing section of the schema can be edited, i.e. new properties and new values can be added. This interactively edited schema can be saved to file (JSON). The schema (be it default, custom or interactive) can be viewed by clicking "Show MIDS implementation", which opens a human-friendly visualization of the MIDS schema, so that it is not necessary to read the JSON file to be able to understand the specifics of the MIDS schema used. Once a dataset and a MIDS implementation have been chosen, calculations can be started by clicking "Start MIDS score calculations".
        ', br(), h4('Results'), '
        The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code /taxonomic group/ collection date. The filename of the dataset is shown, as well as the used MIDS implementation, to make the provenance of the calculations clear.'
      )),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  

# Check dataset file ------------------------------------------------------

  disablesInvalidFile <- reactiveVal(FALSE)
  observeEvent(input$gbiffile, {
    if(!tools::file_ext(input$gbiffile$datapath) %in% c("zip", "txt", "csv")) {
      showModal(modalDialog(
        title = "Invalid input",
        "File must be a zip, txt or csv file"
      ))
      disablesInvalidFile(TRUE)
    } else if (tools::file_ext(input$gbiffile$datapath) == "zip" && !"occurrence.txt" %in% unzip(zipfile = input$gbiffile$datapath, list = TRUE)$Name) {
      showModal(modalDialog(
        title = "Invalid input",
        "Zip file must contain occurence.txt file"
      ))
      disablesInvalidFile(TRUE)
    } else {disablesInvalidFile(FALSE)}
  })
  
  
# Enable / disable action buttons -----------------------------------------

  #hide schema upload when schema is default
  observe({
    if (input$jsonfiletype == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
  })
  
  #check if file is uploading
  hold <- reactiveVal(FALSE)
  onclick("gbiffile", {hold(TRUE)})
  observeEvent(input$gbiffile, {hold(FALSE)})
  
  #disable start when there is no input file, when interactive is chosen but not visited, 
  #or when custom upload is chosen but empty, or when file is uploading
  #or when invalid dataset file is uploaded
  disablestart <- reactiveVal(FALSE)
  observe({
    if (is.null(input$gbiffile) |
        (input$editschema == TRUE && interactiveschema$visited() == FALSE) |
        (input$jsonfiletype == "custom" & is.null(input$customjsonfile)) |
        hold() == TRUE|
        disablesInvalidFile() == TRUE){
      disablestart(TRUE)} else {disablestart(FALSE)}
  })
    
  #disable "View MIDS implementation" when custom upload is chosen but empty, and when edit schema is chosen but not visited
  disableviewschema <- reactiveVal(FALSE)
  observe({
    if (input$jsonfiletype == "custom" & is.null(input$customjsonfile) | 
        (input$editschema == TRUE && interactiveschema$visited() == FALSE)) {
      disableviewschema(TRUE)} else {disableviewschema(FALSE)}
  })
  
  #disable "Edit MIDS implementation" if schema doesn't need to be edited and when custom upload is chosen but empty 
  disableinteractive <- reactiveVal(FALSE)
  observe({
    if ((input$jsonfiletype == "custom" & is.null(input$customjsonfile)) |
        input$editschema == FALSE){
      disableinteractive(TRUE)} else {disableinteractive(FALSE)}
  })
  
# Initialize MIDS implementation ------------------------------------------

  #get path to json schema
  jsonpath <- reactive({
    if (input$jsonfiletype == "default" | is.null(input$customjsonfile$datapath)){
      return(paste0("../../",default_schema))}
    if (input$jsonfiletype == "custom"){
      return(input$customjsonfile$datapath)}
  })
  
  #read json schema from file
  jsonschemafile <- reactive({ 
    read_json_mids_criteria(schema = jsonpath(), outtype = "criteria")
  })
  
  #read json UoM from file
  jsonUoMfile <- reactive({ 
    read_json_unknownOrMissing(schema = jsonpath())
  })
  

# Get final MIDS implementation schema (either from file or from interactive editing) --------

  jsonschemafinal <- reactive({ 
    if (input$editschema == TRUE){
      # get interactive schema
      return(c(list("criteria" = read_json_mids_criteria(schema = interactiveschema$interactivejson(), outtype = "criteria", type = "interactive")), 
               list("UoM" = read_json_unknownOrMissing(schema = interactiveschema$interactivejson(), type = "interactive")), 
               list("properties" = read_json_mids_criteria(schema = interactiveschema$interactivejson(), outtype = "properties", type = "interactive"))
              ))
    } else {
      #get schema from file
      #get filename
      if (input$jsonfiletype == "custom"){
        filename <- paste("Custom:", input$customjsonfile$name)
      } else {
        filename <- paste("Default:", basename(jsonpath()))
      }
      #return schema
      return(c(list("criteria" = jsonschemafile()), list("UoM" = jsonUoMfile()), 
        list("properties" = read_json_mids_criteria(schema = jsonpath(), out = "properties")),
        list("filename"= filename)))
    }
  })
  
  
# Show current MIDS implementation ----------------------------------------
  
  #view MIDS implementation in modal window
  ViewImplementationServer("viewcurrentschema", jsonschemafinal, disableviewschema)
    

# Edit MIDS implementation interactively ----------------------------------
  
  interactiveschema <- InteractiveSchemaServer("interactive", jsonschemafile, 
                                               jsonUoMfile, disableinteractive)
  

# Calculate and show results ----------------------------------------------

  ResultsServer("start", session, reactive(input$gbiffile), jsonschemafinal,
                reactive(input$tabs), disablestart)
 
}
# Run the app ----
shinyApp(ui = ui, server = server)