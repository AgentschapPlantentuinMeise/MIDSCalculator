#Load libraries and source files
library(shiny)
library(shinyBS)
library(ggplot2)
library(DT)
library(shinyjs)
library(sortable)
library(RColorBrewer)
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
             "Calculate MIDS scores"),
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
  
# Enable / disable action buttons -----------------------------------------

  #hide schema upload when schema is default
  observe({
    if (input$jsonfiletype == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
  })
  
  #disable start when there is no input file, when interactive is chosen but not visited, or when custom upload is chosen but empty
  disablestart <- reactiveVal(FALSE)
  observe({
    if (is.null(input$gbiffile) |
        (input$editschema == TRUE && interactiveschema$visited() == FALSE) |
        (input$jsonfiletype == "custom" & is.null(input$customjsonfile))){
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
      return("../../data/schemas/secondschema_conditions_same_level.json")}
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
  
  #get final MIDS implementation schema (either from file or from interactive editing)
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