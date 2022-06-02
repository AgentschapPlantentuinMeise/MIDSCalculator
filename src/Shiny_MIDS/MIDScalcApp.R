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
                          ResultsUI("start"),
                          align="center")
                          )
))

# Define server logic ----
server <- function(input, output, session) {
  
  #hide schema upload when schema is default
  observe({
    if (input$jsonfile == "default"){
      shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
  })
  #disable start when there is no input file, when interactive is chosen but not visited, or when custom upload is chosen but empty
  disablestart <- reactiveVal(FALSE)
  observe({
    if (is.null(input$gbiffile) |
        (input$editschema == TRUE && jsonlist$visited() == FALSE) |
        (input$jsonfile == "custom" & is.null(input$customjsonfile))){
      disablestart(TRUE)} else {disablestart(FALSE)}
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
  

# Calculate and show results ----------------------------------------------

 ResultsServer("start", session, reactive(input$gbiffile$datapath), reactive(input$gbiffile$name), 
                reactive(input$editschema), jsonpath, reactive(input$customjsonfile$name), reactive(jsonlist$jsonlist()),
                jsonschema, jsonUoM, reactive(input$jsonfile), reactive(input$tabs), disableviewschema, disablestart)
  
 
}
# Run the app ----
shinyApp(ui = ui, server = server)