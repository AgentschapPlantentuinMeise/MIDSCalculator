library(shiny)
library(ggplot2)
library(DT)
source(file = "../parse_json_schema.R")
source(file = "../MIDS-calc.R")

#Increase upload limit to 5GB
options(shiny.maxRequestSize = 5000*1024^2)

# Set path to schema
jsonpath <- "../../data/schemas/secondschema_conditions_same_level.json"

# Define UI ----
ui <- navbarPage("Calculate MIDS scores",
                 tabPanel("Submit dataset",
                          fileInput("gbiffile", "Upload GBIF annotated archive",
                                    accept = ".zip"),
                          ),
                 tabPanel("Results",
                          sidebarLayout(
                            sidebarPanel(
                              #placeholders
                              helpText("Filter to calculate MIDS scores for part of the dataset"),
                              sliderInput("range", 
                                          label = "Range of interest:",
                                          min = 0, max = 100, value = c(0, 100)),
                              selectInput("var", 
                                          label = "Choose a variable to display",
                                          choices = list("Percent White", 
                                                         "Percent Black",
                                                         "Percent Hispanic", 
                                                         "Percent Asian"),
                                          selected = "Percent White")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Plot", plotOutput("midsplot"),
                                                   plotOutput("midscritsplot")),
                                          tabPanel("Summary", DT::dataTableOutput("summary"), 
                                                   DT::dataTableOutput("summarycrit")),
                                          tabPanel("Table", 
                                                   DT::dataTableOutput("table"))
                              )
                            )
                          )
                          ),
                 tabPanel("Export csv",
                          downloadButton("downloadData", "Download"))
)

# Define server logic ----
server <- function(input, output) {
  
  #calculate mids levels
  gbif_dataset_mids <- reactive({
    withProgress(message = 'Calculating MIDS scores', value = 0, {
      calculate_mids(gbiffile = input$gbiffile$datapath, jsonfile = jsonpath)
    })
  })
  #create summary of MIDS levels
  midssum <- reactive({
    gbif_dataset_mids() %>% group_by(mids_level) %>% summarise(n = n(), percentage = round(n()/nrow(.)*100))
  })
  #create summary of MIDS criteria
  midscrit <- reactive({
    gbif_dataset_mids()[ , grep("mids[0-3]", names(gbif_dataset_mids())), with = FALSE] %>% 
      map(~{round((sum(.x, na.rm = TRUE) / nrow(gbif_dataset_mids()))*100)}) %>%
      as.data.table() %>%
      t()
  })
  
  #plot mids levels
  output$midsplot<-renderPlot({
    ggplot(midssum(), aes(x=mids_level, y=n)) + 
    geom_bar(stat = "identity", fill = rgb(0.1,0.4,0.5)) +
    coord_cartesian(xlim = c(-1.5, 3.5)) +
    geom_text(data = midssum(), 
              aes(y = n + sum(n)*0.02, label = paste0(n, " (", round((n/sum(n))*100),"%)")))
  })
  #plot mids criteria
  output$midscritsplot<-renderPlot({
    ggplot(as.data.frame(midscrit()), aes(x= rownames(midscrit()), y=V1)) + 
      geom_bar(stat = "identity", fill = rgb(0.1,0.4,0.5)) + 
      coord_flip() + 
      geom_text(data = as.data.frame(midscrit()), 
                aes(y = V1, label = V1), hjust = 1.25, colour = "white")
  })
  
  #summary of mids levels
  output$summary <- DT::renderDataTable({
    midssum()
  })
  #summary of mids criteria
  output$summarycrit <- DT::renderDataTable({
    midscrit()
  })
  #records table with mids levels and criteria
  output$table <- DT::renderDataTable({
    gbif_dataset_mids()
  })
  
  #download csv of record table with mids levels
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(input$gbiffile), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(gbif_dataset_mids(), file, row.names = FALSE)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)