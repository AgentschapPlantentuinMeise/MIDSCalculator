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
ui <- navbarPage(title=div(tags$img(height = 30, src = "Logo_MeiseBotanicGarden_rgb.jpg"), "Calculate MIDS scores"),
                 tabPanel("Submit dataset",
                          fileInput("gbiffile", "Upload zipped GBIF annotated archive (max 5 GB)",
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
                          downloadButton("downloadData", "Download"))
)

# Define server logic ----
server <- function(input, output) {
  
  #calculate mids levels and criteria
  gbif_dataset_mids <- reactive({
    withProgress(message = 'Calculating MIDS scores', value = 0, {
      calculate_mids(gbiffile = input$gbiffile$datapath, jsonfile = jsonpath)
    })
  })
  #create summary of MIDS levels
  midssum <- reactive({
    gbif_dataset_mids() %>% group_by(MIDS_level) %>% summarise(Number_of_records = n(), Percentage = round(n()/nrow(.)*100))
  })
  #create summary of MIDS criteria
  midscrit <- reactive({
    cbind.data.frame(
    names(gbif_dataset_mids()[ , grep("mids[0-3]", names(gbif_dataset_mids())), with = FALSE]),
    gbif_dataset_mids()[ , grep("mids[0-3]", names(gbif_dataset_mids())), with = FALSE] %>%
      map(~{sum(.x, na.rm = TRUE)}) %>% 
      as.numeric() , 
    gbif_dataset_mids()[ , grep("mids[0-3]", names(gbif_dataset_mids())), with = FALSE] %>%  
      map(~{round((sum(.x, na.rm = TRUE) / nrow(gbif_dataset_mids()))*100)}) %>%
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