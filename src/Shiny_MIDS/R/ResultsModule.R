ResultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("start"), "Start MIDS score calculations")
  )
}

ResultsServer <- function(id, parent.session, gbiffile, gbiffilename, edit, 
                          jsonpath, customjsonname, jsonschema, jsonfile, 
                          tabs, disableviewschema, disablestart) {
  moduleServer(id, function(input, output, module.session) {
    ns <- module.session$ns
    

# Enable/ disable start action button -------------------------------------

    observe(
      if (disablestart() == TRUE){
        shinyjs::disable("start")
      } else {
        shinyjs::enable("start")
      })
    
# Calculations ------------------------------------------------------------
    
    #calculate mids levels and criteria
    gbif_dataset_mids <- eventReactive(input$start, {
      if (edit() == FALSE){
        withProgress(message = 'Calculating MIDS scores', value = 0, {
          calculate_mids(gbiffile = gbiffile(), jsonfile = jsonpath())})
      }
      else if (edit() == TRUE){
        withProgress(message = 'Calculating MIDS scores', value = 0, {
          calculate_mids(gbiffile = gbiffile(), jsontype = "list", jsonlist = jsonschema() )})
      }
    })
    
# Allow multiple results tabs --------------------------------------------
    
    #save all MIDS calculations
    allmidscalc <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      allmidscalc$prev_bins[[paste0("res", input$start)]] <- gbif_dataset_mids()
    })
    
    #save all MIDS implementations
    allschemas <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      allschemas$prev_bins[[paste0("res", input$start)]] <- jsonschema()[1:2]
    })
    
    #get which result tab is active
    resulttabnr <- reactive({
      if (grepl("Results", tabs())){
        as.integer(gsub("start-Results", "", tabs()))
      }
    })
    
# Filters -----------------------------------------------------------------
    
    #Initialize filters when new analysis is started
    observeEvent(input$start, {
      #reset rank filter when new dataset is provided
      updateSelectInput(parent.session, ns(paste0("rank", input$start)),
                        selected = "None")
      #update country filter with countries from the dataset
      updateSelectInput(parent.session, ns(paste0("country", input$start)), label = "Filter on countrycode",
                        choices = sort(unique(gbif_dataset_mids()$countryCode)))
      #update date filter with dates from the dataset
      updateSliderInput(parent.session, ns(paste0("date", input$start)), label = "Filter on collection date",
                        min = min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                        max = max(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                        value = c(min(gbif_dataset_mids()$eventDate, na.rm = TRUE),
                                  max(gbif_dataset_mids()$eventDate, na.rm = TRUE)),
                        timeFormat = "%m/%d/%Y")
    })
    
    #update taxonomy filter when a taxonomic rank is chosen
    observeEvent(input[[paste0("rank", resulttabnr())]], {
      if (input[[paste0("rank", resulttabnr())]] != "None"){
        #update taxonomy filter with values from the dataset
        updateSelectInput(parent.session, ns(paste0("taxonomy", resulttabnr())), label = "Filter on taxonomy",
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
      gbif_dataset_mids_filtered() %>% group_by(MIDS_level) %>% 
        summarise(Number_of_records = n(), Percentage = round(n()/nrow(.)*100))
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
        set_colnames(c("MIDS_elements", "Number_of_records","Percentage")) 
    })
    
# Set up plots ------------------------------------------------------------
    
    #define custom color scales
    #MIDS levels
    myColors <- brewer.pal(6, "Blues")[2:6]
    names(myColors) <- c("-1", "0", "1", "2", "3")
    custom_colors <- scale_fill_manual(name = "MIDS level", values = myColors)
    #MIDS criteria
    myColors2 <-  myColors[2:5]
    names(myColors2) <- c("0", "1", "2", "3")
    custom_colors2 <- scale_fill_manual(name = "MIDS level", values = myColors2)
    
    #plot mids levels
    midsplot<-reactive({
      ggplot(midssum(), aes(x=MIDS_level, y=Percentage, fill = factor(MIDS_level))) + 
        geom_bar(stat = "identity") +
        custom_colors +
        coord_cartesian(xlim = c(-1.5, 3.5)) +
        geom_text(data = subset(midssum(), Percentage >= 5), 
                  aes(y = Percentage , label = Percentage),
                  vjust = 1.25, colour = "white") +
        geom_text(data = subset(midssum(), Percentage < 5), 
                  aes(y = Percentage , label = Percentage),
                  vjust = -0.5, colour = "black") +
        labs(x = "MIDS level", y = "Percentage of records at specified MIDS level") +
        ggtitle("MIDS levels") +
        theme(plot.title = element_text(hjust = 0.5, size = 25) , 
              plot.margin = margin(1, 1, 2, 2, "cm"),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15)) 
    })
    
    #plot mids criteria
    midscritsplot<-reactive({
      ggplot(midscrit(), aes(x= MIDS_elements, y=Percentage, fill = factor(substr(MIDS_elements, 5, 5)))) + 
        geom_bar(stat = "identity") + 
        custom_colors2 +
        coord_flip() + 
        geom_text(data = subset(midscrit(), Percentage >= 5), 
                  aes(y = Percentage, label = Percentage), hjust = 1.25, colour = "white") +
        geom_text(data = subset(midscrit(), Percentage < 5), 
                  aes(y = Percentage, label = Percentage), hjust = -0.5, colour = "black") +
        labs(x = "MIDS elements", y = "Percentage of records that meet the element") +
        scale_x_discrete(labels = substr(midscrit()$MIDS_elements, 6, max(nchar(midscrit()$MIDS_elements)))) +
        ggtitle("MIDS elements") +
        theme(plot.title = element_text(hjust = 0.5, size = 25) , 
              plot.margin = margin(1, 1, 2, 2, "cm"),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15)) 
    })
    
    
# Create Results tabs -----------------------------------------------------
    
    #add new tab for each analysis
    observeEvent(input$start, {appendTab(
       session = parent.session, "tabs", 
       tabPanel(
         title = tags$span(paste0("Results", input$start, "    "),
                          CloseTabUI(ns(paste0("close", input$start)))), 
         value = ns(paste0("Results", input$start)), 
         sidebarLayout(
           sidebarPanel(
             helpText("Filter to view MIDS scores for part of the dataset"),
             sliderInput(ns(paste0("date", input$start)), 
                         label = "Filter on collection date:",
                         min = 0, max = 100, value = c(0, 100)),
             selectizeInput(ns(paste0("country", input$start)), 
                            label = "Filter on countrycode",  
                            choices = "Nothing yet",
                            multiple = TRUE),
             selectInput(ns(paste0("rank", input$start)), 
                         label = "Filter on the following taxonomic rank",
                         choices = c("None", "Class", "Order", "Family", "Subfamily", "Genus")),
             selectizeInput(ns(paste0("taxonomy", input$start)), 
                            label = "Filter on taxonomy", 
                            choices = "Select a rank first",
                            selected = "Select a rank first",
                            multiple = TRUE)
           ),
           mainPanel(
            tabsetPanel(type = "tabs",
             tabPanel("Plots",
                      plotOutput(ns(paste0("midsplot_prev", input$start))),
                      plotOutput(ns(paste0("midscritsplot", input$start)))),
             tabPanel("Summary tables", 
                      DT::dataTableOutput(ns(paste0("summary", input$start))), 
                      br(),
                      DT::dataTableOutput(ns(paste0("summarycrit", input$start)))),
             tabPanel("Record table", 
                      DT::dataTableOutput(ns(paste0("table", input$start)))),
             tabPanel("Export csv",
                      br(), br(),
                      downloadButton(ns(paste0("downloadData", input$start)), "Download all"),
                      br(), br(),
                      downloadButton(ns(paste0("downloadDataFiltered", input$start)), "Download filtered dataset"))
             ),
             br(),
             wellPanel(
               fluidRow(
                 column(6,
                        helpText("Dataset:"),
                        verbatimTextOutput(ns(paste0("Used_dataset", input$start)))
                 ),
                 column(6,
                        helpText("MIDS implementation:"),
                        verbatimTextOutput(ns(paste0("Used_MIDS_implementation", input$start))),
                        ViewImplementationUI(ns(paste0("showschema", input$start)))
                 )
               ))
           )
         )
         )
    )
      

# Render outputs ----------------------------------------------------------


      #render plots
      output[[paste0("midsplot_prev", input$start)]] <- renderPlot(midsplot())
      output[[paste0("midscritsplot", input$start)]] <- renderPlot(midscritsplot())
      #render summaries
      output[[paste0("summary", input$start)]] <- 
        DT::renderDataTable(midssum(), rownames = FALSE, options = list(dom = 't'))
      output[[paste0("summarycrit", input$start)]] <- 
        DT::renderDataTable(midscrit(), rownames = FALSE, options = list(dom = 't'))
      #render table
      output[[paste0("table", input$start)]] <- 
        DT::renderDataTable(gbif_dataset_mids_filtered())
      #downloads
      output[[paste0("downloadData", input$start)]] <- 
        downloadHandler(
          filename = function() {
            paste0("download", ".csv")
            #paste0(tools::file_path_sans_ext(gbiffilename()), ".csv")
          },
          content = function(file) {
            write.csv(req(allmidscalc$prev_bins[[paste0("res", resulttabnr())]]), file, row.names = FALSE)
          })
      output[[paste0("downloadDataFiltered", input$start)]] <- 
        downloadHandler(
          filename = function() {
            paste0("download", ".csv")
            #paste0(tools::file_path_sans_ext(gbiffilename()), ".csv")
          },
          content = function(file) {
            write.csv(gbif_dataset_mids_filtered(), file, row.names = FALSE)
          })
      
    })
    

# Information on dataset and MIDS implementation used ---------------------

    #show which dataset was used
    observe(
      output[[paste0("Used_dataset", input$start)]] <-
        renderText(tools::file_path_sans_ext(isolate(gbiffilename())))
    )
    
    #show basic info on which MIDS implementation was used
    observe(
      output[[paste0("Used_MIDS_implementation", input$start)]] <-
        renderText(
          if (isolate(jsonfile()) == "default" & isolate(edit()) == FALSE){
            return(paste("Default:", isolate(basename(jsonpath()))))}
          else if (isolate(jsonfile()) == "custom" & isolate(edit()) == FALSE){
            return(paste("Custom:", isolate(basename(customjsonname()))))}
          else {return("Interactive")}
        )
    )

    #show complete MIDS implementation schema in modal window
    observe(
      ViewImplementationServer(paste0("showschema", resulttabnr()),
         reactive(allschemas$prev_bins[[paste0("res", resulttabnr())]][["criteria"]]),
         reactive(allschemas$prev_bins[[paste0("res", resulttabnr())]][["UoM"]]),
         disableviewschema
      ))
    
# Open results tab automatically when calculations are performed ----------
    
    observeEvent(input$start, {
      updateTabsetPanel(parent.session, "tabs",
                        selected = paste0("start-Results", input$start))
    })
    
# Close results tabs ------------------------------------------------------

    #close tabs
    clear <- reactiveValues()
    observe({
      if (grepl("Results", tabs())){
        i <- gsub("start-Results", "", tabs())
        clear[[paste0("res", i)]] <- CloseTabServer(paste0("close", i), i, parent.session)
      }
    }
    )

    #clear associated saved data
    observe(
      if (grepl("Results", tabs())){
        i <- gsub("start-Results", "", tabs())
        if (req(clear[[paste0("res", i)]]()$value) == "clear"){
          allmidscalc$prev_bins[[paste0("res", i)]] <- NULL
          allschemas$prev_bins[[paste0("res", i)]] <- NULL
        }
      }
    )
    

  })
}