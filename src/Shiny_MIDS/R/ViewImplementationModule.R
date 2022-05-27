ViewImplementationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabPanel(
      id,
      actionButton(ns("view"), "Show MIDS implementation")
    )
  )
}

ViewImplementationServer <- function(id, schema, UoM, disable) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #show criteria
    output$json <- renderPrint(
      for (n_level in seq_along(schema)){
        if (n_level == 1)
        {#print title
          print(HTML(paste0("<div style='text-align: center; background-color: rgb(40, 116, 166, 0.8);
              font-size: 25px; color: white'>", "Criteria", "</div>")))
          #initialize grid
          print(HTML("<div style='display: grid; grid-template-columns: 50% 50%; gap: 20px; padding: 20px'>"))}
        #determine position of element in grid
        if (n_level %% 2 == 0){
          column <- 2
          row <- n_level/2}
        else {
          column <- 1
          row <- (n_level+1)/2}
        print(HTML(paste0("<div style='background-color: #E5E7E9; grid-column: ",
                          column, "; grid-row:", row, "'>")))
        #MIDS levels
        print(HTML("<div style='text-align: center;background-color: rgb(40, 116, 166, 0.8); color: white; font-size: 20px'"))
        print(h3(toupper(names(schema)[[n_level]])))
        print(HTML("</div>"))
        #MIDS elements
        mids_el <- schema[[n_level]]
        for(n_element in seq_along(mids_el)){
          print(HTML("<div style='text-align: center; background-color: rgb(40, 116, 166, 0.2); font-size: 18px; color: #1A5276'"))
          print(h4(names(mids_el)[[n_element]]))
          print(HTML("</div>"))
          #MIDS mappings
          mids_mapping <- mids_el[[n_element]]
          for (n_map in seq_along(mids_mapping)){
            #Split mappings on OR, remove brackets, replace &
            mappings <- stringr::str_split(
              gsub("\\(|\\)", "",
                   gsub("&", "and", mids_mapping[[n_map]]
                   )), "\\|")
            #Divide mappings according to their functions
            present <- list()
            absent <- list()
            for (map in mappings[[1]]){
              if (grepl("!!is.na", map)){
                absent <- c(absent, map)}
              else {present <- c(present, map)}
            }
            #Print mappings that must be present
            if (!is_empty(present)){
              print(tags$u("One of these must be present:"))
              for (presmap in present){
                print(div(gsub("\\!is.na", "", presmap)))
              }
            }
            #Print mappings that must be absent
            if (!is_empty(absent)){
              print(tags$u("Must be absent:"))
              for (absmap in absent){
                print(div(gsub("!!is.na", "", absmap)))
              }
            }
          }
        }
        print(HTML("</div>"))
        if (n_level == length(schema))
        {print(HTML("</div>"))}
      }
    )
    
    #show unknown or missing
    output$jsonUoM <- renderPrint(
      for (n_prop in seq_along(UoM))
      {prop <- names(UoM)[[n_prop]]
      values <- UoM[[n_prop]]
      if (n_prop == 1)
      {#print title
        print(HTML(paste0("<div style='text-align: center; background-color: rgb(40, 116, 166, 0.8); 
              font-size: 25px; color: white'>",
                          "Unknown or missing values", "</div>")))
        #set up grid layout
        print(HTML("<div style='display: grid; grid-template-columns: 50% 50%; gap: 20px; padding: 20px'>"))}
      #determine position of element in grid
      if (n_prop %% 2 == 0){
        column <- 2
        row <- n_prop/2}
      else {
        column <- 1
        row <- (n_prop+1)/2}
      print(HTML(paste0("<div style='background-color: #E5E7E9; grid-column: ",
                        column, "; grid-row:", row, "'>")))
      #Property
      print(HTML("<div style='text-align: center;background-color: rgb(40, 116, 166, 0.8); color: white; font-size: 20px'"))
      print(h3(prop))
      print(HTML("</div>"))
      #Unknown or missing values for that property
      for (value in values){
        print(div(value))
      }
      print(HTML("</div>"))
      if (n_prop == length(UoM))
      {print(HTML("</div>"))}
      }
    )
    
    observeEvent(input$view,
        {showModal(modalDialog(
             title = "MIDS implementation",
             htmlOutput(ns("json")),
             htmlOutput(ns("jsonUoM")),
             easyClose = TRUE,
             footer = NULL
           ))}, 
        ignoreInit = TRUE)
    
    #enable/ disable view action button
    observe(
    if (disable() == TRUE){
    shinyjs::disable("view")
    } else {
    shinyjs::enable("view")
    })
    
    
  })
}
