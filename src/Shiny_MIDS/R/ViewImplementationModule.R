ViewImplementationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabPanel(
      id,
      actionButton(ns("view"), icon("eye")),
      #css
      tags$style(
        HTML(
         paste0('.', ns('title')), "{
          text-align: center; 
          background-color: #2874A6;
          font-size: 25px; 
          color: white;
          box-shadow: 0px 5px 10px 0px rgba(0, 0, 0, 0.5)
         }",
         paste0('.', ns('midslevel')), "{
          text-align: center;
          background-color: #2874A6; 
          color: white; 
          font-size: 20px;
          box-shadow: 0px 2.5px 5px 0px rgba(0, 0, 0, 0.5);
         }",
         paste0('.', ns('midselement')), "{
          text-align: center; 
          background-color: rgb(40, 116, 166, 0.2); 
          font-size: 18px; 
          color: #1A5276;
          box-shadow: none;
         }",
         paste0('.', ns('grid')), "{
           display: grid; 
           grid-template-columns: 50% 50%; 
           gap: 20px; 
           padding: 20px;
         }",
         paste0('.',"implementation_modal",
                "{width: 90% !important;",
                "max-width: 1200px;}")
      ))
    )
  )
}

ViewImplementationServer <- function(id,parent.session,schema) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #show criteria
    output$json <- renderPrint(
      for (n_level in seq_along(schema()$criteria)){
        if (n_level == 1){
          #print title
          print(HTML(paste0("<div class=", ns('title'), "> MIDS criteria </div><br>")))
          print(HTML("<div style='text-align: center'> To reach a MIDS level, all of its elements must be met. </div>"))
          print(HTML("<div style='text-align: center'> To meet a MIDS element, one of its mappings must be present. </div>"))
          #initialize grid
          print(HTML("<div class=", ns('grid'), ">"))
        }
        #determine position of element in grid
        if (n_level %% 2 == 0){
          column <- 2
          row <- n_level/2}
        else {
          column <- 1
          row <- (n_level+1)/2}
        print(HTML(paste0("<div style='background-color: #EEF0F2; box-shadow: 0px 2.5px 5px 0px rgba(0, 0, 0, 0.5); 
                          grid-column: ", column, "; grid-row:", row, "'>")))
        #MIDS levels
        print(HTML("<div class=", ns('midslevel')))
        print(h3(gsub("mids", "Level ", names(schema()$criteria)[[n_level]])))
        print(HTML("</div>"))
        #MIDS elements
        mids_el <- schema()$criteria[[n_level]]
        for(n_element in seq_along(mids_el)){
          print(HTML("<div class=", ns('midselement'))) 
          print(h4(names(mids_el)[[n_element]]))
          print(HTML("</div>"))
          #MIDS mappings
          mids_mapping <- mids_el[[n_element]]
          for (n_map in seq_along(mids_mapping)){
            #Split mappings on OR, remove brackets, replace &
            mappings <- stringr::str_split(
              gsub("\\!\\!", "NOT !",
                gsub("\\(|\\)|`", "",
                   gsub("&", "AND", mids_mapping[[n_map]]
                   ))),
              "\\|")
            #Print mappings 
             for (map in mappings[[1]]){
                  print(div(gsub("\\!is.na", "", map)))
                }
          }
        }
        print(HTML("</div>"))
        if (n_level == length(schema()$criteria))
        {print(HTML("</div>"))}
      }
    )
    
    #show unknown or missing
    output$jsonUoM <- renderPrint(
      for (n_prop in seq_along(schema()$UoM))
      {prop <- names(schema()$UoM)[[n_prop]]
      values <- schema()$UoM[[n_prop]]
      if (n_prop == 1)
      {#print title
        print(HTML(paste0("<div class=", ns('title'), "> Unknown or missing values </div><br>")))
        print(HTML("<div style='text-align: center'> When a property has a value specified here, it will be regarded as absent/not digitized. </div>"))
        #set up grid layout
        print(HTML("<div class=", ns('grid'), ">"))}
      #determine position of element in grid
      if (n_prop %% 2 == 0){
        column <- 2
        row <- n_prop/2}
      else {
        column <- 1
        row <- (n_prop+1)/2}
      print(HTML(paste0("<div style='background-color: #EEF0F2; box-shadow: 0px 2.5px 5px 0px rgba(0, 0, 0, 0.5);
                        grid-column: ", column, "; grid-row:", row, "'>")))
      #Property
      print(HTML("<div class=", ns('midslevel')))
      print(h3(prop))
      print(HTML("</div>"))
      #Unknown or missing values for that property
      for (value in values){
        print(div(value))
      }
      print(HTML("</div>"))
      if (n_prop == length(schema()$UoM))
      {print(HTML("</div>"))}
      }
    )
    
    observeEvent(input$view,
        {showModal(modalDialog(
             title = "MIDS implementation",
             htmlOutput(ns("json")),
             htmlOutput(ns("jsonUoM")),
             easyClose = TRUE,
             footer = NULL,
             class = "implementation_modal"
           ))}, 
        ignoreInit = TRUE)
  })
}
