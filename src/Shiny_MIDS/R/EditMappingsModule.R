EditMappingsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("edit"), 
                 icon("pencil-alt"), 
                 style = "padding:2px; font-size:90%; border-style: none")
  )
}

EditMappingsServer <- function(id, element, mappings) {
  moduleServer(id, function(input, output, module.session) {
    ns <- module.session$ns
    
    stopifnot(!is.reactive(element))
    stopifnot(!is.reactive(mappings))
    
    #open modal when clicking edit icon
    onclick("edit", showModal(modalDialog(
      title = "Edit mappings",
      uiOutput(ns("editmappings")),
      easyClose = TRUE,
      footer = NULL
    )))
       
    output$editmappings <- renderUI({
      x <- list()
      for (mapping in mappings){
        x <- list(x, mapping, actionButton(paste0("remove", mapping),
                                           icon("trash"),
                                           style = "padding:5px; font-size:70%; border-style: none"),
                  br())
      }
      x <- list(fluidRow(
        column(6, h4(paste0(element, ":")), x),
        column(6,
               selectizeInput("newMapping",
                              label = "Enter a new mapping",
                              choices = readLines("www/DWCAcolumnnames.txt"),
                              multiple = TRUE),
               helpText("Select multiple properties at once if they must all be present (&)"),
               actionButton("addMapping", "Add")
        )))
      return(x)
    })
 
  })
}