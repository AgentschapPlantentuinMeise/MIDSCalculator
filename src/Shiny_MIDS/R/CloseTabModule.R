CloseTabUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      actionButton(ns("close"), 
                   icon("times"),
                   style = "padding:5px; font-size:70%; border-style: none")
  )
}

CloseTabServer <- function(id, tab, parent.session) {
  moduleServer(id, function(input, output, module.session) {
        ns <- module.session$ns
        #open modal when clicking close button
        observeEvent(input$close,{ 
          #show modal
          showModal(modalDialog(
             title = "Close tab",
             "Are you sure you want to close this tab?",
             easyClose = TRUE,
             footer = tagList(
               modalButton("Cancel"),
               actionButton(ns("closetab"), "Close")
             )
          ))
        }, ignoreInit = TRUE)
        
        clear <- reactiveValues(value = NULL)
        
        #if confirmation to close is given:
        observeEvent(input$closetab,{ 
          #close results tab
          removeTab(session = parent.session, inputId="tabs", target=paste0("start-Results", tab))
          #close modal
          removeModal()
          #set reactive to clear data
          clear$value <- "clear"
        })
        
        return(reactive(clear))
        
  })
}