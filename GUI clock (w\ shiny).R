library(shiny)
library(miniUI)



clock <- function() {
  
  # gadget page
  ui <- miniPage(
    gadgetTitleBar("Clock"),
    miniContentPanel(
      uiOutput("time")
    )
  )
  
  server <- function(input, output, session) {
    
    # Css confige
    clockStyles <- paste(
      "border: 1px solid #DADADA",
      "background-color: #EFEFEF",
      "border-radius: 5px",
      "font-size: 6em",
      "margin-top: 60px",
      "text-align: center",
      sep = "; "
    )
    
    # reactiveTimer for real-time update
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
    observe({
      
      invalidatePeriodically()
      
      # Get the time
      # render it as a large paragraph element.
      time <- Sys.time()
      output$time <- renderUI({
        p(style = clockStyles, time)
      })
    })
    
    # stop gadget
    observeEvent(input$done, {
      timeText <- paste0("\"", as.character(Sys.time()), "\"")
      rstudioapi::insertText(timeText)
      stopApp()
    })
    
  }
  
  # implementing pane viewer
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
  
}




