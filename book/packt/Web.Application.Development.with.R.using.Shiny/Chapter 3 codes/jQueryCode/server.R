library(shiny) 

shinyServer(function(input, output) { 
  
  output$dataset <- renderTable({
    
    theData = switch(input$dataSet,
                     "iris" = iris,
                     "USPersonalExpenditure" = USPersonalExpenditure,
                     "CO2" = CO2)
    
    head(theData)
    
  })
  
  output$datatext <- renderText({
    
    paste0("This is the ", input$dataSet, " dataset")
    
  })
  
  output$hiddentext <- renderText({
    
    paste0("Dataset has ", nrow(switch(input$dataSet,
                                 "iris" = iris,
                                 "USPersonalExpenditure" =
                                  USPersonalExpenditure,
                                 "CO2" = CO2)), " rows")
    
  })
})