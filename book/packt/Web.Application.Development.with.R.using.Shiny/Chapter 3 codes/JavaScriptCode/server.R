####################################################
##### Animating text with JavaScript - server.R ####
####################################################

library(shiny)

shinyServer(function(input, output) { 
  
  output$textDisplay <- renderText({ # handle Shiny
                                     # text function
    paste0("You said '", input$textArea,           
           "'. There are ", nchar(input$textArea), 
           " characters in this."                
    )
  })
})