####################################
##### minimal example - server.R ###
####################################

library(shiny) # load shiny at beginning at both scripts

shinyServer(function(input, output) { # server is defined within these parentheses
  
  output$textDisplay <- renderText({ # mark function as reactive and assign to
                                     # output$textDisplay for passing to ui.R
    
    paste0("You said '", input$comment,           # from the text
           "'. There are ", nchar(input$comment), # input control as
           " characters in this."                # defined in ui.R
    )
  })
})