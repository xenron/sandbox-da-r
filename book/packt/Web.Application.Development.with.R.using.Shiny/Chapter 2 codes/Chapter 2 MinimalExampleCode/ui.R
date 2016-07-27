##############################
### minimal example - ui.R ###
##############################

library(shiny) # load shiny at beginning at both scripts

shinyUI(pageWithSidebar( # standard shiny layout, controls on the left, output on the right
  
  headerPanel("Minimal example"), # give the interface a title
  
  sidebarPanel( # all the UI controls go in here
    textInput(inputId = "comment",  # this is the name of the variable- this will be passed to server.R
              label = "What would you like to say?", # display label for the variable
              value = "" # initial value
    )
  ),
  
  mainPanel( # all of the output elements go in here
    h3("This is you saying it"), # title with HTML helper
    textOutput("textDisplay") # this is the name of the output element as defined in server.R
  )
))