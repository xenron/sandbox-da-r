#################################
### custom HTML output - ui.R ###
#################################

library(shiny) 

shinyUI(pageWithSidebar( 
  
  headerPanel("Patient Opinion posts by area"),
  
  sidebarPanel( 
    
    radioButtons("area", "Service area",
                 c("Armadillo", "Baboon", "Camel", "Deer", "Elephant"),
                 selected = "Armadillo")
  ),
  
  mainPanel( 
    h3("Total posts"),
    HTML("<p>Cumulative <em>totals</em> over time</p>"),
    plotOutput("plotDisplay"),
    htmlOutput("outputLink")
    )
))