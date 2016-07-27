#######################################
##### custom HTML output - server.R ###
#######################################

library(shiny)
library(ggplot2)

# load the data- keeping strings as strings

PO <- read.csv("PO.csv", stringsAsFactors = FALSE)

# create a new variable to hold the area in and fill with blanks

PO$Area <- NA

# find posts that match service codes and label them with the correct names

PO$Area[grep("RHARY", PO$HealthServices, ignore.case=TRUE)] <- "Armadillo"

PO$Area[grep("RHAAR", PO$HealthServices, ignore.case=TRUE)] <- "Baboon"

PO$Area[grep("715", PO$HealthServices, ignore.case=TRUE)] <- "Camel"

PO$Area[grep("710i", PO$HealthServices, ignore.case=TRUE)] <- "Deer"

PO$Area[grep("700", PO$HealthServices, ignore.case=TRUE)] <- "Elephant"

# create a postings variable to add together for a cumulative sum- give it 1

PO$ToAdd <- 1

# remove all missing values for Area (since they will never be shown)

PO <- PO[!is.na(PO$Area),]

# API returns data in reverse chronological order- reverse it

PO <- PO[nrow(PO):1,]

# produce cumulative sum column

PO$Sum <- ave(PO$ToAdd, PO$Area, FUN = cumsum)

# produce a date column from the data column in the spreadsheet

PO$Date <- as.Date(substr(PO$dtSubmitted, 1, 10), format = "%Y-%m-%d")

# define Shiny

shinyServer(function(input, output) { 
  
  output$plotDisplay <- renderPlot({
    
    # select only the area as selected in the UI
    
    toPlot = PO[PO$Area == input$area,]
    
    print(
      ggplot(toPlot, aes(x = Date, y = Sum)) + geom_line()
    )
    
  })
  
  output$outputLink <- renderText({
    
    # switch command in R as in many other programming languages
    
    link <- switch(input$area,
                   "Armadillo" = "http://www.patientopinion.org.uk/services/rhary",
                   "Baboon" = "http://www.patientopinion.org.uk/services/rhaar",
                   "Camel" = "http://www.patientopinion.org.uk/services/rha_715",
                   "Deer" = "http://www.patientopinion.org.uk/services/rha_710i",
                   "Elephant" = "http://www.patientopinion.org.uk/services/rha_700"
    )
    
    # paste the HTML together
    
    paste0('<form action="', link, '"target="_blank">
             <input type="submit" value="Go to main site">
             </form>')
  })
})