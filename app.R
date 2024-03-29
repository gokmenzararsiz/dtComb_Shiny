#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(DT)
library(dtComb)
source("ui.R")
source("server.R")
# Define UI for application that draws a histogram


# Define server logic required to draw a histogram

# Run the application 
shinyApp(ui = ui, server = server)
