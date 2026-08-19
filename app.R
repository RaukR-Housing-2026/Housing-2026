library(shiny)
library(leaflet)

source("plot_data.R")

ui <- plotOutput("map")

server <- function(input, output) {
   output$map <- renderPlot(
      map) }

shinyApp(ui=ui, server=server)
