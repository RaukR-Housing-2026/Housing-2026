library(shiny)
library(tidyverse)

shinyApp(
  ui = fluidPage(
    h3("A BETTER TITLE"),
    fluidRow(
      column(4,
        selectInput("direct_input",
                    label = "Select direction",
                    choices = c("Most expensive", "Cheapest"))
      ), 
      column(4,
        selectInput(
          "year_input",
          label = "Select year",
          selected = 2025,
          choices = sort(unique(brf_lan_tidy$year))
        )
      ),
      column(4,
        selectInput(
          "price_input",
          label = "Select price measure",
          choices = c("Median price" = "median_price",
                  "Mean price"   = "mean_price")
        )
      )
    ),
    tableOutput("table_output")
  ),

  server = function(input, output) {
    output$table_output <- renderTable({
      df <- brf_lan_tidy |>
        filter(year == as.numeric(input$year_input))

      if (input$direct_input == "Most expensive") {
        df <- arrange(df, desc(.data[[input$price_input]]))
      } else {
        df <- arrange(df, .data[[input$price_input]])
      }

      df |>
        select(region, year, n_sales, all_of(input$price_input)) |>
        head(5) |>
        rename(
          "Region" = region,
          "Year" = year,
          "Number of sales" = n_sales,
          "Price (SEK)" = all_of(input$price_input))
    }, digits = 0)
  })

