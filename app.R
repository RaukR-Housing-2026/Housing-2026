library(shiny)

my_year <- 2025

source("plot_data.R")

ui <- plotOutput("map", click = "map_click")

server <- function(input, output) {
   
   fish_sf <-
   st_sample(
      st_buffer(water, -25000), # Button only fully in water
      size = 1) %>%
      st_buffer(dist = 25000) # Button size

   map <- map + 
            geom_sf_text(data = fish_sf, label = "🐟", size = 5) +
            ## Emoji does not respect alpha, so cover:
            geom_sf(data = fish_sf, fill = 'blue3', alpha = 1/2, colour = 'blue3') 
      
   hit <- reactiveVal(FALSE)
   observeEvent(input$map_click, {
      req(input$map_click)
      click_pt <-
      st_point(c(input$map_click$x, input$map_click$y)) %>%
      st_sfc(crs = st_crs(fish_sf) )
         
      if ( any( st_intersects(click_pt, fish_sf, sparse = FALSE ) ) ) { hit(TRUE) } } )
         
   output$map <-
      renderPlot(
         if( !hit() ) { map
         } else       { map + 
           ggtitle('Fishing mode activated') +
           geom_sf(
            data = df %>%
              mutate(`Antal dagar i tusental, totalt` = `Antal dagar i tusental, totalt` %>%
                { replace(., is.na(.) | . == 0, 1) } ),
            aes(
               geometry=geometry,
               fill=median_price / `Antal dagar i tusental, totalt`) ) +
           scale_fill_distiller(
         palette='Reds',
         direction=1,
         limits =
            c(0,
         max(df %>%
            { .$median_price / .$`Antal dagar i tusental, totalt` } ) ),
         name = 'median housing price\n normalized by fishing days')
         } ) }
         
shinyApp(ui=ui, server=server)
