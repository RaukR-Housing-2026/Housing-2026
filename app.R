library(shiny)

source("plot_data.R")

ui <- plotOutput("map", click = "map_click")

server <- function(input, output) {
   
   fish_sf <-
   st_sample(
      st_buffer(water, -25000), # Button only fully in water
      size = 1) %>%
      st_buffer(dist = 25000) # Button size
      
      
   hit <- reactiveVal(FALSE)
   observeEvent(input$map_click, {
      req(input$map_click)
      click_pt <-
      st_point(c(input$map_click$x, input$map_click$y)) %>%
      st_sfc(crs = st_crs(fish_sf) )
         
      if ( any( st_intersects(click_pt, fish_sf, sparse = FALSE ) ) ) { hit(TRUE) } } )
         
   output$map <-
      renderPlot(
         if( !hit() ) { map + 
            geom_sf_text(data = fish_sf, label = "🐟", size = 5)
            #> Emoji does not respect alpha  
         } else       {  map +
            geom_sf_text(data = fish_sf, label = "🐟", size = 5) +
            ggtitle('Got it!')
         ) }
         
shinyApp(ui=ui, server=server)
