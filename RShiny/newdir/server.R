
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(DT)
# library(leaflet)
# library(sp)


shinyServer(
  
  function(input, output) {
    
    
    colm = reactive({
      
      as.numeric(input$program)
      
    })
    
    program_category_data = reactive({
      
      
      a = reschool_summer_program[apply(as.data.frame(reschool_summer_program[,colm()]) == 1, 1, any), c(1,2,3,4,5,6,7,8,colm())]
      
      
      return(a) 
      
      
      
    })
    
    
    neighborhood_data = reactive({
      
      summer_program_data = program_category_data()
      
      if(input$neighborhoods != "No neighborhood selected" ) {
        
        a = subset(summer_program_data, summer_program_data$nbhd_name == input$neighborhoods & 
                     summer_program_data$session_cost  >= input$slider[1] & summer_program_data$session_cost  <= input$slider[2] )
        
      }
      
      else {
        
        a = subset(summer_program_data,  
                   summer_program_data$session_cost  >= input$slider[1] & summer_program_data$session_cost  <= input$slider[2]  )
        
      }
      
      return(a) 
      
      
    })
    
    
    
    
    output$datatable = DT::renderDataTable({
      
      
      data_table1 = neighborhood_data()
      
      DT::datatable(data_table1[,-c(5,6,7)], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      
      
    })
    
    
    
    
    output$mymap = renderLeaflet({
      
      neighborhood_data1 = neighborhood_data()
      
      #Creating the variables to be used in the leaflet to show median household data
      bins <- c(0, 20000, 40000, 60000, 80000, 100000, Inf)
      pal <- colorBin("YlOrRd", domain = shape_census@data$MED_HH_, bins = bins)
      labels <- sprintf(
        "Children aged 5-18 = %i <br/> Number of sessions = %i <br/> %% Hispanic students = %g%% <br/> %% English student learners = %g%% <br/> 
        %% Students who use transportation = %g%% <br/> %% Students with disability = %g%% ",
        shape_census@data$AGE_5_T, shape_census@data$count, shape_census@data$perc_hispanic_students, shape_census@data$perc_nonenglish_students,
        shape_census@data$perc_with_transport_students, shape_census@data$perc_disable_students) %>% lapply(htmltools::HTML)
      
      #Creating the variables to be used in the leaflet to show percentage of households % of people over 25 who have atleast a high school degree
      bins_edu <- c(0, 10, 20,30,40,50,Inf)
      pal_edu <- colorBin("YlOrRd", domain = shape_census@data$PCT_HSD, bins = bins_edu)
      
      #Creating the variables to be used in the leaflet to show percentage of hispanic population
      bins_hispanic <- c(0, 10, 20,30,40,50,60,70,80,Inf)
      pal_hispanic <- colorBin("YlOrRd", domain = shape_census@data$PCT_HIS, bins = bins_hispanic)
      
      #Creating the variables to be used in the leaflet to show percentage of non-english population
      bins_language <- c(0, 10, 20,30,40,50,60,70,Inf)
      pal_language <- colorBin("YlOrRd", domain = shape_census@data$PCT_NON, bins = bins_language)
      
      
      #Function for creating the markers
      map_with_markers = function(data, lat, lon){
        leaflet()  %>% setView(lng = -104.991531, lat = 39.742043,zoom = 10) %>% addTiles() %>%
          addMarkers(lng = jitter(data$lon), lat = jitter(data$lat), popup = "single marker", 
                     clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)) 
        
        
      }
      
      
      #Function for creating demographic maps
      demographic_maps = function(pal_type, column_name ){
        
        map_with_markers(neighborhood_data1, lat, long) %>% addPolygons(data = shape_census,
                                                                        fillColor = ~pal_type(shape_census@data[,column_name]),
                                                                        weight = 2,
                                                                        opacity = 1,
                                                                        color = "white",
                                                                        dashArray = "3",
                                                                        fillOpacity = 0.7,
                                                                        highlight = highlightOptions(
                                                                          weight = 5,
                                                                          color = "#666",
                                                                          dashArray = "",
                                                                          fillOpacity = 0.7,
                                                                          bringToFront = TRUE),
                                                                        label = labels,
                                                                        labelOptions = labelOptions(
                                                                          style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                          textsize = "15px",
                                                                          direction = "auto")) %>%
          addLegend(pal = pal_type, values = shape_census@data[,column_name], opacity = 0.7, title = NULL,
                    position = "bottomright")
        
        
      }
      
      if(is.null(input$demographics) == TRUE){
        
        map_with_markers(neighborhood_data1, lat, long) %>% addPolygons(data = shape_census, color = "#999999", weight = 1, smoothFactor = 0.5,
                                                                        opacity = 1.0, fillOpacity = 0.5)
        
      }
      
      
      
      
      else if(input$demographics == "Median household income ($)" ) {
        
        demographic_maps(pal,"MED_HH_")
        
        
      }
      
      
      else if(input$demographics == "Above 25 high school degree holders(%)") {
        
        demographic_maps(pal_edu,"PCT_HSD")
        
        
      }
      
      
      else if(input$demographics == "Hispanic population (%)") {
        
        demographic_maps(pal_hispanic, "PCT_HIS")
        
        
        
      }
      
      
      else if(input$demographics == "Non native English speakers (%)") {
        
        demographic_maps(pal_language, "PCT_NON")
        
      }
      
    })
    
    
  })





