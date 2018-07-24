
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
  
  ############################################################ReSchool Programs tab##################################################################
  function(input, output) {
    
    #Getting column numbers depending on the type of the program selected. Used to subset the data in the next step
    colm = reactive({
      
      as.numeric(input$program)
      
    })
    
    #Subsetting the data for the type of the program selected
    program_category_data = reactive({
      
      
      a = reschool_summer_program[apply(as.data.frame(reschool_summer_program[,colm()]) == 1, 1, any), c(1,2,3,4,5,6,7,8,9,10,11,12,colm())]
      
      
      return(a) 
      
      
      
    })
    
    
    #Subsetting the data for the neighborhood and the cost of the program selected
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
    
    
    
    #Displaying the relevant data in the data tab based on the selections
    output$datatable = DT::renderDataTable({
      
      
      data_table1 = neighborhood_data()
      
      DT::datatable(data_table1[,-c(5,6,7)], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      
      
    })
    
    
    
    #Plotting the map based on the selections made. 
    output$mymap = renderLeaflet({
      
      neighborhood_data1 = neighborhood_data()
      
      
      #Creating the variables to be used in the leaflet to show median household data
      bins <- c(0, 20000, 40000, 60000, 80000, 100000, Inf)
      pal <- colorBin("viridis", domain = shape_census@data$MED_HH_, bins = bins)
      
     
      
      #Hovering on the neighborhood text
      labels <- sprintf(
        "Children aged 5-18 = %i <br/> Number of sessions = %i <br/> %% Hispanic students = %g%% <br/> %% English student learners = %g%% <br/> 
        %% Students who use transportation = %g%% <br/> %% Students with disability = %g%% ",
        shape_census@data$AGE_5_T, shape_census@data$count, shape_census@data$perc_hispanic_students, shape_census@data$perc_nonenglish_students,
        shape_census@data$perc_with_transport_students, shape_census@data$perc_disable_students) %>% lapply(htmltools::HTML)
      
      #Text in the pop up markers
      marker_popup_text <- sprintf(
        "Session name = %s, <br/> Organization name = %s, <br/> Session start date = %s, <br/> Session end date = %s, <br/> Session cost = %i,  <br/> Special needs offerings = %s,
         <br/> Scholarships = %s, <br/> Session short description = %s",
        neighborhood_data1$session_name,neighborhood_data1$camp_name, neighborhood_data1$session_date_start, neighborhood_data1$session_date_end,
        neighborhood_data1$session_cost,neighborhood_data1$has_special_needs_offerings, neighborhood_data1$has_scholarships,
        neighborhood_data1$session_short_description) %>% lapply(htmltools::HTML)
      
      
      
      #Creating the variables to be used in the leaflet to show percentage of households % of people over 25 who have atleast a high school degree
      bins_edu <- c(0, 10, 20,30,40,50,Inf)
      pal_edu <- colorBin("viridis", domain = shape_census@data$PCT_HSD, bins = bins_edu)
      
      #Creating the variables to be used in the leaflet to show percentage of hispanic population
      bins_hispanic <- c(0, 10, 20,30,40,50,60,70,80,Inf)
      pal_hispanic <- colorBin("Greens", domain = shape_census@data$PCT_HIS, bins = bins_hispanic)
      
      #Creating the variables to be used in the leaflet to show percentage of non-english population
      bins_language <- c(0, 10, 20,30,40,50,60,70,Inf)
      pal_language <- colorBin("viridis", domain = shape_census@data$PCT_NON, bins = bins_language)
      
      
      #Function for creating the markers
      map_with_markers = function(data, lat, lon){
        leaflet()  %>% setView(lng = -104.991531, lat = 39.742043,zoom = 10) %>% addTiles() %>%
          addMarkers(lng = jitter(data$lon), lat = jitter(data$lat), popup = marker_popup_text, 
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
                                                                          color = "#777",
                                                                          dashArray = "",
                                                                          fillOpacity = 0.7,
                                                                          bringToFront = TRUE),
                                                                        label = labels,
                                                                        labelOptions = labelOptions(
                                                                          style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                          textsize = "15px",
                                                                          direction = "auto"), layerId = shape_census@data$nbhd_name) %>%
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
      
      
      else if(input$demographics == "High school degree or equivalent(%)") {
        
        demographic_maps(pal_edu,"PCT_HSD")
        
        
      }
      
      
      else if(input$demographics == "Hispanic population (%)") {
        
        demographic_maps(pal_hispanic, "PCT_HIS") %>% addMinicharts(
          shape_census@data$x, shape_census@data$y,
          chartdata = shape_census@data[, c("xxx", "yyy")], 
          width = 30, layerId = shape_census@data$nbhd_name
        )

        
        
        
      }
      
      
      else if(input$demographics == "Non native English speakers (%)") {
        
        demographic_maps(pal_language, "PCT_NON")
        
      }
      
    })
    
    ####################################Other out of school resources tab###################################################################
    colm_other = reactive({
      
      input$program_other
      
    })
   
      
      parks_data = reactive({
        
        
        if(input$neighborhoods_other != "No neighborhood selected" ) {
          
          a = parks[which(parks[, "nbhd_name"] == input$neighborhoods_other),]
          
        }
        
        else {
          
          a = parks
      
        }
        
        return(a) 
    })
    
    
    libraries_data = reactive({
      
      
      if(input$neighborhoods_other != "No neighborhood selected" ) {
        
        a = libraries[which(libraries[, "nbhd_name"] == input$neighborhoods_other),]
        
      }
      
      else {
        
        a = libraries
        
      }
      
      return(a) 
    })
      
    
    
      
     
      
      output$mymap_other = renderLeaflet({
        
      
        
        parks_data1 = parks_data()
        libraries_data1 = libraries_data()
        
        m = leaflet()  %>% setView(lng = -104.991531, lat = 39.742043,zoom = 10) %>% addTiles()
        
  
        for (col in colm_other()){
          print(col)
         if(col == "Parks"){
           print(head(parks_data1))

           map <- m %>% addMarkers(map = m, lng = jitter(parks_data1$long), lat = jitter(parks_data1$lat))


         }

          if(col == "Libraries"){
            print(col)

            map %>% addMarkers(map, lng = jitter(libraries_data1$long), lat = jitter(libraries_data1$lat))


          }

        }
        
        
        
       #  
       #  if(input$program_other == "Libraries"){
       #    
       #    map <- map %>% addMarkers(lng = jitter(libraries_data1$long), lat = jitter(libraries_data1$lat)) 
       #  }   
       #  
       #  
       # else if(input$program_other == "Parks"){
       #   
       #   map <- map %>%
       #     addMarkers(lng = jitter(parks_data1$long), lat = jitter(parks_data1$lat)) 
       # }
        
        
        
      })
    
    
   

    
  })  
    
    
    
    
    
    
  





