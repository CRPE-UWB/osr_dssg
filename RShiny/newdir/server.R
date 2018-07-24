# Server logic for our Shiny web app.

# First run the source_file.R,
# then run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)

shinyServer(
  
  function(input, output) {
    
    colm <- reactive({
      as.numeric(input$program)
    })
    
    program_category_data <- reactive({
      a <- reschool_summer_program[apply(as.data.frame(reschool_summer_program[,colm()]) == 1, 1, any), 
                                  c(1,2,3,4,5,6,7,8,9,10,11,12,colm())
                                  ]
      return(a)
    })
    
    # Subset by neighborhood, based on input selection
    neighborhood_data <- reactive({
      summer_program_data <- program_category_data()
      
      if(input$neighborhoods != "No neighborhood selected" ) {
        a <- subset(summer_program_data, 
                   summer_program_data$nbhd_name == input$neighborhoods & 
                     summer_program_data$session_cost  >= input$slider[1] & 
                     summer_program_data$session_cost  <= input$slider[2]
                   )
      }
      else {
        a <- subset(summer_program_data,  
                   summer_program_data$session_cost >= input$slider[1] & 
                     summer_program_data$session_cost  <= input$slider[2]
                   )
      }
      return(a) 
    })
    
    # Output table for data tab
    output$datatable <- DT::renderDataTable({
      data_table1 <- neighborhood_data()
      DT::datatable(data_table1[,-c(5,6,7)], 
                    options = list(lengthMenu = c(5, 30, 50), 
                                   pageLength = 5)
                    )
    })

    ####### MAKE THE MAP #######
    output$mymap <- renderLeaflet({
      
      # Get the data
      neighborhood_data1 <- neighborhood_data()
      
      # Construct demographic labels for hovering on the neighborhoods
      labels <- sprintf(
        "No. children 5-17 yrs old = %i <br/> 
         No. program sessions = %i <br/> 
         %% Hispanic students = %g%% <br/> 
         %% English student learners = %g%% <br/> 
         %% Students who use transportation = %g%% <br/> 
         %% Students with disability = %g%% ",
        shape_census@data$AGE_5_T, 
        shape_census@data$count, 
        shape_census@data$perc_hispanic_students, 
        shape_census@data$perc_nonenglish_students,
        shape_census@data$perc_with_transport_students, 
        shape_census@data$perc_disable_students
        ) %>% lapply(htmltools::HTML)
    
      # Construct pop-ups for when you click on a program marker
      marker_popup_text <- sprintf(
        "<b>%s</b><br/> 
         %s <br/> 
         <i>%s</i><br/>
         $%i per session<br/>
         Starts: %s, Ends: %s <br/>  
         Special needs = %s,  
         Scholarships = %s <br/>",
        neighborhood_data1$session_name, 
        neighborhood_data1$camp_name, 
        neighborhood_data1$session_short_description,
        neighborhood_data1$session_cost,
        neighborhood_data1$session_date_start, 
        neighborhood_data1$session_date_end,
        neighborhood_data1$has_special_needs_offerings, 
        neighborhood_data1$has_scholarships
        ) %>% lapply(htmltools::HTML)
      
      # Variables for (median household income) in leaflet map
      bins_income <- c(0, 20000, 40000, 60000, 80000, 100000, Inf)
      pal_income <- colorBin("Blues", domain = shape_census@data$MED_HH_, bins = bins_income)
      
      # Variables for (% people over 25 with at least a high school diploma) in leaflet map
      bins_edu <- c(0, 10, 20, 30, 40, 50, 100)
      pal_edu <- colorBin("Blues", domain = shape_census@data$PCT_HSD, bins = bins_edu)
      
      # Variables for (% Hispanic) in leaflet map
      bins_hispanic <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 100)
      pal_hispanic <- colorBin("Blues", domain = shape_census@data$PCT_HIS, bins = bins_hispanic)
      
      # Variables for (% language other than English) in leaflet map
      bins_language <- c(0, 10, 20, 30, 40, 50, 60, 70, 100)
      pal_language <- colorBin("Blues", domain = shape_census@data$PCT_NON, bins = bins_language)
      
      # Function to make the base map
      make_base_map <- function() {
        leaflet()  %>% 
          setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
          #addTiles() %>%
          addProviderTiles(providers$CartoDB.Positron)
      }
      
      # Function to add the demographic info to the map
      add_demographic_map <- function(map, pal_type, column_name){
        addPolygons(map, data = shape_census,
                    fillColor = ~pal_type(shape_census@data[,column_name]),
                    weight = 2,
                    opacity = 1,
                    color = "#777",
                    dashArray = "",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      bringToFront = FALSE
                      ),
                     label = labels,
                     labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "12px",
                      direction = "auto"
                      )
                      ) %>% 
        addLegend(pal = pal_type,
                  values = shape_census@data[,column_name],
                  opacity = 0.7,
                  title = NULL,
                  position = "bottomright"
                  )
      }
      
      # Function to add program markers to the map
      # lat, long are the column names for latitude and longitude
      add_program_markers <- function(map, data, lat, long){
          addCircleMarkers(map, lng = jitter(data$long), lat = jitter(data$lat), 
                           radius = 4,
                           stroke = TRUE,
                           weight = 1,
                           fillColor = "yellow",
                           fillOpacity = 0.5,
                           popup = marker_popup_text 
                           # clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
          ) 
      }
      
      # DRAW THE DEMOGRAPHICS MAP, based on input selection
      if(is.null(input$demographics) == TRUE){
        make_base_map() %>%
          # plain grey neighborhoods for no demographic selection
          addPolygons(data = shape_census,
                      color = "black",
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 1.0,
                      fillColor = "#999",
                      fillOpacity = 0.5,
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "12px",
                        direction = "auto"
                      ),
                      highlight = highlightOptions(
                        bringToFront = FALSE,
                        weight = 5,
                        color = "#666"
                      )
                      ) %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "Median household income ($)" ) {
        make_base_map() %>%
          add_demographic_map(pal_income,"MED_HH_") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "High school degree or equivalent (%)") {
        make_base_map() %>%
          add_demographic_map(pal_edu,"PCT_HS_") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "Hispanic population (%)") {
        make_base_map() %>%
          add_demographic_map(pal_hispanic, "PCT_HIS") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "Non-native English speakers (%)") {
        make_base_map() %>%
          add_demographic_map(pal_language, "PCT_NON") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      
    })  # finish rendering leaflet map

    
  })  
    
    