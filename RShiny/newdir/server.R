# Server logic for our Shiny web app.

# First run the source_file.R,
# then run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)

shinyServer(
  
  #### ReSchool Programs tab ####
  function(input, output) {
    
    # Getting column numbers depending on the type of the program selected. 
    # (used to subset the data in the next step)
    colm <- reactive({
      as.numeric(input$program)
    })
    
    # Subsetting the data for the type of the program selected
    program_category_data <- reactive({
      a <- reschool_summer_program[apply(as.data.frame(reschool_summer_program[,colm()]) == 1, 1, any), 
                                  c(1,2,3,4,5,6,7,8,9,10,11,12,colm())
                                  ]
      return(a)
    })
    
    # Subsetting the data for the neighborhood and the cost of the program selected
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
    
    # Output the relevant data in the data tab based on the selections
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
        "<b>No. program sessions = %i </b><br/>
         No. children 5-17 yrs old = %i <br/> 
         %% Hispanic students = %g%% <br/> 
         %% English student learners = %g%% <br/> 
         %% Students who use transportation = %g%% <br/> 
         %% Students with disability = %g%% ",
        shape_census@data$count,
        shape_census@data$AGE_5_T, 
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
      pal_income <- colorBin("Greens", domain = shape_census@data$MED_HH_, bins = bins_income)
      
      # Variables for (% people over 25 with at least a high school diploma) in leaflet map
      bins_edu <- c(0, 5, 10, 15, 20, 25)
      pal_edu <- colorBin("Purples", domain = shape_census@data$PCT_HSD, bins = bins_edu)
      
      # Variables for language other than english
      pal_language <- colorBin("Blues", domain = shape_census@data$PCT_NON)
      
      # Variables for (% race) in leaflet map
      #bins_hispanic <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 100)
      pal_hispanic <- colorBin("Greens", domain = shape_census@data$PCT_HIS) #, bins = bins_hispanic)
      pal_black <- colorBin("Blues", domain = shape_census@data$PCT_BLA, bins = 5)
      pal_white <- colorBin("Purples", domain = shape_census@data$PCT_WHI, bins = 5)
      
      # Function to make the base map
      make_base_map <- function() {
        leaflet()  %>% 
          setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
          #addTiles() %>%
          addProviderTiles(providers$CartoDB.Positron)
      }
      
      legendTitles <- list(MED_HH_ = "Median HH Income ($)",
                           PCT_HS_ = "HS Degree <br> Or Equiv. (%)",
                           PCT_HIS = "% Hispanic",
                           PCT_BLA = "% Black",
                           PCT_WHI = "% White",
                           PCT_NON = "Lang. Besides <br>English (%)"
                           )
      
      # Function to add the demographic info to the map
      add_demographic_map <- function(map, pal_type, column_name){
        addPolygons(map, data = shape_census,
                    fillColor = ~pal_type(shape_census@data[,column_name]),
                    weight = 2,
                    opacity = 1,
                    color = "#777",
                    dashArray = "",
                    fillOpacity = 0.5,
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
                  title = as.character(legendTitles[column_name]),
                  position = "bottomright"
                  )
      }
      
      # Function to add program markers to the map
      # lat, long are the column names for latitude and longitude
      add_program_markers <- function(map, data, lat, long){
          addCircleMarkers(map, lng = jitter(data$long, factor = 1, amount = 0.0005), 
                           lat = jitter(data$lat, factor = 1, amount = 0.0005), 
                           radius = 6,
                           stroke = FALSE,
                           weight = 1,
                           fillColor = "yellow",
                           fillOpacity = 0.5,
                           popup = marker_popup_text
                           # clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
          ) %>%
          addLegend(
            position = "bottomright",
            colors = c("yellow"),
            opacity = 0.5,
            
            labels = "summer program"
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
      else if(input$demographics == "Black population (%)") {
        make_base_map() %>%
          add_demographic_map(pal_black, "PCT_BLA") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "White population (%)") {
        make_base_map() %>%
          add_demographic_map(pal_white, "PCT_WHI") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      else if(input$demographics == "Non-English speakers (%)") {
        make_base_map() %>%
          add_demographic_map(pal_language, "PCT_NON") %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      
    })  # finish rendering leaflet map for reschool programs
    
    #### Other out of school resources tab ####
    colm_other = reactive({
      input$program_other
    })
    
    #Function to subset all the resource datasets based on the neighborhood selected
    subset_for_neighborhoods = function(file){
      
      b = reactive({
        
        if(input$neighborhoods_other != "No neighborhood selected" ) {
          a = file[which(file[, "nbhd_name"] == input$neighborhoods_other),]
        }
        else {
          a = file
        }
        return(a) 
        
      })
      
      return(b)
      
    }
    
    
    #Calling the function to create the subsetted datasets
    parks_data = subset_for_neighborhoods(parks)
    libraries_data = subset_for_neighborhoods(libraries)
    rec_centers_data = subset_for_neighborhoods(rec_centers)
    museums_data = subset_for_neighborhoods(museums)
    playgrounds_data = subset_for_neighborhoods(playgrounds)
    fields_data = subset_for_neighborhoods(fields)
    
    
    
    #Creating reactive element for the map
    output$mymap_other = renderLeaflet({
        
        parks_data1 = parks_data()
        libraries_data1 = libraries_data()
        rec_centers_data1 = rec_centers_data()
        museums_data1 = museums_data()
        playgrounds_data1 = playgrounds_data()
        fields_data1 = fields_data()
        
        #Creating the base map
        m = leaflet()  %>% setView(lng = -104.991531, lat = 39.742043,zoom = 10) %>% addTiles()
        
        #Creating a function to add circle markers on the map depending on the resource selected through the checkbox input
        add_circle_markers = function(m, file, color_code){
         
         addCircleMarkers(m , data = file, lng = jitter(file$long, factor = 1, amount = 0.0005), 
                          lat = jitter(file$lat, factor = 1, amount = 0.0005), 
                          radius = 6,
                          stroke = FALSE,
                          weight = 1,
                          fillColor = color_code,
                          fillOpacity = 0.5) 
        
       }
        
        #Creating a loop to plot the loactions depending on the resource selected through the checkbox input 
        for (col in colm_other()){
        
         if(col == "Parks"){
           

           m = m %>% add_circle_markers(parks_data1, "yellow")
            

         }

          if(col == "Libraries"){
        
           
           m = m %>% add_circle_markers(libraries_data1, "red")
          

          }
          
          if(col == "Rec Centers"){
            
            
            m = m %>% add_circle_markers(rec_centers_data1, "blue")
            
            
          }
          
          if(col == "Playgrounds"){
            
            
            m = m %>% add_circle_markers(playgrounds_data1, "green")
            
            
          }
          
          if(col == "Museums"){
            
            
            m = m %>% add_circle_markers(museums_data1, "purple")
            
            
          }
          
          if(col == "Fields"){
            
            
            m = m %>% add_circle_markers(fields_data1, "orange")
            
            
          }

        }
        
        return(m)
        
        
        
      })
    

    
    output$dt <- renderUI({

  
     
      lapply(as.list(seq_len(length(as.list(colm_other())))), function(i) {
        id <- paste0("dt", i)

        id <- paste0("dt", i)
        DT::dataTableOutput(id)
        })
      })
 
    observe(
    for (i in seq_len(length(colm_other()))) {
      id <- paste0("dt", i)

      if(colm_other()[i] == "Parks"){
            output[[id]] <- DT::renderDataTable({
              dat = datatable(parks_data()[, c(3,4,5,6,7,8,11)],options = list(pageLength = 3, initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}")),
                                                             caption = "Parks", style = "bootstrap") %>%
              formatStyle(colnames(parks_data()[, c(3,4,5,6,7,8,11)]),backgroundColor = 'yellow')
          return(dat)    
      })}
      else if(colm_other()[i] == "Libraries"){
        output[[id]] <- DT::renderDataTable(libraries_data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 3))
      }
      
      else if(colm_other()[i] == "Rec Centers"){
        output[[id]] <- DT::renderDataTable(rec_centers_data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 3))
      }
      else if(colm_other()[i] == "Museums"){
        output[[id]] <- DT::renderDataTable(museums_data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 3))
      }
      else if(colm_other()[i] == "Fields"){
        output[[id]] <- DT::renderDataTable(fields_data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 3))
      }
      else if(colm_other()[i] == "Playgrounds"){
        output[[id]] <- DT::renderDataTable(playgrounds_data(), options = list(lengthMenu = c(5, 10, 15), pageLength = 3))
      }

   })
    
    
    
   
    
  })  
    

