# Server logic for our Shiny web app.

# First run the source_file.R,
# then run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)
library(leaflet.minicharts)
library(mapview)

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
      a <- reschool_summer_program[apply(as.data.frame(reschool_summer_program[,colm()]) == TRUE, 1, any), 
                                  c(1:12,colm())
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
                    options = list(pageLength = 3, 
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                     "}")),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: center; color: black ;',
                      htmltools::h3("ReSchool Programs")
                    ), 
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
      ) %>%
        formatStyle(colnames(data_table1[,-c(5,6,7)]),
                    backgroundColor = 'lightblue'
        )
                    
    })

    ####### STUFF TO CREATE THE BASIC MAPS W/ DEMOGRAPHICS  #######
    
    # Function to make the base map
    make_base_map <- function() {
      leaflet()  %>% 
        setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
        #addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
    
    # Construct demographic nbhd_labels for hovering on the neighborhoods
    nbhd_labels <- sprintf(
      "<b>%s</b><br/>
      No. program sessions = %i <br/>
      No. children 5-17 yrs old = %i <br/> 
      %% Hispanic students = %g%% <br/> 
      %% English student learners = %g%% <br/> 
      %% Students who use transportation = %g%% <br/> 
      %% Students with disability = %g%% ",
      shape_census@data$NBHD_NA,
      replace(shape_census@data$count, is.na(shape_census@data$count), 0), # show 0s not NAs
      shape_census@data$AGE_5_T, 
      shape_census@data$perc_hispanic_students, 
      shape_census@data$perc_nonenglish_students,
      shape_census@data$perc_with_transport_students, 
      shape_census@data$perc_disable_students
    ) %>% lapply(htmltools::HTML)
    
    # Bins and color palettes for demographic variables in leaflet map
    bins_income <- c(0, 20000, 40000, 60000, 80000, 100000, Inf)
    pal_income <- colorBin("Greens", domain = shape_census@data$MED_HH_, bins = bins_income)
    bins_edu <- c(0, 5, 10, 15, 20, 25)
    pal_edu <- colorBin("Purples", domain = shape_census@data$PCT_HSD, bins = bins_edu)
    pal_language <- colorBin("Blues", domain = shape_census@data$PCT_NON)
    # bins_hispanic <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 100)
    pal_hispanic <- colorBin("Greens", domain = shape_census@data$PCT_HIS) #, bins = bins_hispanic)
    pal_black <- colorBin("Blues", domain = shape_census@data$PCT_BLA, bins = 5)
    pal_white <- colorBin("Purples", domain = shape_census@data$PCT_WHI, bins = 5)
    
    pal_all_races <- colorFactor("Set2", domain = shape_census@data$majority_race)
    
    # Legend titles for demographic maps
    legendTitles <- list(MED_HH_ = "Median HH Income ($)",
                         PCT_HS_ = "HS Degree <br> Or Equiv. (%)",
                         PCT_HIS = "% Hispanic",
                         PCT_BLA = "% Black",
                         PCT_WHI = "% White",
                         PCT_NON = "Lang. Besides <br>English (%)",
                         majority_race = "Most Common<br>Race/Ethnicity"
    )
    
    # Function to add the demographic info to the map
    add_demographic_map <- function(map, pal_type, column_name, label_type){
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
                  label = label_type,
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
    
    ####### MAKE THE RESCHOOL PROGRAMS MAP #######
    
    output$mymap <- renderLeaflet({
      
      # Get the neighborhood data
      neighborhood_data1 <- neighborhood_data()
    
      # Construct pop-ups for when you click on a program marker
      marker_popup_text <- sprintf(
        "<b>Program: %s</b><br/> 
         Organization: %s <br/> 
         <i>Description: %s</i><br/>
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
      
      # Function to add program markers to the map
      # lat, long are the column names for latitude and longitude
      add_program_markers <- function(map, data, lat, long){
          addCircleMarkers(map, lng = jitter(data$long, factor = 1, amount = 0.0005), 
                           lat = jitter(data$lat, factor = 1, amount = 0.0005), 
                           radius = 4,
                           stroke = FALSE,
                           weight = 1,
                           fillColor = "yellow",
                           fillOpacity = 0.5,
                           label = marker_popup_text,
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "12px",
                             direction = "auto"
                           )
                           # clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
          ) %>%
          addLegend(
            position = "bottomright",
            colors = c("yellow"),
            opacity = 0.5,
            labels = "program"
          )
      }
      
      # Function to draw the base map + demographics + program markers
      make_reschool_map <- function(palette, col_name) {
        make_base_map() %>%
          add_demographic_map(palette,col_name,nbhd_labels) %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      
      ##### ACTUALLY DRAW THE RESCHOOL MAP #####
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
                      label = nbhd_labels,
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
        make_reschool_map(pal_income,"MED_HH_")
      }
      else if(input$demographics == "High school degree or equivalent (%)") {
        make_reschool_map(pal_edu,"PCT_HS_")
      }
      else if(input$demographics == "Hispanic population (%)") {
        make_reschool_map(pal_hispanic, "PCT_HIS") # %>% 
          # addMinicharts(
          #   shape_census@data$x, shape_census@data$y,
          #   chartdata = shape_census@data[, c("xxx", "yyy")], 
          #   width = 30, layerId = shape_census@data$nbhd_name
          # )
      }
      else if(input$demographics == "Black population (%)") {
        make_reschool_map(pal_black, "PCT_BLA")
      }
      else if(input$demographics == "White population (%)") {
        make_reschool_map(pal_white, "PCT_WHI")
      }
      else if(input$demographics == "Non-English speakers (%)") {
        make_reschool_map(pal_language, "PCT_NON")
      }
      else if(input$demographics == "All races") {
        labels_race_breakdown <- shape_census@data$racial_dist_html
        
        make_base_map() %>%
          add_demographic_map(pal_all_races, "majority_race", ~labels_race_breakdown) %>%
          add_program_markers(neighborhood_data1, lat, long)
      }
      
    })
    
    #### Other out of school resources tab ####
    colm_other <- reactive({
      input$program_other
    })
    
    # Function to subset all the resource datasets based on the neighborhood selected
    subset_for_neighborhoods <- function(file){
      
      b <- reactive({
        if(input$neighborhoods_other != "No neighborhood selected" ) {
          a <- file[which(file[, "nbhd_name"] == input$neighborhoods_other),]
        }
        else {
          a <- file
        }
        return(a) 
      })
      
      return(b)
      
    }
    
    # Create reactive elements for the subsetted datasets
    parks_data <- subset_for_neighborhoods(parks)
    libraries_data <- subset_for_neighborhoods(libraries)
    rec_centers_data <- subset_for_neighborhoods(rec_centers)
    museums_data <- subset_for_neighborhoods(museums)
    playgrounds_data <- subset_for_neighborhoods(playgrounds)
    fields_data <- subset_for_neighborhoods(fields)
    
    # Create the map
    output$mymap_other = renderLeaflet({
        
        # Get the data
        parks_data1 <- parks_data()
        libraries_data1 <- libraries_data()
        rec_centers_data1 <- rec_centers_data()
        museums_data1 <- museums_data()
        playgrounds_data1 <- playgrounds_data()
        fields_data1 <- fields_data()
        
        ##### ACTUALLY DRAW THE OTHER RESOURCES MAP #####
        if(is.null(input$demographics_other) == TRUE){
          open_resource_map <- make_base_map() %>%
            # plain grey neighborhoods for no demographic selection
            addPolygons(data = shape_census,
                        color = "black",
                        weight = 1, 
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillColor = "#999",
                        fillOpacity = 0.5,
                        label = nbhd_labels,
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
            )
        }
        else if(input$demographics_other == "Median household income ($)" ) {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_income,"MED_HH_",nbhd_labels)
        }
        else if(input$demographics_other == "High school degree or equivalent (%)") {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_edu,"PCT_HS_",nbhd_labels)
        }
        else if(input$demographics_other == "Hispanic population (%)") {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_hispanic, "PCT_HIS",nbhd_labels)
        }
        else if(input$demographics_other == "Black population (%)") {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_black, "PCT_BLA",nbhd_labels)
        }
        else if(input$demographics_other == "White population (%)") {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_white, "PCT_WHI",nbhd_labels)
        }
        else if(input$demographics_other == "Non-English speakers (%)") {
          open_resource_map <- make_base_map() %>% add_demographic_map(pal_language, "PCT_NON",nbhd_labels)
        }
        else if(input$demographics_other == "All races") {
          open_resource_map <- make_base_map() %>%
            add_demographic_map(pal_all_races, "majority_race", ~shape_census@data$racial_dist_html)
        }
        
        # Function to add circle markers on the map depending on the resource type(s) selected
        add_circle_markers = function(m, file, legend_title, color_code, popup_html = NULL){
         addCircleMarkers(m , data = file, 
                          lng = jitter(file$long, factor = 1, amount = 0.0005), 
                          lat = jitter(file$lat, factor = 1, amount = 0.0005), 
                          radius = 4,
                          stroke = FALSE,
                          weight = 1,
                          fillColor = color_code,
                          fillOpacity = 0.5,
                          label = popup_html,
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "12px",
                            direction = "auto"
                          )
                          )  %>%
            addLegend(
              position = "bottomright",
              colors = c(color_code),
              opacity = 0.5,
              labels = legend_title
            )
       }
        
        # Loop over selected resources types, plotting the locations of each
        for (col in colm_other()){
        
          if(col == "Parks"){
            parks_popup <- sprintf(
                "<b>%s</b><br/>
                Nature: %s, 
                Garden: %s, <br/>
                Biking: %s",
                parks_data1$name,
                # parks_data1$sqft
                parks_data1$has_nature,
                parks_data1$has_garden,
                parks_data1$has_biking
              ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(parks_data1, col, "green", parks_popup)
          }
         
          if(col == "Libraries"){
            libraries_popup <- sprintf(
              "<b>%s Library</b>",
              libraries_data1$name
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(libraries_data1, col, "blue", libraries_popup)
          }
          
          if(col == "Rec Centers"){
            rec_centers_popup <- sprintf(
              "<b>%s</b><br/>
               Cardio: %s <br/>
               Weights: %s <br/>
               Gym: %s <br/>
               Arts and Culture: %s <br/>
               Day Camps: %s <br/>
               Education Programs: %s <br/>
               Fitness and Health: %s <br/>
               Senior Programs: %s <br/>
               Social Enrich Clubs: %s <br/>
               Special Events: %s <br/>
               Sports: %s <br/>
               Aquatics: %s <br/>
              ",
              rec_centers_data1$name,
              rec_centers_data1$has_cardio,
              rec_centers_data1$has_weights,
              rec_centers_data1$has_gym,
              rec_centers_data1$has_arts_culture,
              rec_centers_data1$has_day_camps,
              rec_centers_data1$has_educ_programs,
              rec_centers_data1$has_fitness_health_programs,
              rec_centers_data1$has_senior_programs,
              rec_centers_data1$has_social_enrich_clubs,
              rec_centers_data1$has_special_events,
              rec_centers_data1$has_sports,
              rec_centers_data1$has_aquatics
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(rec_centers_data1, col, "orange", rec_centers_popup)
          }
          
          if(col == "Playgrounds"){
            playgrounds_popup <- sprintf(
              "<b>%s Playground</b>",
              playgrounds_data1$location
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(playgrounds_data1, col, "red", playgrounds_popup)
          }
          
          if(col == "Museums"){
            museums_popup <- sprintf(
              "<b>%s</b>",
              museums_data1$name
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(museums_data1, col, "purple", museums_popup)
          }
          
          if(col == "Fields"){
            fields_popup <- sprintf(
              "<b>%s Field</b><br/>
              %s",
              fields_data1$sport,
              fields_data1$location
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% add_circle_markers(fields_data1, col, "yellow", fields_popup)
          }

        }
        
        return(open_resource_map)

      })
    
    # Make the data tables for the Other Resources Data Tab
    output$dt <- renderUI({
      lapply(as.list(seq_len(length(as.list(colm_other())))), function(i) {
        id <- paste0("dt", i)
        DT::dataTableOutput(id)
      })
    })
    
    
    #Function to get datatables for eaxh resources. Has a bunch of aesthetics
    data_table_function = function(checkbox_input, data, column_names){
      
      datatable(data,
                options = list(pageLength = 3, 
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}")),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center; color: black ;',
                  htmltools::h3(checkbox_input)
                ), 
                style = "bootstrap",
                class = 'cell-border stripe',
                rownames = FALSE,
                colnames = column_names
      ) %>%
        formatStyle(colnames(data),
                    backgroundColor = 'lightblue'
        )
    }
    
    observe(
      for (i in seq_len(length(colm_other()))) {
        id <- paste0("dt", i)
        
        if(colm_other()[i] == "Parks"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Parks", parks_data()[, c(3,4,5,6,7,8,11)],
                                       c("Park name", "Class", "Has nature", "Has garden", "Has biking", "Sqft", "Nbhd name"))
              
              
            return(dat)    
          })}
        else if(colm_other()[i] == "Libraries"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Libraries", libraries_data()[, c(3,4,5,6,9)],
                                       c("Library name", "Patron Count", "Circulation Vol", "Sqft", "Nbhd name"))
            
            
            return(dat)    
          })
        }
       
        else if(colm_other()[i] == "Rec Centers"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Rec Centers", rec_centers_data()[, c(3,4,9:20, 23)],
                                       c("Rec Center name", "Type", "Has cardio", "Has weights","Has gym",
                                         "Has arts culture","Has day camps", "Has educ programs", "Has fitness health programs",
                                         "Has senior programs","Has social enrich clubs", "Has special events",
                                         "Has sports","Has aquatics", "Nbhd name"))
            
            
            return(dat)    
          })
        }
        else if(colm_other()[i] == "Museums"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Museums", museums_data()[, c(3,4,7)],
                                       c("Museum name", "Address", "Nbhd name"))
            
            
            return(dat)    
          })
        }
        else if(colm_other()[i] == "Fields"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Fields", fields_data()[, c(3,4,5,6,7, 10)],
                                       c("Sport", "Location", "Tier", "Class", "Sqft", "Nbhd name"))
            
            
            return(dat)    
          })
        }
        else if(colm_other()[i] == "Playgrounds"){
          output[[id]] <-DT::renderDataTable({
            dat <- data_table_function("Playgrounds", playgrounds_data()[, c(3,4,5,6,9)],
                                       c("Location", "Year rehabilitated", "Class", "Sqft", "Nbhd name"))
            
            
            return(dat)    
          })
        }
        
      })

    
  })  
    

