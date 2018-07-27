# Server logic for our Shiny web app.

# First run the source_file.R,
# then run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)

shinyServer(
  function(input, output) {
    
    #############################
    # Reschool Programs Tab
    #############################
    
    ####### RESCHOOL PROGRAMS SUBSETTING BY COST AND TYPE #######
    
    program_category_data <- reactive({
      return(subset_for_category(reschool_summer_program, as.numeric(input$program)))
    })
    
    program_cost_data <- reactive({
      return(subset_for_cost(program_category_data(),input$slider[1],input$slider[2]))
    })
    
    neighborhood_data <- reactive({
      return(subset_for_neighborhoods(program_cost_data(),input$neighborhoods))
    })
    
    ####### RESCHOOL PROGRAMS DATA TAB #######
    
    # Output the relevant data in the data tab based on the selections
    output$datatable <- DT::renderDataTable({
      data_table1 <- neighborhood_data()
      DT::datatable(data_table1[,-c(5,6,7)], 
                    options = list(pageLength = 3, 
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css(
                                      {'background-color': '#000', 'color': '#fff'}
                                      );",
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
    
    ####### RESCHOOL PROGRAMS MAP #######
    
    output$mymap <- renderLeaflet({
      
      # Get the neighborhood data
      neighborhood_data1 <- neighborhood_data()
    
      # Construct pop-ups for when you click on a program marker
      marker_popup_text <- sprintf(
        "<b>%s</b><br/> 
         %s <br/> 
         <i>%s</i><br/>
         $%i per session<br/>
         Starts: %s, Ends: %s <br/>  
         Special needs = %s,  
         Scholarships = %s <br/>",
        wrap_text(paste("Program: ",neighborhood_data1$session_name)), 
        wrap_text(paste("Organization: ",neighborhood_data1$camp_name)), 
        wrap_text(paste("Description: ",neighborhood_data1$session_short_description)),
        neighborhood_data1$session_cost,
        neighborhood_data1$session_date_start, 
        neighborhood_data1$session_date_end,
        neighborhood_data1$has_special_needs_offerings, 
        neighborhood_data1$has_scholarships
        ) %>% lapply(htmltools::HTML)
      
      ##### ACTUALLY DRAW THE RESCHOOL MAP #####
      if(is.null(input$demographics)){
        make_base_map() %>%
          add_blank_map() %>%
          add_circle_markers(neighborhood_data1, "Programs", "yellow", marker_popup_text)
      }
      else if(input$demographics == "Median household income ($)" ) {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_income,"MED_HH_")
      }
      else if(input$demographics == "High school degree or equivalent (%)") {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_edu,"PCT_HS_")
      }
      else if(input$demographics == "Hispanic population (%)") {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_hispanic, "PCT_HIS") 
      }
      else if(input$demographics == "Black population (%)") {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_black, "PCT_BLA")
      }
      else if(input$demographics == "White population (%)") {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_white, "PCT_WHI")
      }
      else if(input$demographics == "Non-English speakers (%)") {
        make_reschool_map(neighborhood_data1, marker_popup_text, pal_language, "PCT_NON")
      }
      else if(input$demographics == "All races") {
        labels_race_breakdown <- shape_census@data$racial_dist_html
        
        make_base_map() %>%
          add_demographic_map(pal_all_races, "majority_race", 
                              ~labels_race_breakdown) %>%
          add_circle_markers(neighborhood_data1, "Programs", "yellow", marker_popup_text)
      }
      
    })
    
    #############################
    # Other Resources Tab
    #############################
    
    # Create reactive elements for the subsetted datasets
    parks_data <- reactive({subset_for_neighborhoods(parks, input$neighborhoods_other)})
    libraries_data <- reactive({subset_for_neighborhoods(libraries, input$neighborhoods_other)})
    rec_centers_data <- reactive({subset_for_neighborhoods(rec_centers, input$neighborhoods_other)})
    museums_data <- reactive({subset_for_neighborhoods(museums, input$neighborhoods_other)})
    playgrounds_data <- reactive({subset_for_neighborhoods(playgrounds, input$neighborhoods_other)})
    fields_data <- reactive({subset_for_neighborhoods(fields, input$neighborhoods_other)})
    
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
            add_blank_map()
        }
        else if(input$demographics_other == "Median household income ($)" ) {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_income,"MED_HH_",nbhd_labels)
        }
        else if(input$demographics_other == "High school degree or equivalent (%)") {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_edu,"PCT_HS_",nbhd_labels)
        }
        else if(input$demographics_other == "Hispanic population (%)") {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_hispanic, "PCT_HIS",nbhd_labels)
        }
        else if(input$demographics_other == "Black population (%)") {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_black, "PCT_BLA",nbhd_labels)
        }
        else if(input$demographics_other == "White population (%)") {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_white, "PCT_WHI",nbhd_labels)
        }
        else if(input$demographics_other == "Non-English speakers (%)") {
          open_resource_map <- make_base_map() %>% 
            add_demographic_map(pal_language, "PCT_NON",nbhd_labels)
        }
        else if(input$demographics_other == "All races") {
          open_resource_map <- make_base_map() %>%
            add_demographic_map(pal_all_races, "majority_race", ~shape_census@data$racial_dist_html)
        }
        
        # Loop over selected resources types, plotting the locations of each
        for (col in input$program_other){
        
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
            
            open_resource_map <- open_resource_map %>% 
              add_circle_markers(fields_data1, col, "yellow", fields_popup)
          }

        }
        
        return(open_resource_map)

      })
    
    # Make the data tables for the Other Resources Data Tab
    output$dt <- renderUI({
      lapply(as.list(seq_len(length(as.list(input$program_other)))), function(i) {
        id <- paste0("dt", i)
        DT::dataTableOutput(id)
      })
    })
    
    
    # Function to get datatables for each resources. Has a bunch of aesthetics
    data_table_function = function(checkbox_input, data, column_names){
      
      datatable(data,
                options = list(pageLength = 3, 
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css(
                                  {'background-color': '#000', 'color': '#fff'}
                                  );",
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
      for (i in seq_len(length(input$program_other))) {
        id <- paste0("dt", i)
        
        if(input$program_other[i] == "Parks"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Parks", 
                                       parks_data()[, c(3,4,5,6,7,8,11)],
                                       c("Park name", "Class", "Has nature", "Has garden", 
                                         "Has biking", "Sqft", "Nbhd name")
                                       )
            return(dat)    
          })}
        else if(input$program_other[i] == "Libraries"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Libraries", 
                                       libraries_data()[, c(3,4,5,6,9)],
                                       c("Library name", "Patron Count", 
                                         "Circulation Vol", "Sqft", "Nbhd name"))
            return(dat)    
          })
        }
       
        else if(input$program_other[i] == "Rec Centers"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Rec Centers", rec_centers_data()[, c(3,4,9:20, 23)],
                                       c("Rec Center name", "Type", "Has cardio", 
                                         "Has weights","Has gym", "Has arts culture",
                                         "Has day camps", "Has educ programs", 
                                         "Has fitness health programs", "Has senior programs",
                                         "Has social enrich clubs", "Has special events",
                                         "Has sports","Has aquatics", "Nbhd name")
                                       )
            return(dat)    
          })
        }
        else if(input$program_other[i] == "Museums"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Museums", museums_data()[, c(3,4,7)],
                                       c("Museum name", "Address", "Nbhd name"))
            return(dat)    
          })
        }
        else if(input$program_other[i] == "Fields"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Fields", 
                                       fields_data()[, c(3,4,5,6,7, 10)],
                                       c("Sport", "Location", "Tier", 
                                         "Class", "Sqft", "Nbhd name"))
            return(dat)    
          })
        }
        else if(input$program_other[i] == "Playgrounds"){
          output[[id]] <-DT::renderDataTable({
            dat <- data_table_function("Playgrounds", 
                                       playgrounds_data()[, c(3,4,5,6,9)],
                                       c("Location", "Year rehabilitated", "Class", 
                                         "Sqft", "Nbhd name"))
            return(dat)    
          })
        }
        
      })

    
  })  