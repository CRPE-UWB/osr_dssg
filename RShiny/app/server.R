# Server logic for our Shiny web app.

# First run the source_file.R,
# then run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)
library(mapview)

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
                    options = list(pageLength = 5, 
                                   scrollX = TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css(
                                      {'background-color': '#000', 'color': '#fff'}
                                      );",
                                     "}")),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; color: black ;',
                      htmltools::h3("ReSchool Programs")
                    ),
                    width = 300,
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
      ) %>%
        formatStyle(colnames(data_table1[,-c(5,6,7)]),
                    backgroundColor = 'lightyellow'
        )
                    
    })
    
    ####### RESCHOOL PROGRAMS MAP #######
    
    reschool_mapdata <- reactiveValues(dat = 0)
    
    output$mymap <- renderLeaflet({
      
      # Subset to data for only this neighborhood
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
      if(input$demographics == "None selected"){
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = NULL, col_name = NULL)
      }
      else if(input$demographics == "Median household income ($)" ) {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_income, "MED_HH_")
      }
      else if(input$demographics == "Less than high school degree (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_edu,"PCT_LES")
      }
      else if(input$demographics == "College graduates (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_edu2,"PCT_COL")
      }
      else if(input$demographics == "Hispanic population (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_hispanic, "PCT_HIS") 
      }
      else if(input$demographics == "Black population (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_black, "PCT_BLA")
      }
      else if(input$demographics == "White population (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_white, "PCT_WHI")
      }
      else if(input$demographics == "Non-English speakers (%)") {
        reschool_mapdata$dat <- make_reschool_map(neighborhood_data1, marker_popup_text, pal = pal_language, "PCT_NON")
      }
      else if(input$demographics == "All races") {
        labels_race_breakdown <- shape_census@data$racial_dist_html
        
        reschool_mapdata$dat <- make_base_map() %>%
          add_colored_polygon_map(shape_census, pal_all_races, ~shape_census@data$racial_dist_html, 
                                  "majority_race", legend_titles_demographic) %>%
          add_circle_markers(neighborhood_data1, "program", myyellow, marker_popup_text)
      }
      
    })
    
    ####### MAKE THE DOWNLOAD FEATURE FOR THE RESCHOOL PROGRAMS MAP #######
    output$reschool_map_down <- downloadHandler(
      filename = 'reschool_programs_map.jpeg',
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        # set the zoom and pan based on current status of map
        reschool_mapdata$dat <-setView(reschool_mapdata$dat,
                              lat = input$mymap_center$lat,
                              lng = input$mymap_center$lng,
                              zoom = input$mymap_zoom
                              )
        
        mapshot(reschool_mapdata$dat, file = file, cliprect = "viewport")
      }
    )
    
    ####### MAKE THE RESCHOOL PROGRAMS SUMMARY ANALYSIS #######
    
    # subset to only this neighborhood
    # CAN PROBABLY REMOVE THIS FUNCTION AND REPLACE WITH THE ONE JOE WROTE
    subset_reschool_for_neighborhoods <- function(df){
      b <- reactive({
        a <- df[which(df[, "nbhd_name"] == input$neighborhoods), ]
        return(a) 
      })
      return(b)
    }
    
    output$summary_title <- renderUI({
      summary_data <- subset_reschool_for_neighborhoods(nbhd_program_summary)()
      sprintf('<h3> "%s" Summary </h3>',
              summary_data[, "nbhd_name"]
      ) %>% lapply(htmltools::HTML)
      
    })
    
    output$program_type_summary <- renderPlot(
      {
        summary_data <- subset_reschool_for_neighborhoods(nbhd_program_summary)()
      
        data <- unlist(summary_data[,c(3:9, 12:13)])
        names(data) <- c("academic", "arts", "cooking", "dance", "drama",
                         "music", "nature", "sports", "stem")
        
        par(mar = c(3.1, 5.1, 2.1, 2.1))  # make left margin larger to fit names(data)
        barplot(rev(data),  # reverse so that reads top - bottom alphabetically
                main = "Program Types",
                col = c(mygreen2, mypurple3, myblue2, 
                        mygreen, myblue3, mygreen3,
                        mypurple2, myblue, mypurple),
                horiz = TRUE,
                las = 1
                )
      },
      width = "auto",
      height = 250
    )
    
    output$program_special_cats <- renderUI({
      summary_data <- subset_reschool_for_neighborhoods(nbhd_program_summary)()
      
      sprintf("Programs with Scholarships: %i <br/> Special Needs Programs: %i <br/><br/>",
              summary_data[, "total_scholarships"],
              summary_data[, "total_special_needs"]
      ) %>% lapply(htmltools::HTML)
    })
    
    output$program_cost_summary <- renderPlot(
      {
        summary_data <- subset_reschool_for_neighborhoods(nbhd_program_summary)()
        # dummy plot just to check
        par(mar = c(3.1, 2.1, 2.1, 2.1))  # make margins same as other plot
        barplot(1,
                main = "Program Costs")
      },
      width = "auto",
      height = 250
    )
    
    output$nbhd_summary <- renderDataTable({
      summary_data <- subset_reschool_for_neighborhoods(nbhd_program_summary)()
      
      datatable(summary_data, 
                    options = list(pageLength = 1, 
                                   scrollX = TRUE,
                                   searching = FALSE,
                                   paging = FALSE,
                                   ordering = FALSE,
                                   lengthChange = FALSE,
                                   info = FALSE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css(
                                      {'background-color': '#000', 'color': '#fff'}
                                      );",
                                     "}")),
                    # caption = htmltools::tags$caption(
                    #   style = 'caption-side: top; text-align: left; color: black ;',
                    #   htmltools::h3("caption")
                    # ),
                    width = 300,
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
      ) %>%
        formatStyle(colnames(summary_data),
                    backgroundColor = 'lightblue'
        )

    })
    
  
    #### Other out of school resources tab ####
    
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
    other_mapdata <- reactiveValues(dat = 0)
    
    output$mymap_other = renderLeaflet({
        
        # Get the data
        parks_data1 <- parks_data()
        libraries_data1 <- libraries_data()
        rec_centers_data1 <- rec_centers_data()
        museums_data1 <- museums_data()
        playgrounds_data1 <- playgrounds_data()
        fields_data1 <- fields_data()
        
        ##### ACTUALLY DRAW THE OTHER RESOURCES MAP #####
        if(input$demographics_other == "None selected"){
          open_resource_map <- make_base_map() %>% add_blank_map()
        }
        else if(input$demographics_other == "Median household income ($)" ) {
          open_resource_map <- make_demographic_map(pal_income, "MED_HH_")
        }
        else if(input$demographics_other == "Less than high school degree (%)") {
          open_resource_map <- make_demographic_map(pal_edu, "PCT_LES")
        }
        else if(input$demographics_other == "College graduates (%)") {
          open_resource_map <- make_demographic_map(pal_edu2, "PCT_COL")
        }
        else if(input$demographics_other == "Hispanic population (%)") {
          open_resource_map <- make_demographic_map(pal_hispanic, "PCT_HIS")
        }
        else if(input$demographics_other == "Black population (%)") {
          open_resource_map <- make_demographic_map(pal_black, "PCT_BLA")
        }
        else if(input$demographics_other == "White population (%)") {
          open_resource_map <- make_demographic_map(pal_white, "PCT_WHI")
        }
        else if(input$demographics_other == "Non-English speakers (%)") {
          open_resource_map <- make_demographic_map(pal_language, "PCT_NON")
        }
        else if(input$demographics_other == "All races") {
          open_resource_map <- make_base_map() %>%
            add_colored_polygon_map(shape_census, pal_all_races, ~shape_census@data$racial_dist_html, 
                                    "majority_race", legend_titles_demographic)
        }
        
        # Loop over selected resources types, plotting the locations of each
        for (col in input$program_other){
          
          add_resource_markers <- function(map, data, color, popup) {
            add_circle_markers(map, data, col, color, popup, opacity = 1.0)
          }
        
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
              add_resource_markers(parks_data1, parks_color, parks_popup)
          }
         
          if(col == "Libraries"){
            libraries_popup <- sprintf(
              "<b>%s Library</b>",
              libraries_data1$name
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_resource_markers(libraries_data1, libraries_color, libraries_popup)
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
              add_resource_markers(rec_centers_data1, rec_centers_color, rec_centers_popup)
          }
          
          if(col == "Playgrounds"){
            playgrounds_popup <- sprintf(
              "<b>%s Playground</b>",
              playgrounds_data1$location
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_resource_markers(playgrounds_data1, playgrounds_color, playgrounds_popup)
          }
          
          if(col == "Museums"){
            museums_popup <- sprintf(
              "<b>%s</b>",
              museums_data1$name
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_resource_markers(museums_data1, museums_color, museums_popup)
          }
          
          if(col == "Fields"){
            fields_popup <- sprintf(
              "<b>%s Field</b><br/>
              %s",
              fields_data1$sport,
              fields_data1$location
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_resource_markers(fields_data1, fields_color, fields_popup)
          }

        }
        
        other_mapdata$dat <- open_resource_map
        return(open_resource_map)

      })
    
    # Make the download button for the other resources map
    output$other_map_down <- downloadHandler(
      filename = 'other_resources_map.jpeg',
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        # set the zoom and pan based on current status of map
        other_mapdata$dat <-setView(other_mapdata$dat,
                                       lat = input$mymap_other_center$lat,
                                       lng = input$mymap_other_center$lng,
                                       zoom = input$mymap_other_zoom
        )
        
        mapshot(other_mapdata$dat, file = file, cliprect = "viewport")
      }
    )
    
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
                               scrollX = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css(
                                  {'background-color': '#000', 'color': '#fff'}
                                  );",
                                 "}")),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: black ;',
                  htmltools::h3(checkbox_input)
                ), 
                width = 300,
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
    
    
    
    #############################
    # Search Data Tab
    #############################
    #Subset the search data depending on the slider input and the zipcode slected in the sidebar panel
    # Getting column numbers depending on the type of the program selected. 
    # (used to subset the data in the next step)
    colm_search <- reactive({
      input$program_search
      
      
    })
    
    
    subset_search_data = reactive({
      
      
      
      if(input$minprice_search != "No min price selected"){
        mincost_search_data = subset(google_analytics, google_analytics$mincost  >= input$minprice_search)
      }else{
        mincost_search_data = google_analytics
      }
      
      
      
      if(input$maxprice_search != "No max price selected"){
        maxcost_search_data = subset(mincost_search_data, mincost_search_data$maxcost  <= input$maxprice_search)
      }else{
        maxcost_search_data = mincost_search_data
      }
      
      
      
      if(input$zipcode_searchprog != "No zipcode selected" ) {
        zipcode_search_data <- subset(maxcost_search_data, 
                                      maxcost_search_data$location == input$zipcode_searchprog)
      }
      else {
        zipcode_search_data <- maxcost_search_data
        
      }
      
      
      if(input$sessiontimes_searchprog != "No session time selected" ) {
        sessiontime_search_data <- subset(zipcode_search_data, 
                                          zipcode_search_data$sessiontimes == input$sessiontimes_searchprog)
      }
      else {
        sessiontime_search_data <- zipcode_search_data
        
      }
      
      print(head(sessiontime_search_data))
      
      if(length(colm_search()) > 0){
        
        data_list = list()
        
        for(i in 1:length(colm_search())){
          
          data_list[[i]] = sessiontime_search_data[which(sessiontime_search_data$category == colm_search()[i]),]
        }
        
        final_search_data = as.data.frame(data.table::rbindlist(data_list))
       
        
      }
      
      else {
        
        final_search_data = sessiontime_search_data
      }
      
      
      return(final_search_data) 
      
      
    })
    

    

    #Display the total number of searches made with this combination selected in the side bar panel
    output$totalsearches <- renderText({
      
      sprintf(
        
        "<font size=\"+1\"><b><i> Number of searches </i><br/><font size=\"+4\"> %s </b>",
        sum(subset_search_data()[,"users"])
      ) 
    })
  
    #Display the total percentage of searches made with this combination selected in the side bar panel
    output$percentagesearches <- renderText({
      
      sprintf(
         "<font size=\"+1\"><b><i> Percentage searches </i><br/><font size=\"+4\"> %s%% </b>",
         round(((sum(subset_search_data()[,"users"])*100)/sum(google_analytics$users)), 2)
      ) 
    })
  
    
    
    # Output the relevant data in the data tab based on the search data tab
    output$datatable_search <- DT::renderDataTable({
      data_table1 <- subset_search_data()
      DT::datatable(data_table1, 
                    options = list(pageLength = 10, 
                                   scrollX = TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css(
                                     {'background-color': '#000', 'color': '#fff'}
                                   );",
                                     "}")),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: center; color: black ;',
                      htmltools::h3("Search Data")
                    ), 
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
      ) %>%
        formatStyle(colnames(data_table1),
                    backgroundColor = 'lightblue'
        )
      
    })
    
    
    
    
    
    
    
    
    #############################
    # Access Index Tab
    #############################
    
    # first calculate the aggregated access index based on user input
    index <- reactive({calculate_aggregated_index(input$drive_or_transit,input$type_access,input$cost_access)})
    # Bins and color palettes for demographic variables in leaflet map
    pal_access <- reactive({colorBin("Blues", domain = index())})
    
    #output$test <- renderPrint({index()})
    
    # Create labels and stuff
    access_label <- reactive({sprintf(
      "<b>Access index: %f</b><br/><b>Block Group: %f</b>",
      index(), as.numeric(as.character(shape_census_block@data$Id2))
      ) %>% lapply(htmltools::HTML)
    })
    
    # map it up
    output$mymap_access <- renderLeaflet({
      map <- make_base_map() %>%
        add_colored_polygon_map(shape_census_block, pal_access(), access_label(), 
                                vals=index(), legend_title="Access Index")
      return(map)
    })
  })  