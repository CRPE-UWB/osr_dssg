# Server logic for our Shiny web app.

# Run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)
library(mapview)

shinyServer(
  function(input, output, session) {
    
    ############################################################################################################
    # Reschool Programs Tab
    ############################################################################################################
    
    ####### RESCHOOL PROGRAMS SUBSETTING BY COST AND TYPE #######
    
    program_category_data <- reactive({
      cat_dat <- subset_for_category(reschool_summer_program, input$program)
      if ( !is.null(input$special_needs) ) {
        cat_dat <- subset_for_special_needs(cat_dat)
      }
      return(cat_dat)
    })
    
    program_cost_and_type_data <- reactive({
      return(subset_for_cost(program_category_data(),input$slider[1],input$slider[2]))
    })
    
    neighborhood_data <- reactive({
        return(subset_for_neighborhoods(program_cost_and_type_data(),input$neighborhoods))
    })
    
    ####### RESCHOOL PROGRAMS DATA TAB #######
    
    # for subsetting to only the given neighborhood
    summary_data <- reactive({
      return(subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods))
    })
    
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
                    backgroundColor = '#c6dbef'
        )
                    
    })
    
    output$download_reschool_data <- downloadHandler(
      filename = "b4s_programs.csv",
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        write.csv(neighborhood_data(), file, row.names = FALSE)
      }
    )
    
    ####### RESCHOOL PROGRAMS MAP #######
    
    reschool_mapdata <- reactiveValues(dat = 0)
    
    programs_per_nbhd <- reactive({
      if (nrow(program_cost_and_type_data())==0) {return(rep(0, nrow(shape_census)))}
      program_frequencies <- table(program_cost_and_type_data()[,"nbhd_name"])
      tmp <- data.frame("nbhd_name"=names(program_frequencies), "freq"=as.vector(program_frequencies))
      tmp <- merge(tmp, data.frame("nbhd_name"=shape_census@data[,"NBHD_NA"]), by="nbhd_name", all.y=TRUE)
      tmp <- tmp[order(as.character(tmp$nbhd_name)),]
      tmp$freq[is.na(tmp$freq)] <- 0
      return(tmp$freq)
    })
    
    nbhd_labels_reactive <- reactive({get_nbhd_census_labels(programs_per_nbhd())})
    nbhd_labels_student_reactive <- reactive({get_nbhd_student_labels(programs_per_nbhd())})
    
    output$mymap <- renderLeaflet({
      nbhd_labels <- nbhd_labels_reactive() 
      nbhd_labels_student <- nbhd_labels_student_reactive()
      # Subset to data for only this neighborhood
      neighborhood_data1 <- neighborhood_data()
      
      # Construct pop-ups for when you click on a program marker
      program_popup_text <- make_program_popups(neighborhood_data1)
      
      labFormatAge = function(type, cuts, p) {
       n = length(cuts)
       paste0(round(cuts[-n]), " &ndash; ", round(cuts[-1]))
      }
      
      ##### ACTUALLY DRAW THE RESCHOOL MAP #####
      curr_map <- create_demographic_map(input$school_or_census, input$demographics, input$student_demographics,
                                         census_labels=nbhd_labels, student_labels=nbhd_labels_student)
      curr_map <- curr_map %>% add_circle_markers(neighborhood_data1, "program", myyellow, 
                                                  program_popup_text, weight = 0.7, opacity = 0.8)
      
      # Outline the selected neighborhoods!
      curr_map <- add_neighborhoods_outline(curr_map, input$neighborhoods)
      
      reschool_mapdata$dat <- curr_map
      return(curr_map)
    })
    
    observeEvent(input$mymap_shape_click, {
      if (!is.null(input$mymap_shape_click$id)) {
        if (input$mymap_shape_click$id %in% input$neighborhoods) {
          new_choices <- input$neighborhoods[!input$neighborhoods==input$mymap_shape_click$id]
        } else {
          new_choices <- c(input$neighborhoods, input$mymap_shape_click$id)
        }
        updateSelectInput(session, "neighborhoods", selected = new_choices)
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

    output$summary_title <- renderUI({
      summary_nbhds <- summary_data()[, "nbhd_name"]
      if ("No neighborhood selected" %in% summary_nbhds){
        summary_nbhds <- "All Neighborhoods"
      }

      sprintf('<h3>Summary for %s</h3>',
              toString(summary_nbhds)
      ) %>% lapply(htmltools::HTML)
    })
    
    output$program_type_summary <- renderPlotly({
      
      # format the data properly
      data_names <- c("academic", "arts", "cooking", "dance", "drama",
                      "music", "nature", "sports", "stem")
      relevant_colnames <- c("total_academic", "total_arts", "total_cooking", "total_dance", "total_drama",
                             "total_music", "total_nature", "total_sports", "total_stem")

      if (nrow(summary_data())==0) {
        dat <- rep(0,9)
      } else {
        dat <- colSums(summary_data()[,relevant_colnames])
        dat <- unlist(dat)
      }

      names(dat) <- data_names
      
      # actually make the plot
      plot_ly(x = sort(data_names, decreasing = TRUE),
              y = dat[sort(data_names, decreasing = TRUE)],
              type = "bar"
              ) %>%
        layout(xaxis = list(title = "Program Category"), 
               yaxis = list(title = "No. Programs"),  
               title = "Programs by Category"
               )
      
    })
    
    output$program_special_cats <- renderUI({
      if (nrow(summary_data())==0) {
        sprintf("No programs in this neighborhood.") %>% lapply(htmltools::HTML)
      } else {
        sprintf("<center><b>Programs with Scholarships: %i <br><br> 
                Programs Accommodating Special Needs: %i </b> <br><br></center>",
                sum(summary_data()[, "total_scholarships"]),
                sum(summary_data()[, "total_special_needs"])
        ) %>% lapply(htmltools::HTML)
      }
    })
    
    output$program_cost_summary <- renderPlotly({
      nbhd_cost_data <- subset_for_neighborhoods(reschool_summer_program, input$neighborhoods)
      nbhd_cost_data <- nbhd_cost_data[,"session_cost"]
      
      # par(mar = c(5.1, 5.1, 2.1, 2.1))  # set margins
      
      # actually make the plot
      if (sum(nbhd_cost_data > 0) >0 ){
        plot_ly(x = ~nbhd_cost_data[nbhd_cost_data > 0],
                type = "histogram",
                name = "Not Free"
        ) %>%
          layout(xaxis = list(title = "Total Cost ($)"), 
                 yaxis = list(title = "No. Programs"), 
                 title = "Programs by Cost"
          ) %>%
          add_bars(x = 0,
                   y = sum(nbhd_cost_data == 0),
                   name = "Free"
          )
      }
      else{
        plot_ly(y = sum(nbhd_cost_data == 0),
                x = 0,
                type = "bar",
                color = "orange"
        ) %>%
          layout(xaxis = list(title = "Total Cost ($)"), 
                 yaxis = list(title = "No. Programs"), 
                 title = "Programs by Cost"
          )
        
        
      }
        
    
    })
    
    # Data table for nbhd summary - deprecated
    # output$nbhd_summary <- renderDataTable({
    #   datatable(summary_data(), 
    #                 options = list(pageLength = 3, 
    #                                scrollX = TRUE,
    #                                searching = FALSE,
    #                                paging = FALSE,
    #                                ordering = FALSE,
    #                                lengthChange = FALSE,
    #                                info = FALSE,
    #                                initComplete = JS(
    #                                  "function(settings, json) {",
    #                                  "$(this.api().table().header()).css(
    #                                   {'background-color': '#000', 'color': '#fff'}
    #                                   );",
    #                                  "}")),
    #                 # caption = htmltools::tags$caption(
    #                 #   style = 'caption-side: top; text-align: left; color: black ;',
    #                 #   htmltools::h3("caption")
    #                 # ),
    #                 width = 300,
    #                 style = "bootstrap",
    #                 class = 'cell-border stripe',
    #                 rownames = FALSE
    #                 
    #   ) %>%
    #     formatStyle(colnames(summary_data),
    #                 backgroundColor = '#c6dbef'
    #     )
    # 
    # })
    
  
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
    
    nbhd_labels_reactive_other <- reactive({get_nbhd_census_labels()})
    nbhd_labels_student_reactive_other <- reactive({get_nbhd_student_labels()})
    
    output$mymap_other = renderLeaflet({
        
        # Get the data
        parks_data1 <- parks_data()
        libraries_data1 <- libraries_data()
        rec_centers_data1 <- rec_centers_data()
        museums_data1 <- museums_data()
        playgrounds_data1 <- playgrounds_data()
        fields_data1 <- fields_data()
        
        ##### ACTUALLY DRAW THE OTHER RESOURCES MAP #####
        
        open_resource_map <- create_demographic_map(input$school_or_census_other, input$demographics_other, input$student_demographics_other, 
                                                    nbhd_labels_reactive_other(), nbhd_labels_student_reactive_other())
        
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
        
        # Outline selected neighborhoods
        open_resource_map <- add_neighborhoods_outline(open_resource_map, input$neighborhoods_other)

        other_mapdata$dat <- open_resource_map
        return(open_resource_map)
      })
    
    observeEvent(input$mymap_other_shape_click, {
      if (!is.null(input$mymap_other_shape_click$id)) {
        if (input$mymap_other_shape_click$id %in% input$neighborhoods_other) {
          new_choices <- input$neighborhoods_other[!input$neighborhoods_other==input$mymap_other_shape_click$id]
        } else {
          new_choices <- c(input$neighborhoods_other, input$mymap_other_shape_click$id)
        }
        updateSelectInput(session, "neighborhoods_other", selected = new_choices)
      }
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
        return(list(DT::dataTableOutput(id), 
                    downloadButton(paste0("download_", id), label = "Download Data"),
                    br(), br()
                    ))
      })
    })
    
    # Function to get datatables for each resources. Has a bunch of aesthetics
    data_table_function <- function(checkbox_input, data, column_names){
      
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
                    backgroundColor = '#c6dbef'
        )
    }
    
    observe(
      for (i in seq_len(length(input$program_other))) {
        id <- paste0("dt", i)
        download_id <- paste0("download_", id)
        
        if(input$program_other[i] == "Parks"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Parks", 
                                       parks_data()[, c(3,4,5,6,7,8,11)],
                                       c("Park name", "Class", "Has nature", "Has garden", 
                                         "Has biking", "Sqft", "Nbhd name")
                                       )
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "parks.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(parks_data(), file, row.names = FALSE)
            }
          )
          
          }
        else if(input$program_other[i] == "Libraries"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Libraries", 
                                       libraries_data()[, c(3,4,5,6,9)],
                                       c("Library name", "Patron Count", 
                                         "Circulation Vol", "Sqft", "Nbhd name"))
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "libraries.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(libraries_data(), file, row.names = FALSE)
            }
          )
          
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
          
          output[[download_id]] <- downloadHandler(
            filename = "rec_centers.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(rec_centers_data(), file, row.names = FALSE)
            }
          )
          
        }
        else if(input$program_other[i] == "Museums"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Museums", museums_data()[, c(3,4,7)],
                                       c("Museum name", "Address", "Nbhd name"))
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "museums.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(museums_data(), file, row.names = FALSE)
            }
          )
          
        }
        else if(input$program_other[i] == "Fields"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Fields", 
                                       fields_data()[, c(3,4,5,6,7, 10)],
                                       c("Sport", "Location", "Tier", 
                                         "Class", "Sqft", "Nbhd name"))
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "fields.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(fields_data(), file, row.names = FALSE)
            }
          )
          
        }
        else if(input$program_other[i] == "Playgrounds"){
          output[[id]] <-DT::renderDataTable({
            dat <- data_table_function("Playgrounds", 
                                       playgrounds_data()[, c(3,4,5,6,9)],
                                       c("Location", "Year rehabilitated", "Class", 
                                         "Sqft", "Nbhd name"))
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "playgrounds.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(playgrounds_data(), file, row.names = FALSE)
            }
          )
          
        }
        
      })
    
    
    
    ###################################################################################################################
    # Search Data Tab
    ###################################################################################################################
    # Subset the search data depending on the slider input and the zipcode selected in the sidebar panel
    # Getting column numbers depending on the type of the program selected. 
    # (used to subset the data in the next step)
    colm_search <- reactive({
      input$program_search
    })
    
    # Subsetting the data depending on the various selections made in the sidebar panel 
    subset_search_data = reactive({
      
      if(input$minprice_search != ""){
        mincost_search_data = google_analytics[which(google_analytics$mincost  >= as.numeric(input$minprice_search)),]
      }
      else {
        mincost_search_data = google_analytics
      }
      
      if(input$maxprice_search != ""){
        maxcost_search_data = mincost_search_data[which(mincost_search_data$maxcost  <= as.numeric(input$maxprice_search)),]
      }
      else {
        maxcost_search_data = mincost_search_data
      }
      
      if(input$minage_search != ""){
        minage_search_data = maxcost_search_data[which(maxcost_search_data$minage  >= as.numeric(input$minage_search)),]
      }
      else{
        minage_search_data = maxcost_search_data
      }
      
      if(input$maxage_search != ""){
        maxage_search_data = minage_search_data[which(minage_search_data$maxage  <= as.numeric(input$maxage_search)),]
      }
      else{
        maxage_search_data = minage_search_data
      }
      
      if(input$zipcode_searchprog != "N/A" ) {
        zipcode_search_data <- subset(maxage_search_data, 
                                      maxage_search_data$location == input$zipcode_searchprog)
      }
      else {
        zipcode_search_data <- maxage_search_data
      }
      
      if(input$sessiontimes_searchprog != "N/A" ) {
        sessiontime_search_data <- subset(zipcode_search_data, 
                                          zipcode_search_data$sessiontimes == input$sessiontimes_searchprog)
      }
      else {
        sessiontime_search_data <- zipcode_search_data
      }
      
      if(length(colm_search()) != 0){
        data_list = list()
        for(i in 1:length(colm_search())){
          data_list[[i]] = sessiontime_search_data[which(sessiontime_search_data$category == colm_search()[i]),]
        }
        category_search_data = as.data.frame(data.table::rbindlist(data_list))
      }
      else {
        category_search_data = sessiontime_search_data
      }
      
      if(input$specialneeds_search == "Special needs students" ) {
        final_search_data <- subset(category_search_data, 
                                    category_search_data$specialneeds  == "specialNeedsStudent")
      }
      else if(input$specialneeds_search == "Scholarships Available" ) {
        final_search_data <- subset(category_search_data, 
                                    category_search_data$scholarships  == "scholarshipsAvailable")
      }
      else {
        final_search_data <- category_search_data
      }
      
      return(final_search_data) 
      
    })

    # Display the total number of searches made with this combination selected in the side bar panel
    output$totalsearches <- renderText({
      
      sprintf(
        "<b><i> Number of searches with these filters:</i><br/><font size=\"+3\"> %s </b>",
        sum(subset_search_data()[,"users"])
      ) 
    })
  
    # Display the total percentage of searches made with this combination selected in the side bar panel
    output$percentagesearches <- renderText({
      
      sprintf(
         "<b><i> Percentage of searches with these filters:</i><br/><font size=\"+3\"> %s%% </b>",
         round(((sum(subset_search_data()[,"users"])*100)/sum(google_analytics$users)), 2)
      ) 
    })
  
    # Output the relevant data in the data tab based on the search data tab
    output$datatable_search <- DT::renderDataTable({
      data_table1 <- subset_search_data()
      DT::datatable(data_table1, 
                    options = list(pageLength = 7, 
                                   scrollX = TRUE,
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css(
                                     {'background-color': '#000', 'color': '#fff'}
                                   );",
                                     "}")),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; color: black;',
                      htmltools::h3("Search Data")
                    ),
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
      ) %>%
        formatStyle(colnames(data_table1),
                    backgroundColor = '#c6dbef'
        )
      
    })
    
    output$download_search_data <- downloadHandler(
      filename = "b4s_searches.csv",
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        write.csv(subset_search_data(), file, row.names = FALSE)
      }
    )
    

    #################### Rendering plots for visualization tab in the search data tab #################################
    # 'Sort by' variable graph
    output$search_sort_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What distances and session times do people search for, and how do they sort their results?", message=FALSE))
      search_sort_summary %>%
        plot_ly(labels = ~sort, values = ~total_searches) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Search Results Most 'Sorted By'",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    })
    
    # Sessiontimes variable graph
    output$search_sessiontimes_plot = renderPlotly({
      validate(need(input$specific_search_questions=="What distances and session times do people search for, and how do they sort their results?", message=FALSE))
      
      search_sessiontimes_summary %>%
        plot_ly(labels = ~sessiontimes, values = ~total_searches) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Searches by Session Time",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    })
    
    # Distance graph
    output$search_distance_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What distances and session times do people search for, and how do they sort their results?", message=FALSE))
      
      xform <- list(categoryorder = "array",
                    categoryarray = c(search_distance_summary$distance))
      plot_ly(data = search_distance_summary,
              x = ~distance,
              y = ~total_searches,
              type = "bar" ) %>%
        layout(xaxis = list(title = "Maximum Distance in Miles"), 
               yaxis = list(title = "No. Searches"), 
               title = "Searches by Distance") %>% 
        layout(xaxis = xform)
      
    })
    
    #Zipcode searches graph
    output$search_zipcode_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What locations are people searching for?", message=FALSE))
      
      xform <- list(categoryorder = "array",
                    categoryarray = c(search_zipcode_summary$location))
      plot_ly(data = search_zipcode_summary,
              x = ~location,
              y = ~total_searches,
              type = "bar" ) %>%
        layout(xaxis = list(title = "zipcode"), 
               yaxis = list(title = "Number of searches"), 
               title = "Number of searches by zipcode") %>% 
        layout(xaxis = xform)
      
      
    })
    
    #Zipcode sessions graph
    output$search_programs_zipcode_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What locations are people searching for?", message=FALSE))
      
      xform <- list(categoryorder = "array",
                    categoryarray = c(final_zipcode_searches_programs$location))
      
      plot_ly(final_zipcode_searches_programs, x = ~location, y = ~total_sessions, type = 'bar', 
              name = 'No. Searches', color = I("dark orange"))%>%
        layout(yaxis = list(title = 'Count'), barmode = 'group', title = 'Number of programs by zipcode' ) %>% 
        layout(xaxis = xform)

    })
    
    # Bubble graph for comparing programs
    output$search_compare_prog_category = renderPlotly({
      validate(need(input$specific_search_questions=="What program categories do people search for the most?", message=FALSE))
      
      p = ggplot(programs_sessions, aes(x= `Percentage of searches`, 
                                        y= `Percentage of programs`, 
                                        #size = 0.5, 
                                        color = category)
                 ) + 
        geom_abline(slope = 1, intercept = 0, color = "lightgray") +
        geom_point(size = 4.0) + 
        xlim(0,NA) + ylim(0,NA) +
        theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        stat_smooth(method="lm", se=FALSE) + 
        theme(legend.title=element_blank()) + 
        ggtitle("Prevalence of Searches vs. Programs, by Category")
      
      # Converting this plot into a ggplotly output
      p <- p + guides(size=FALSE)
      p <- p + scale_size_continuous(guide=FALSE) 
      ggplotly(p) 
      
      
    })
    
    # Number of searches by program category graph
    output$search_prog_category = renderPlotly({
      validate(need(input$specific_search_questions=="What program categories do people search for the most?", message=FALSE))
      
      plot_ly(data = search_programtype_summary,
              x = ~category,
              y = ~total_searches,
              name = "Searches by Program Category",
              type = "bar" ) %>%
        layout(xaxis = list(title = "Program Categories"), 
               yaxis = list(title = "No. Searches"), 
               title = "Searches by Program Category")
      
    })
    
    ###################################################################################################################
    # Access Index Tab
    ###################################################################################################################
    
    # first calculate the aggregated access index based on user input
    index <- reactive({calculate_aggregated_index(input$drive_or_transit,input$type_access,input$cost_access)})
    # Bins and color palettes for demographic variables in leaflet map
    pal_access <- reactive({
      if (input$drive_or_transit=="drive") {return(colorBin("Blues", domain = index()))}
      else {return(colorBin("Greens", domain = index()))}
      })
    
    # Create labels and stuff
    access_label <- reactive({get_block_census_labels(index())})
    
    ####### PROGRAM SUBSETTING BY COST AND TYPE #######
    
    program_list <- list("academic"=c("has_academic","has_stem"),
                         "sports"=c("has_sports"),
                         "art"=c("has_cooking","has_dance","has_drama","has_music","has_arts"),
                         "nature"=c("has_nature"))

    program_category_data_access <- reactive({
      return(subset_for_category(reschool_summer_program,
                                 unlist(program_list[input$type_access], use.names=F)))
    })

    program_cost_data_access <- reactive({
      cost_mapping <- list("free"=0, "low"=50, "any"=max(reschool_summer_program$session_cost))
      return(subset_for_cost(program_category_data_access(),0,cost_mapping[input$cost_access]))
    })

    neighborhood_data_access <- reactive({
      #return(program_cost_data_access())
      return(subset_for_neighborhoods(program_cost_data_access(),input$neighborhoods_access))
    })
    
    # map it up
    access_mapdata <- reactiveValues(dat = 0)
    
    output$mymap_access <- renderLeaflet({
      if (length(input$type_access)==0) {
        curr_map <- make_base_map() %>% 
          add_blank_map()
      }
      else {
        # Subset to data for only this neighborhood
        neighborhood_data_access <- neighborhood_data_access()
        
        # Construct pop-ups for when you click on a program marker
        program_popup_text_access <- make_program_popups(neighborhood_data_access)
        
        curr_map <- make_base_map() %>%
          add_colored_polygon_map(shape_census_block, pal_access(), access_label(), 
                                  vals=index(), legend_title="Access Index", my_weight=.3) %>%
          add_circle_markers(neighborhood_data_access, "program", myyellow, program_popup_text_access)
      }
      
      curr_map <- add_neighborhoods_outline(curr_map, input$neighborhoods_access)
      
      access_mapdata$dat <- curr_map
      return(curr_map)
    })
    
    ####### MAKE THE DOWNLOAD FEATURE FOR THE ACCESS INDEX MAP #######
    output$access_map_down <- downloadHandler(
      filename = 'access_index_map.jpeg',
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        # set the zoom and pan based on current status of map
        access_mapdata$dat <-setView(access_mapdata$dat,
                                       lat = input$mymap_access_center$lat,
                                       lng = input$mymap_access_center$lng,
                                       zoom = input$mymap_access_zoom
        )
        
        mapshot(access_mapdata$dat, file = file, cliprect = "viewport")
      }
    )
    
  })  

