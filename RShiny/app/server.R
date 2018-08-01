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
      return(subset_for_category(reschool_summer_program, input$program))
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
    
    output$mymap <- renderLeaflet({
      
      # Subset to data for only this neighborhood
      neighborhood_data1 <- neighborhood_data()
      
      # Construct pop-ups for when you click on a program marker
      program_popup_text <- make_program_popups(neighborhood_data1)
      
      ##### ACTUALLY DRAW THE RESCHOOL MAP #####
      if(input$demographics == "None selected"){
        curr_map <- make_demographic_map(NULL, NULL)
      }
      else if(input$demographics == "Median household income ($)" ) {
        curr_map <- make_demographic_map(pal_income, "MED_HH_")
      }
      else if(input$demographics == "Less than high school degree (%)") {
        curr_map <- make_demographic_map(pal_edu, col_name="PCT_LES") 
      }
      else if(input$demographics == "College graduates (%)") {
        curr_map <- make_demographic_map(pal_edu2, col_name="PCT_COL") 
      }
      else if(input$demographics == "Hispanic population (%)") {
        curr_map <- make_demographic_map(pal_hispanic, col_name="PCT_HIS") 
      }
      else if(input$demographics == "Black population (%)") {
        curr_map <- make_demographic_map(pal_black, col_name="PCT_BLA") 
      }
      else if(input$demographics == "White population (%)") {
        curr_map <- make_demographic_map(pal_white, col_name="PCT_WHI") 
      }
      else if(input$demographics == "Non-English speakers (%)") {
        curr_map <- make_demographic_map(pal_language, col_name="PCT_NON") 
      }
      else if(input$demographics == "All races") {
        labels_race_breakdown <- shape_census@data$racial_dist_html
        curr_map <- make_base_map() %>%
          add_colored_polygon_map(shape_census, pal_all_races, ~labels_race_breakdown, 
                                  "majority_race", legend_titles_demographic)
      }
      
      curr_map <- curr_map %>% add_circle_markers(neighborhood_data1, "program", myyellow, program_popup_text)
      
      if (input$neighborhoods != "No neighborhood selected") {
        curr_map <- curr_map %>% add_neighborhood_outline(input$neighborhoods)
      }
      
      return(curr_map)
    })
    
    ####### MAKE THE RESCHOOL PROGRAMS SUMMARY ANALYSIS #######
    
    # for subsetting to only the given neighborhood
    summary_data <- reactive({
      if (input$neighborhoods=="No neighborhood selected") {
        return(nbhd_program_summary[which(nbhd_program_summary[, "nbhd_name"] == input$neighborhoods),])
      }
      else {
        return(subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods))
      }
      })
    
    output$summary_title <- renderUI({
      #summary_data <- subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods)
      sprintf('<h3> "%s" Summary </h3>',
              summary_data()[, "nbhd_name"]
      ) %>% lapply(htmltools::HTML)
      
    })
    
    output$program_type_summary <- renderPlot(
      {
        #summary_data <- subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods)
        
        data <- unlist(summary_data()[,c(3:9, 12:13)])
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
      #summary_data <- subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods)
      
      sprintf("Programs with Scholarships: %i <br/> Special Needs Programs: %i <br/><br/>",
              summary_data()[, "total_scholarships"],
              summary_data()[, "total_special_needs"]
      ) %>% lapply(htmltools::HTML)
    })
    
    output$program_cost_summary <- renderPlot(
      {
        #summary_data <- subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods)
        # dummy plot just to check
        par(mar = c(3.1, 2.1, 2.1, 2.1))  # make margins same as other plot
        barplot(1,
                main = "Program Costs")
      },
      width = "auto",
      height = 250
    )
    
    output$nbhd_summary <- renderDataTable({
      #summary_data <- subset_for_neighborhoods(nbhd_program_summary, input$neighborhoods)
      
      datatable(summary_data(), 
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
        
        if (input$neighborhoods_other != "No neighborhood selected") {
          open_resource_map <- open_resource_map %>% add_neighborhood_outline(input$neighborhoods_other)
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
      
      print(input$minprice_search)
      if(input$minprice_search != ""){
        mincost_search_data = google_analytics[which(google_analytics$mincost  >= as.numeric(input$minprice_search)),]
        
      }else{
        mincost_search_data = google_analytics
      }
      

      if(input$maxprice_search != ""){
        maxcost_search_data = mincost_search_data[which(mincost_search_data$maxcost  <= as.numeric(input$maxprice_search)),]
        
      }else{
        maxcost_search_data = mincost_search_data
      }
      
      
      
      
      
      if(input$minage_search != ""){
        minage_search_data = maxcost_search_data[which(maxcost_search_data$minage  >= as.numeric(input$minage_search)),]
        
      }else{
        minage_search_data = maxcost_search_data
      }
      
      
      if(input$maxage_search != ""){
        maxage_search_data = minage_search_data[which(minage_search_data$maxage  <= as.numeric(input$maxage_search)),]
        
      }else{
        maxage_search_data = minage_search_data
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      if(input$zipcode_searchprog != "No zipcode selected" ) {
        zipcode_search_data <- subset(maxage_search_data, 
                                      maxage_search_data$location == input$zipcode_searchprog)
      }
      else {
        zipcode_search_data <- maxage_search_data
        
      }
      
      
      if(input$sessiontimes_searchprog != "No session time selected" ) {
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
    

    

    #Display the total number of searches made with this combination selected in the side bar panel
    output$totalsearches <- renderText({
      
      sprintf(
        
        "<b><i> Number of searches made in this combination </i><br/><font size=\"+3\"> %s </b>",
        sum(subset_search_data()[,"users"])
      ) 
    })
  
    #Display the total percentage of searches made with this combination selected in the side bar panel
    output$percentagesearches <- renderText({
      
      sprintf(
         "<b><i> Percentage searches </i><br/><font size=\"+3\"> %s%% </b>",
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
      "<b>Access index: %.2f</b><br/>",
      index()
      ) %>% lapply(htmltools::HTML)
    })
    
    ####### RESCHOOL PROGRAMS SUBSETTING BY COST AND TYPE #######
    
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
      return(subset_for_neighborhoods(program_cost_data_access(),input$neighborhoods_access))
    })
    
    
    
    # map it up
    output$mymap_access <- renderLeaflet({
      if (length(input$type_access)==0) {
        curr_map <- make_base_map() %>% 
          add_blank_map()
      }
      else {
        # Subset to data for only this neighborhood
        neighborhood_data_access <- neighborhood_data_access()
        #neighborhood_data_access <- program_cost_data_access()
        
        # Construct pop-ups for when you click on a program marker
        program_popup_text_access <- make_program_popups(neighborhood_data_access)
        
        curr_map <- make_base_map() %>%
          add_colored_polygon_map(shape_census_block, pal_access(), access_label(), 
                                  vals=index(), legend_title="Access Index") %>%
          add_circle_markers(neighborhood_data_access, "program", myyellow, program_popup_text_access)
      }
      if (input$neighborhoods_access!="No neighborhood selected") {
        curr_map <- curr_map %>%
          add_neighborhood_outline(input$neighborhoods_access)
      }
      return(curr_map)
    })
  })  