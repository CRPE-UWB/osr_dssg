# Server logic for our Shiny web app.

# Run the app by clicking 'Run App' above.

library(shiny)
library(DT)
library(leaflet)
library(sp)
library(mapview)
library(ineq) # for creating the Lorenz curve
library(DescTools)  # for computing the gini coefficient

shinyServer(
  function(input, output, session) {
    
    ################################################################################################
    # Reschool Programs Tab
    ################################################################################################
    
    ####### RESCHOOL PROGRAMS SUBSETTING BY COST AND TYPE #######
    
    program_category_data <- reactive({
      cat_dat <- subset_for_category(reschool_summer_program, input$program)
      if ( !is.null(input$special_needs) ) {
        cat_dat <- subset_for_special_needs(cat_dat)
      }
      return(cat_dat)
    })
    
    program_cost_and_type_data <- reactive({
      temp <- subset_for_cost(program_category_data(),input$slider[1],input$slider[2])
      # also subset for # hours and days
      temp2 <- subset_for_hours(temp, input$hours_slider[1], input$hours_slider[2])
      return(subset_for_days(temp2, input$days_slider[1], input$days_slider[2]))
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
      DT::datatable(data_table1[,-which( names(data_table1) %in% c("lat","long","nbhd_id") )], 
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
                      htmltools::h3("Blueprint4Summer Programs")
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
      filename = "b4s_programs_selected.csv",
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
    #nbhd_labels_student_reactive <- reactive({get_nbhd_student_labels(programs_per_nbhd())})
    
    output$mymap <- renderLeaflet({
      nbhd_labels <- nbhd_labels_reactive() 
      #nbhd_labels_student <- nbhd_labels_student_reactive()
      # Subset to data for only this neighborhood
      neighborhood_data1 <- neighborhood_data()
      
      # Construct pop-ups for when you click on a program marker
      program_popup_text <- make_program_popups(neighborhood_data1)
      
      labFormatAge = function(type, cuts, p) {
       n = length(cuts)
       paste0(round(cuts[-n]), " &ndash; ", round(cuts[-1]))
      }
      
      ##### ACTUALLY DRAW THE RESCHOOL MAP #####
      curr_map <- create_demographic_map("census_dems", input$demographics, input$student_demographics,
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
    
    ####### MAKE THE RESCHOOL PROGRAMS Summary Analysis #######

    output$summary_title <- renderUI({
      summary_nbhds <- input$neighborhoods
      if ("All neighborhoods" %in% summary_nbhds){
        summary_nbhds <- "All Neighborhoods"
      } 
      else if (length(summary_nbhds)==0) {
        summary_nbhds <- "(No Neighborhoods Selected)"
      }

      sprintf('<h3>Program Summary for %s</h3>',
              toString(summary_nbhds)
      ) %>% lapply(htmltools::HTML)
    })
    
    output$nbhd_census_demog_summary <- renderUI({
      # subset to the selected neighborhoods
      summary_data <- subset_for_neighborhoods(shape_census@data, input$neighborhoods)
      
      # aggregate the demographics over all selected neighborhoods
      # dummy transformations for now
      summary_AGE_5_T <- sum(summary_data$AGE_5_T)
      summary_MED_HH_ <- summary_data$MED_HH_
      summary_PCT_LES <- sum(summary_data$PCT_LES * summary_data$POP_EDU) / sum(summary_data$POP_EDU)
      summary_PCT_COL <- sum(summary_data$PCT_COL * summary_data$POP_EDU) / sum(summary_data$POP_EDU)
      summary_PCT_NON <- sum(summary_data$PCT_NON * summary_data$POP_LAN) / sum(summary_data$POP_LAN)
      summary_PCT_HIS <- sum(summary_data$PCT_HIS * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      summary_PCT_WHI <- sum(summary_data$PCT_WHI * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      summary_PCT_BLA <- sum(summary_data$PCT_BLA * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      
      sprintf("<h4>Census Demographics</h4> 
              No. children 5-17 yrs old = %s <br>
              < HS Degree (%% Over 25) = %.1f%% <br>
              College Graduates (%% Over 25) = %.1f%% <br>
              %% Language Besides English Spoken = %.1f%% <br>
              %% Hispanic Population = %.1f%% <br>
              %% White population = %.1f%% <br>
              %% Black population = %.1f%%<br>",
              format(summary_AGE_5_T, big.mark = ","),
              summary_PCT_LES,
              summary_PCT_COL,
              summary_PCT_NON,
              summary_PCT_HIS,
              summary_PCT_WHI,
              summary_PCT_BLA #,
              #format(min(summary_MED_HH_), big.mark = ","), 
              #format(max(summary_MED_HH_), big.mark = ",")
      ) %>% lapply(htmltools::HTML)
    })
    
    output$med_income_summary <- renderPlotly({
      summary_data <- subset_for_neighborhoods(shape_census@data, input$neighborhoods)
      
      if (nrow(summary_data)>0) {
        plot_ly(data = summary_data, 
                x = ~MED_HH_,
                y = "",
                text = as.character(summary_data$nbhd_name),
                type = "scatter",
                mode = "markers",
                marker = list(size = 15),
                alpha = 0.5,
                height = 100,
                hoverinfo = 'text'
        ) %>%
          layout( xaxis = list(title = "", range = c(0,NULL)),
                  title = "Median HH Income ($)",
                  titlefont = list(size = 12),
                  margin = list(l = 0)
          ) %>%
          config(displayModeBar = FALSE)
      }
      
    })
    
    # output$nbhd_student_demog_summary <- renderUI({
    #   # subset to the selected neighborhoods
    #   if ("No neighborhood selected" %in% input$neighborhoods){
    #     summary_data_student <- aggregate_dps_student_nbhds
    #   }
    #   else{
    #     summary_data_student <- subset_for_neighborhoods(aggregate_dps_student_nbhds, input$neighborhoods)
    #   }
    #   
    #   # aggregate the demographics over all selected neighborhoods
    #   total_nbhd_students <- sum(summary_data_student$total_students, na.rm = TRUE)
    #   
    #   if (total_nbhd_students < 10){
    #     summary_perc_el_students <- NA
    #     summary_perc_disable_students <- NA
    #     summary_perc_hispanic_students <- NA
    #     summary_perc_white_students <- NA
    #     summary_perc_black_students <- NA
    #   }
    #   else{
    #     summary_perc_el_students <- sum(summary_data_student$perc_el_students * summary_data_student$total_students, 
    #                                             na.rm = TRUE) / total_nbhd_students
    #     summary_perc_disable_students <- sum(summary_data_student$perc_disable_students * summary_data_student$total_students, 
    #                                          na.rm = TRUE) / total_nbhd_students
    #     summary_perc_hispanic_students <- sum(summary_data_student$perc_hispanic_students * summary_data_student$total_students, 
    #                                           na.rm = TRUE) / total_nbhd_students
    #     summary_perc_white_students <- sum(summary_data_student$perc_white_students * summary_data_student$total_students, 
    #                                        na.rm = TRUE) / total_nbhd_students
    #     summary_perc_black_students <- sum(summary_data_student$perc_black_students * summary_data_student$total_students, 
    #                                        na.rm = TRUE) / total_nbhd_students
    #   }
    #   
    #   # Print it!
    #   sprintf(
    #     "<h4>Student Demographics</h4>
    #     %% English student learners = %.1f%% <br>
    #     %% Students with disability = %.1f%% <br>
    #     %% Hispanic students = %.1f%% <br>
    #     %% White students = %.1f%% <br>
    #     %% Black students = %.1f%% <br><br>
    #     <i>Sample size = %s students</i>",
    #     summary_perc_el_students,
    #     summary_perc_disable_students,
    #     summary_perc_hispanic_students,
    #     summary_perc_white_students,
    #     summary_perc_black_students,
    #     format(total_nbhd_students, big.mark = ",")
    #   ) %>% lapply(htmltools::HTML)
    # })
    
    output$program_summary_plot <- renderPlotly({
      
      ###########################
      # Case 1: Category Analysis
      ###########################
      if (input$program_analysis == "category_question"){
        
        # get data
        data_names <- c("academic", "arts", "cooking", "dance", "drama",
                        "music", "nature", "sports", "stem", "scholarships", "special_needs")
        relevant_colnames <- c("total_academic", "total_arts", "total_cooking", "total_dance", "total_drama",
                               "total_music", "total_nature", "total_sports", "total_stem", "total_scholarships",
                               "total_special_needs")
        if (nrow(summary_data())==0) {
          dat <- rep(0,length(data_names))
        } else {
          dat <- colSums(summary_data()[,relevant_colnames])
          dat <- unlist(dat)
        }
        names(dat) <- data_names
        
        # make the plot
        plot_ly(x = sort(data_names, decreasing = TRUE),
                y = dat[sort(data_names, decreasing = TRUE)],
                type = "bar"
        ) %>%
          layout(xaxis = list(title = "Category"), 
                 yaxis = list(title = "No. Programs"),  
                 title = "Programs by Category",
                 margin = list(b=100),
                 annotations = list(x  = 0, 
                                    y = 0,
                                    text = paste("<i>Programs w/ Scholarships:", 
                                                 sum(summary_data()[, "total_scholarships"]),
                                                 "<br>Programs for Special Needs:",
                                                 sum(summary_data()[, "total_special_needs"]),
                                                 "</i>"
                                                 ),
                                    showarrow = FALSE,
                                    xref = "paper",
                                    yref = "paper",
                                    xshift = -10,
                                    yshift = -100
                                    )
          )
      }
      #######################
      # Case 2: Cost Analysis
      #######################
      else if (input$program_analysis == "cost_question"){
        
        # get the data
        nbhd_cost_data <- subset_for_neighborhoods(reschool_summer_program, input$neighborhoods)
        nbhd_cost_data <- nbhd_cost_data[,"cost_per_hour"]
        
        # make the plot - if at least one not-free program
        if (sum(nbhd_cost_data > 0, na.rm = TRUE) > 0 ){
          plot_ly(x = ~nbhd_cost_data[nbhd_cost_data > 0],
                  type = "histogram",
                  name = "Not Free"
          ) %>%
            layout(xaxis = list(title = "Cost per Hour ($)"), 
                   yaxis = list(title = "No. Programs"), 
                   title = "Programs by Cost"
            ) %>%
            add_bars(x = 0,
                     y = sum(nbhd_cost_data == 0, na.rm = TRUE),
                     name = "Free"
            )
        }
        # make the plot - if all programs are free
        else{
          plot_ly(y = sum(nbhd_cost_data == 0, na.rm = TRUE),
                  x = 0,
                  type = "bar",
                  color = "orange"
          ) %>%
            layout(xaxis = list(title = "Cost per Hour ($)"), 
                   yaxis = list(title = "No. Programs"), 
                   title = "Programs by Cost"
            )
        }
      }
      #######################
      # Case 3: Date Analysis
      #######################
      else {
        
        # get the data
        dat <- subset_for_neighborhoods(reschool_summer_program, input$neighborhoods)
        relevant_colnames <- c("session_date_start", "session_date_end", "session_name")
        if (nrow(dat)==0) {
          dat <- data.frame(as.Date("2000-06-16"), as.Date("3000-06-16"), "Not a Real Program")
          names(dat) <- relevant_colnames
        } else {
          dat <- dat[,relevant_colnames]
          dat$session_date_start <- as.Date(dat$session_date_start)
          dat$session_date_end <- as.Date(dat$session_date_end)
          dat <- dat[ do.call(order, dat), ]
        }
        
        # make the plot
        p1 <- plot_ly(data = dat) %>%
          add_segments(x = ~session_date_start,
                       xend = ~session_date_end,
                       y = 1:nrow(dat),
                       yend = 1:nrow(dat),
                       text = ~session_name,
                       hoverinfo = 'text',
                       name = "Middle of Program"
          ) %>%
          add_markers(x = c(dat$session_date_start, dat$session_date_end),
                      y = rep(1:nrow(dat),2),
                      color = I("grey"),
                      text = rep(dat$session_name,2),
                      hoverinfo = 'text',
                      name = "Start/End Dates"
          ) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = "", showticklabels = FALSE),
                 title = "Programs by Date",
                 showlegend = FALSE
          )
        
        # different kind of plot
        get_all_dates <- function(start_date, end_date) {
          return(seq.Date(start_date, end_date, by = "days"))
        }
        
        good_idxs <- ! (is.na(dat$session_date_start) | is.na(dat$session_date_end))
        all_dates <- mapply(get_all_dates, dat$session_date_start[good_idxs], 
                            dat$session_date_end[good_idxs])
        p2 <- plot_ly(x = all_dates,
                      type = "histogram",
                      name = "Total Programs<br>On This Day",
                      color = "grey") %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = "No. Programs"),
                 title = "Number of Programs Happening Over Time"
                 )

        output_plot <- subplot(p1, p2, nrows = 2, shareX = TRUE)

      }
      
      # end cases for analysis questions
      
    })
    
    ################################################################################################
    # Other Resources Tab
    ################################################################################################
    
    # Create reactive elements for the subsetted datasets
    parks_data <- reactive({subset_for_neighborhoods(parks, input$neighborhoods_other)})
    libraries_data <- reactive({subset_for_neighborhoods(libraries, input$neighborhoods_other)})
    rec_centers_data <- reactive({subset_for_neighborhoods(rec_centers, input$neighborhoods_other)})
    #museums_data <- reactive({subset_for_neighborhoods(museums, input$neighborhoods_other)})
    playgrounds_data <- reactive({subset_for_neighborhoods(playgrounds, input$neighborhoods_other)})
    fields_data <- reactive({subset_for_neighborhoods(fields, input$neighborhoods_other)})
    pools_data <- reactive({subset_for_neighborhoods(pools, input$neighborhoods_other)})
    
    # Create the map
    other_mapdata <- reactiveValues(dat = 0)
    
    nbhd_labels_reactive_other <- reactive({get_nbhd_census_labels()})
    nbhd_labels_student_reactive_other <- reactive({get_nbhd_student_labels()})
    
    output$mymap_other = renderLeaflet({
        
        # Get the data
        parks_data1 <- parks_data()
        libraries_data1 <- libraries_data()
        rec_centers_data1 <- rec_centers_data()
        #museums_data1 <- museums_data()
        playgrounds_data1 <- playgrounds_data()
        fields_data1 <- fields_data()
        pools_data1 <- pools_data()
        
        ##### ACTUALLY DRAW THE OTHER RESOURCES MAP #####
        
        open_resource_map <- create_demographic_map("census_dems", input$demographics_other, input$student_demographics_other, 
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
                ifelse(parks_data1$has_nature, "YES", "NO"),
                ifelse(parks_data1$has_garden, "YES", "NO"),
                ifelse(parks_data1$has_biking, "YES", "NO")
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
              ifelse(rec_centers_data1$has_cardio, "YES", "NO"),
              ifelse(rec_centers_data1$has_weights, "YES", "NO"),
              ifelse(rec_centers_data1$has_gym, "YES", "NO"),
              ifelse(rec_centers_data1$has_arts_culture, "YES", "NO"),
              ifelse(rec_centers_data1$has_day_camps, "YES", "NO"),
              ifelse(rec_centers_data1$has_educ_programs, "YES", "NO"),
              ifelse(rec_centers_data1$has_fitness_health_programs, "YES", "NO"),
              ifelse(rec_centers_data1$has_senior_programs, "YES", "NO"),
              ifelse(rec_centers_data1$has_social_enrich_clubs, "YES", "NO"),
              ifelse(rec_centers_data1$has_special_events, "YES", "NO"),
              ifelse(rec_centers_data1$has_sports, "YES", "NO"),
              ifelse(rec_centers_data1$has_aquatics, "YES", "NO")
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
          
          # if(col == "Museums"){
          #   museums_popup <- sprintf(
          #     "<b>%s</b>",
          #     museums_data1$name
          #   ) %>% lapply(htmltools::HTML)
          #   
          #   open_resource_map <- open_resource_map %>% 
          #     add_resource_markers(museums_data1, museums_color, museums_popup)
          # }
          
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
          
          if(col == "Pools"){
            pools_popup <- sprintf(
              "<b>%s</b><br/>
              %s Pool <br/> %s",
              pools_data1$name,
              pools_data1$type,
              pools_data1$location
            ) %>% lapply(htmltools::HTML)
            
            open_resource_map <- open_resource_map %>% 
              add_resource_markers(pools_data1, museums_color, pools_popup)
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
                                       parks_data()[, c("name","class", "has_nature", "has_garden", "has_biking","sqft")],
                                       c("Park name", "Class", "Has nature", "Has garden", 
                                         "Has biking", "Sqft") #, "Nbhd name")
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
                                       libraries_data()[, c("name","patron_count","circulation_volume","sqft")],
                                       c("Library name", "Patron Count", 
                                         "Circulation Vol", "Sqft")) #, "Nbhd name"))
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
            dat <- data_table_function("Rec Centers", rec_centers_data()[, c("name","type","has_cardio","has_weights",
                                                                             "has_gym","has_arts_culture","has_day_camps",
                                                                             "has_educ_programs","has_fitness_health_programs",
                                                                             "has_senior_programs","has_social_enrich_clubs",
                                                                             "has_special_events","has_sports","has_aquatics")],
                                       c("Rec center name", "Type", "Has cardio", 
                                         "Has weights","Has gym", "Has arts culture",
                                         "Has day camps", "Has educ programs", 
                                         "Has fitness health programs", "Has senior programs",
                                         "Has social enrich clubs", "Has special events",
                                         "Has sports","Has aquatics")
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
        # else if(input$program_other[i] == "Museums"){
        #   output[[id]] <- DT::renderDataTable({
        #     dat <- data_table_function("Museums", museums_data()[, c(3,4,7)],
        #                                c("Museum name", "Address", "Nbhd name"))
        #     return(dat)    
        #   })
        #   
        #   output[[download_id]] <- downloadHandler(
        #     filename = "museums.csv",
        #     content = function(file) {
        #       # temporarily switch to the temp dir, in case you do not have write
        #       # permission to the current working directory
        #       owd <- setwd(tempdir())
        #       on.exit(setwd(owd))
        #       
        #       write.csv(museums_data(), file, row.names = FALSE)
        #     }
        #   )
        #   
        # }
        else if(input$program_other[i] == "Fields"){
          output[[id]] <- DT::renderDataTable({
            dat <- data_table_function("Fields", 
                                       fields_data()[, c("sport","location","tier","class","sqft")],
                                       c("Sport", "Location", "Tier", 
                                         "Class", "Sqft")) #, "Nbhd name"))
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
                                       playgrounds_data()[, c("location","year_rehab","class","sqft")],
                                       c("Location", "Year rehabilitated", "Class", 
                                         "Sqft")) #, "Nbhd name"))
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
        else if(input$program_other[i] == "Pools"){
          output[[id]] <-DT::renderDataTable({
            dat <- data_table_function("Pools", 
                                       pools_data()[, c("name","type","location")],
                                       c("Pool name", "Pool type", "Location")) #, "Nbhd name"))
            return(dat)    
          })
          
          output[[download_id]] <- downloadHandler(
            filename = "pools.csv",
            content = function(file) {
              # temporarily switch to the temp dir, in case you do not have write
              # permission to the current working directory
              owd <- setwd(tempdir())
              on.exit(setwd(owd))
              
              write.csv(pools_data(), file, row.names = FALSE)
            }
          )
          
        }
        
      })
    
    #######################################
    # Summary analysis for other resources
    #######################################
    
    output$summary_title_other <- renderUI({
      summary_nbhds <- input$neighborhoods_other
      if ("All neighborhoods" %in% summary_nbhds){
        summary_nbhds <- "All Neighborhoods"
      }
      else if (is.null(summary_nbhds)) {
        summary_nbhds <- "(No Neighborhoods Selected)"
      }
      
      sprintf('<h3>Summary for %s</h3>',
              toString(summary_nbhds)
      ) %>% lapply(htmltools::HTML)
    })
    
    # make the plot - number of each resource type
    output$other_resources_summary <- renderPlotly({
      
      num_parks <- nrow(parks_data())
      num_playgrounds <- nrow(playgrounds_data())
      num_rec_centers <- nrow(rec_centers_data())
      num_libraries <- nrow(libraries_data())
      #num_museums <- nrow(museums_data())
      num_pools <- nrow(pools_data())
      num_fields <- nrow(fields_data())
      
      plot_ly(x = c("Parks", "Playgrounds", "Rec Centers", "Libraries", "Pools", "Fields"),
              y = c(num_parks, num_playgrounds, num_rec_centers, num_libraries, num_pools, num_fields),
              type = "bar"
              ) %>%
        layout(xaxis = list(title = "Resource Type"),
               yaxis = list(title = "No. Resources"),
               title = "Number of Public Resources by Type"
               )
      
    })
    
    ########################################################
    # add demographics for the selected neighborhoods
    ########################################################
    output$nbhd_census_demog_summary_other <- renderUI({
      # subset to the selected neighborhoods
      summary_data <- subset_for_neighborhoods(shape_census@data, input$neighborhoods_other)
      
      # aggregate the demographics over all selected neighborhoods
      # dummy transformations for now
      summary_AGE_5_T <- sum(summary_data$AGE_5_T)
      summary_MED_HH_ <- summary_data$MED_HH_
      summary_PCT_LES <- sum(summary_data$PCT_LES * summary_data$POP_EDU) / sum(summary_data$POP_EDU)
      summary_PCT_COL <- sum(summary_data$PCT_COL * summary_data$POP_EDU) / sum(summary_data$POP_EDU)
      summary_PCT_NON <- sum(summary_data$PCT_NON * summary_data$POP_LAN) / sum(summary_data$POP_LAN)
      summary_PCT_HIS <- sum(summary_data$PCT_HIS * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      summary_PCT_WHI <- sum(summary_data$PCT_WHI * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      summary_PCT_BLA <- sum(summary_data$PCT_BLA * summary_data$POP_RAC) / sum(summary_data$POP_RAC)
      
      sprintf("<h4>Census Demographics</h4> 
              No. children 5-17 yrs old = %s <br>
              < HS Degree (%% Over 25) = %.1f%% <br>
              College Graduates (%% Over 25) = %.1f%% <br>
              %% Language Besides English Spoken = %.1f%% <br>
              %% Hispanic Population = %.1f%% <br>
              %% White population = %.1f%% <br>
              %% Black population = %.1f%%<br>",
              format(summary_AGE_5_T, big.mark = ","),
              summary_PCT_LES,
              summary_PCT_COL,
              summary_PCT_NON,
              summary_PCT_HIS,
              summary_PCT_WHI,
              summary_PCT_BLA #,
              #format(min(summary_MED_HH_), big.mark = ","), 
              #format(max(summary_MED_HH_), big.mark = ",")
      ) %>% lapply(htmltools::HTML)
    })
    
    # output$nbhd_student_demog_summary_other <- renderUI({
    #   # subset to the selected neighborhoods
    #   summary_data_student <- subset_for_neighborhoods(aggregate_dps_student_nbhds, input$neighborhoods_other)
    #   
    #   # aggregate the demographics over all selected neighborhoods
    #   total_nbhd_students <- sum(summary_data_student$total_students)
    #   
    #   if (total_nbhd_students < 10){
    #     summary_perc_el_students <- NA
    #     summary_perc_disable_students <- NA
    #     summary_perc_hispanic_students <- NA
    #     summary_perc_white_students <- NA
    #     summary_perc_black_students <- NA
    #   }
    #   else{
    #     summary_perc_el_students <- sum(summary_data_student$perc_el_students * summary_data_student$total_students, 
    #                                             na.rm = TRUE) / total_nbhd_students
    #     summary_perc_disable_students <- sum(summary_data_student$perc_disable_students * summary_data_student$total_students, 
    #                                          na.rm = TRUE) / total_nbhd_students
    #     summary_perc_hispanic_students <- sum(summary_data_student$perc_hispanic_students * summary_data_student$total_students, 
    #                                           na.rm = TRUE) / total_nbhd_students
    #     summary_perc_white_students <- sum(summary_data_student$perc_white_students * summary_data_student$total_students, 
    #                                        na.rm = TRUE) / total_nbhd_students
    #     summary_perc_black_students <- sum(summary_data_student$perc_black_students * summary_data_student$total_students, 
    #                                        na.rm = TRUE) / total_nbhd_students
    #   }
    #   
    #   # Print it!
    #   sprintf(
    #     "<h4>Student Demographics</h4>
    #     %% English student learners = %.1f%% <br>
    #     %% Students with disability = %.1f%% <br>
    #     %% Hispanic students = %.1f%% <br>
    #     %% White students = %.1f%% <br>
    #     %% Black students = %.1f%% <br><br>
    #     <i>Sample size = %s students</i>",
    #     summary_perc_el_students,
    #     summary_perc_disable_students,
    #     summary_perc_hispanic_students,
    #     summary_perc_white_students,
    #     summary_perc_black_students,
    #     format(total_nbhd_students, big.mark = ",")
    #   ) %>% lapply(htmltools::HTML)
    # })
    
    output$med_income_summary_other <- renderPlotly({
      summary_data <- subset_for_neighborhoods(shape_census@data, input$neighborhoods_other)
      
      if (nrow(summary_data)>0){
        plot_ly(data = summary_data, 
                x = ~MED_HH_,
                y = "",
                text = as.character(summary_data$nbhd_name),
                type = "scatter",
                marker = list(size = 15),
                alpha = 0.5,
                height = 100,
                hoverinfo = 'text'
        ) %>%
          layout( xaxis = list(title = "", range = c(0,NULL)),
                  title = "Median HH Income ($)",
                  titlefont = list(size = 12),
                  margin = list(l = 0)
          ) %>%
          config(displayModeBar = FALSE)
      }
      
    })
    
    
    ################################################################################################
    # Search Data Tab
    ################################################################################################
    # Subset the search data depending on the slider input and the zipcode selected in the sidebar panel
    # Getting column numbers depending on the type of the program selected. 
    # (used to subset the data in the next step)
    colm_search <- reactive({
      input$program_search
    })
    
    # Subsetting the data depending on the various selections made in the sidebar panel 
    subset_search_data = reactive({
      
      if(input$minprice_search != ""){
        mincost_search_data = google_analytics[which(google_analytics$mincost >= 
                                                       as.numeric(input$minprice_search)),]
      }
      else {
        mincost_search_data = google_analytics
      }
      
      if(input$maxprice_search != ""){
        maxcost_search_data = mincost_search_data[which(mincost_search_data$maxcost <= 
                                                          as.numeric(input$maxprice_search)),]
      }
      else {
        maxcost_search_data = mincost_search_data
      }
      
      if(input$minage_search != ""){
        minage_search_data = maxcost_search_data[which(maxcost_search_data$minage >= 
                                                         as.numeric(input$minage_search)),]
      }
      else{
        minage_search_data = maxcost_search_data
      }
      
      if(input$maxage_search != ""){
        maxage_search_data = minage_search_data[which(minage_search_data$maxage <= 
                                                        as.numeric(input$maxage_search)),]
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
                                          zipcode_search_data$sessiontimes == 
                                            input$sessiontimes_searchprog)
      }
      else {
        sessiontime_search_data <- zipcode_search_data
      }
      
      if(length(colm_search()) != 0){
        data_list = list()
        for(i in 1:length(colm_search())){
          data_list[[i]] = sessiontime_search_data[which(sessiontime_search_data$category == 
                                                           colm_search()[i]),]
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
  
    # Display the total % of searches made with this combination selected in the side bar panel
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
    

    ############## Rendering plots for visualization tab in the search data tab ###################
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
    
    # Zipcode searches graph
    output$search_zipcode_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What locations are people searching for? (Charts)", message=FALSE))
      
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
    
    # Zipcode sessions graph
    output$search_programs_zipcode_plot <- renderPlotly({
      validate(need(input$specific_search_questions=="What locations are people searching for? (Charts)", message=FALSE))
      
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
    
    search_mapdata <- reactiveValues(dat = 0)
    
    output$search_mymap <- renderLeaflet({
      validate(need(input$specific_search_questions=="What locations are people searching for? (Map)", message=FALSE))
      
      pal_search = colorBin("YlOrRd", domain = search_map_data@data$total_searches, bins = 5)
      labels_search = sprintf(
        "<strong>Zipcode</strong>: %s<br/><strong>Number of Searches</strong>: %s",
        search_map_data@data$GEOID10, search_map_data@data$total_searches) %>% lapply(htmltools::HTML)
      search_mapdata$dat <- leaflet()  %>% 
                              setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
                              addProviderTiles(providers$CartoDB.Positron) %>%
                              addPolygons(data = subset_denver_zipcodes, color = "#777",
                                          weight = 1,
                                          smoothFactor = 0.5,
                                          opacity = 1.0) %>%
                              addPolygons(data = search_map_data, color = "#444444", weight = 1, smoothFactor = 0.5,
                                          opacity = 1.0, fillOpacity = 0.5,
                                          fillColor = ~pal_search(total_searches),
                                          highlight = highlightOptions(
                                            bringToFront = FALSE,
                                            weight = 5,
                                            color = "#666"
                                          ),
                                          label = labels_search,
                                          labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
                              addLegend(pal = pal_search, 
                                        values = search_map_data$total_searches, 
                                        opacity = 0.7, 
                                        title = "Number of Searches",
                                        position = "bottomright")
      
    })
    
    ####### MAKE THE DOWNLOAD FEATURE FOR THE SEARCHES MAP #######
    output$search_map_down <- downloadHandler(
      filename = 'b4s_search_map.jpeg',
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        # set the zoom and pan based on current status of map
        access_mapdata$dat <-setView(search_mapdata$dat,
                                     lat = input$search_mymap_center$lat,
                                     lng = input$search_mymap_center$lng,
                                     zoom = input$search_mymap_zoom
        )
        
        mapshot(access_mapdata$dat, file = file, cliprect = "viewport")
      }
    )
    
    ################################################################################################
    # Access Index Tab
    ################################################################################################
    
    # first calculate the aggregated access index based on user input
    index <- reactive({calculate_aggregated_index(input$drive_or_transit,input$type_access,input$cost_access, input$disability)})
    index_nbhd <- reactive({calculate_aggregated_index(input$drive_or_transit,input$type_access,input$cost_access, input$disability, block=FALSE)})
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
      df <- subset_for_category(reschool_summer_program,
                                 unlist(program_list[input$type_access], use.names=F))
      if (input$disability==TRUE) {df <- subset_for_special_needs(df)}
      return(df)
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
    
    df_access <- reactive(data.frame("block_group"=shape_census_block@data$Id2, 
                                     "access_index"=round(index(),2), 
                                     "num_children_5_to_17"=shape_census_block@data$Ag_L_18-shape_census_block@data$Ag_Ls_5,
                                     "median_household_income"=shape_census_block@data$Mdn_HH_,
                                     "less_than_hs_percent"=round(100*shape_census_block$LESS_TH/shape_census_block@data$TTL_ppl,2),
                                     "hispanic_percent"=round(shape_census_block@data$PCT_Hsp,2),
                                     "white_percent"=round(shape_census_block@data$PCT_Wht,2),
                                     "black_percent"=round(shape_census_block@data$PCT_Afr,2)))
    
    df_access_nbhd <- reactive(data.frame("neighborhood"=shape_census@data$NBHD_NA, 
                                     "access_index"=round(index_nbhd(),2), 
                                     "num_children_5_to_17"=shape_census@data$AGE_5_T,
                                     "median_household_income"=shape_census@data$MED_HH_,
                                     "less_than_hs_percent"=round(shape_census$PCT_LES,2),
                                     "hispanic_percent"=round(shape_census@data$PCT_HIS,2),
                                     "white_percent"=round(shape_census@data$PCT_WHI,2),
                                     "black_percent"=round(shape_census@data$PCT_BLA,2)))
    
    # Output the relevant data in the data tab based on the selections
    output$datatable_access <- DT::renderDataTable({
      DT::datatable(df_access(), 
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
                      htmltools::h3("Access Index by Block Group")
                    ),
                    width = 300,
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
                    ) %>%
        formatStyle(colnames(df_access()),
                    backgroundColor = '#c6dbef'
        )
  })
    
    output$download_access_data <- downloadHandler(
      filename = "access_index.csv",
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        write.csv(df_access(), file, row.names = FALSE)
      }
    )
    
    # Output the relevant data in the data tab based on the selections
    output$datatable_access_nbhd <- DT::renderDataTable({
      DT::datatable(df_access_nbhd(), 
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
                      htmltools::h3("Access Index by Neighborhood")
                    ),
                    width = 300,
                    style = "bootstrap",
                    class = 'cell-border stripe',
                    rownames = FALSE
                    
                    ) %>%
        formatStyle(colnames(df_access_nbhd()),
                    backgroundColor = '#c6dbef'
        )
  })
    
    output$download_access_data_nbhd <- downloadHandler(
      filename = "access_index.csv",
      content = function(file) {
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        write.csv(df_access(), file, row.names = FALSE)
      }
    )
    
    output$lorenz <- renderPlot({
      tot_young_pop <- shape_census_block$Ag_L_18-shape_census_block$Ag_Ls_5
      plot(Lc(index()*tot_young_pop, tot_young_pop, na.rm=TRUE),
           col="darkred",lwd=2, 
           xlab = "Fraction of Students",
           ylab = "Cumulative Share of Access*"
           ) 
    })
    
    output$lorenz_text <- renderUI({
      tot_young_pop <- shape_census_block$Ag_L_18-shape_census_block$Ag_Ls_5
      gini_coeff <- Gini(index()*tot_young_pop, tot_young_pop)
      sprintf('<h4>Gini Coefficent: %.2f</h4><br>
              <h4>About the Lorenz Curve and Gini Coefficient</h4>
              <p> One way to investigate the degree of inequality in terms of access is to compare 
              the situation with perfect equality in the distribution of access (where each student 
              has the same access index) against the actual distribution. This is what the Lorenz curve 
              and Gini coefficient do. Points on the <strong>Lorenz curve</strong> correspond to
              statements like "the bottom x%% of students have y%% of the total access in the city."
              If there was an even distribution of all access scores, then the points would fall on the 
              diagonal, implying that each student had equal access to opportunities. The farther the 
              red Lorenz curve is from the black diagonal, the more unequally distributed access to
              out of school resources is.</p>
              <p>The <strong>Gini coefficient</strong> 
              quantifies how far the Lorenz curve is from the diagonal, by calculating the ratio between 
              the area between the Lorenz curve and the diagonal, and the area in the triangle 
              below the diagonal. Thus, a high Gini coefficient indicates a large discrepancy between the 
              curve and the diagonal, and high inequality in the distribution. Note that the Gini coefficient 
              is always between 0 and 1.</p>
              ', 
              gini_coeff) %>% lapply(htmltools::HTML)
      
    })
    
    # output$access_scatter <- renderPlotly({
    #   plot_ly(x = shape_census@data$AGE_5_T, y = index_nbhd(), text=shape_census@data$NBHD_NA, 
    #           type='scatter', mode='markers') %>%
    #     layout(title = "Neighborhoods with low access and high student age population",
    #            xaxis = list(title="Number of students in each neighborhood"), #showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
    #            yaxis = list(title = "Access Index score") #showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
    #            )
    # })
    
    race_access_means <- reactive({get_race_access_means(index())})

    output$access_demog <- renderPlotly({
      access_race_list <- race_access_means()
      access_race_names <- names(access_race_list)
      access_race_vals <- unlist(access_race_list)
      plot_ly(x=access_race_names, 
              y=access_race_vals, 
              type="bar") %>%
        layout(title = "Average Access Index by Race / Ethnicity",
               xaxis = list(title = "Race / Ethnicity"),
               yaxis = list(title = "Average Access Index")
               )
    })
    
  })  

