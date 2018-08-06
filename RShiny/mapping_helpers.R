#
# Helper functions for MAPPING in the Shiny App
# (see other_helpers.R for non-mapping functions)
#

############################################################################################
# Making Basic Maps
############################################################################################

# Function to make a base map
make_base_map <- function() {
  leaflet()  %>% 
    setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
    addProviderTiles(providers$CartoDB.Positron)
}

# Function to add a "blank" map, with no demographic information but still showing the nbhds
add_blank_map <- function(map, spdf=shape_census, id_col_name=NULL, my_labels=nbhd_labels) {
  
  layer_id=NULL
  
  if (!is.null(id_col_name)) {layer_id <- as.character(spdf@data[,id_col_name])}
  addPolygons(map, data = spdf,
              color = "#777",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0,
              fillColor = "#999",
              fillOpacity = 0.3,
              label = my_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", 
                             padding = "3px 8px"),
                textsize = "12px",
                direction = "right",
                offset = c(35,0)
              ),
              highlight = highlightOptions(
                bringToFront = FALSE,
                weight = 5,
                color = "#666"
              ),
              layerId = layer_id
  )
}

# Function to add colored polygons to the map - used for visualizing demographics
add_colored_polygon_map <- function(map, spdf, pal_type, label_type, 
                                    column_name=NULL, legend_titles=NULL, legend_title=NULL, 
                                    vals=NULL, labFormat = labelFormat(), my_weight=1, id_col_name=NULL){
  if (is.null(vals)) {
    vals <- spdf@data[,column_name]
    }
  if (is.null(legend_title)) {
    legend_title <- legend_titles[column_name]
    }
  layer_id=NULL
  if (!is.null(id_col_name)) {
    layer_id <- as.character(spdf@data[,id_col_name])
  }
  
  # actually draw the polygons
  addPolygons(map, data = spdf,
              fillColor = ~pal_type(vals),
              weight = my_weight,
              opacity = 1,
              color = "#777",
              dashArray = "",
              fillOpacity = 0.4,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                bringToFront = FALSE
              ),
              label = label_type,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "right",
                offset = c(35,0)
              ),
              layerId = layer_id
  ) %>% 
    addLegend(pal = pal_type,
              values = vals,
              opacity = 0.45,
              title = as.character(legend_title),
              labFormat = labFormat,
              position = "bottomright"
    )
}

# Function to draw the WHOLE DEMOGRAPHICS MAP - no circle markers, though
make_demographic_map <- function(pal, col_name, labFormat=NULL, my_labels=nbhd_labels) {
  if (is.null(col_name)) {
    make_base_map() %>% add_blank_map(id_col_name="NBHD_NA", my_labels = my_labels)
  }
  else{
    make_base_map() %>%
      add_colored_polygon_map(shape_census, pal, my_labels, col_name, 
                              legend_titles_demographic, labFormat = labFormat, id_col_name = "NBHD_NA")
  }
}

create_demographic_map <- function(school_or_census, demographics, student_demographics, census_labels, student_labels) {
  if(school_or_census=="census_dems") {
    if(demographics=="none"){
      return(make_demographic_map(NULL, NULL, my_labels = census_labels))
    }
    else if(demographics == "majority_race") {
      labels_race_breakdown <- shape_census@data$racial_dist_html
      return(make_base_map() %>%
               add_colored_polygon_map(shape_census, pal_list[[demographics]], ~labels_race_breakdown,
                                       demographics, legend_titles_demographic)
      )
    }
    else {
      return(make_demographic_map(pal_list[[demographics]], demographics, labFormat = lab_format_list[[demographics]],
                                  my_labels=census_labels))
    }
  }
  
  else if(school_or_census=="student_dems") {
    if(student_demographics == "none"){
      curr_map <- make_demographic_map(NULL,NULL, my_labels=student_labels)
    } else {
      curr_map <- make_demographic_map(pal_black, student_demographics, labFormat = labelFormat(suffix = " %"), my_labels=student_labels)
    }
  }
}

# Function to add circle markers to the map, with a legend
add_circle_markers <- function(map, data, legend_title, color_code, popup_text, opacity = 0.5, weight = 1.0){
  if (nrow(data)>0){
    addCircleMarkers(map, 
                     lng = jitter(data$long, factor = 1, amount = 0.0005), 
                     lat = jitter(data$lat, factor = 1, amount = 0.0005), 
                     radius = 4,
                     stroke = TRUE,
                     weight = weight,
                     color = "black",
                     fillColor = color_code,
                     fillOpacity = opacity,
                     label = popup_text,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "12px",
                       direction = "right",
                       offset = c(5,0)
                     )
    ) %>%
      addLegend(
        position = "bottomright",
        colors = c(color_code),
        opacity = opacity,
        labels = legend_title
      )
  }
  else {
    return(map)
  }
}

############################################################################################
# Outlining neighborhoods on the map
############################################################################################

# Function to draw an outline of a group of neighborhoods:
add_neighborhoods_outline <- function(map, nbhd_list) {
  if ( !is.null(nbhd_list) ) {
    if ("All neighborhoods" %in% nbhd_list) {
      return(map %>% add_outline())
    } else {
      return(map %>% add_outline(nbhd_list))
    }
  } else {
    return(map)
  }
}

# Helper function for add_neighborhoods_outline
add_outline <- function(map, nbhd_list=NULL) {
  if (is.null(nbhd_list)) {
    relevant_nbhds <- shape_census
  }
  else {
    relevant_nbhds <- subset(shape_census, NBHD_NA %in% nbhd_list)
  }
  addPolygons(map, data = unionSpatialPolygons(relevant_nbhds, IDs=rep(0,nrow(relevant_nbhds))),
              fill = FALSE, weight=5, color = "#777", opacity = 1)
}