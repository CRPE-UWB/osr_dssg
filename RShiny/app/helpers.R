#############################
# Simple helpers 
#############################
wrap_text <- function(s, offset) {
  gsub('(.{1,50})(\\s|$)', '\\1<br/>',s)
}

make_program_popups <- function(program_data) {
  sprintf(
    "<b>%s</b><br/> 
    %s <br/> 
    <i>%s</i><br/>
    $%i per session<br/>
    Starts: %s, Ends: %s <br/>  
    Special needs = %s,  
    Scholarships = %s <br/>",
    wrap_text(paste("Program: ",program_data$session_name)), 
    wrap_text(paste("Organization: ",program_data$camp_name)), 
    wrap_text(paste("Description: ",program_data$session_short_description)),
    program_data$session_cost,
    program_data$session_date_start, 
    program_data$session_date_end,
    program_data$has_special_needs_offerings, 
    program_data$has_scholarships
  ) %>% lapply(htmltools::HTML)
}

calculate_aggregated_index <- function(transport_mode, types, cost) {
  if (transport_mode=="drive") {
    df <- driving_index
  } else {
    df <- transit_index
  }
  # look for the intersection of indices containing the words in the string-vector "types"
  # and the string "cost"
  shared_indices <- stack(sapply(FUN=grep,X=c(types,cost),x=colnames(df)))$values
  shared_indices <- shared_indices[duplicated(shared_indices)]

  if (length(shared_indices)>1) {
    val <- rowMeans(df[,shared_indices])
  } else {
    val <- df[,shared_indices]
  }
  return(val)
}

#############################
# Mapping helpers 
#############################

# Function to make a base map
make_base_map <- function() {
  leaflet()  %>% 
    setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
    addProviderTiles(providers$CartoDB.Positron)
}

add_blank_map <- function(map) {
  addPolygons(map, data = shape_census,
              color = "#777",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0,
              fillColor = "#999",
              fillOpacity = 0.3,
              label = nbhd_labels,
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
              )
  )
}

# Function to add demographic info to a map
add_colored_polygon_map <- function(map, spdf, pal_type, label_type, 
                                    column_name=NULL, legend_titles=NULL, legend_title=NULL, 
                                    vals=NULL, labFormat = labelFormat(), my_weight=1){
  if (is.null(vals)) {vals <- spdf@data[,column_name]}
  if (is.null(legend_title)) {legend_title <- legend_titles[column_name]}
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
              )
  ) %>% 
    addLegend(pal = pal_type,
              values = vals,
              opacity = 0.45,
              title = as.character(legend_title),
              labFormat = labFormat,
              position = "bottomright"
    )
}

# Function to add circle markers to the map
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

# Function to draw an outline of a SINGLE neighborhood:
add_neighborhood_outline <- function(map, neighborhood_name) {
  addPolygons(map, data = subset(shape_census, NBHD_NA==neighborhood_name),
              fill = FALSE, weight=5, color = "#777", opacity = 1)
}

# Function to draw the base OTHER RESOURCES map + demographics
make_demographic_map <- function(pal, col_name, labFormat) {
  if (is.null(col_name)) {
    make_base_map() %>% add_blank_map()
  }
  else{
    make_base_map() %>%
      add_colored_polygon_map(shape_census, pal, nbhd_labels, col_name, 
                              legend_titles_demographic, labFormat = labFormat)
  }
}

####### SUBSETTING FUNCTIONS  #######

# Function to subset all the resource datasets based on the neighborhood selected
subset_for_neighborhoods <- function(df, neighborhoods_list){
    if( ! ("All neighborhoods" %in% neighborhoods_list| is.null(neighborhoods_list)) ) {
      a <- df[which(df[, "nbhd_name"] %in% neighborhoods_list),]
    }
    else {
      a <- df
    }
    return(a) 
}

# Subsetting the data for cost
subset_for_cost <- function(df, min_cost, max_cost) {
  return(df[df$session_cost >= min_cost & 
              df$session_cost  <= max_cost,])
}

# Subsetting the data for the type of the program selected
subset_for_category <- function(df, col) {
  return(df[apply(as.data.frame(df[,col])==1,1,any),])
}