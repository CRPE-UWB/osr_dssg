#############################
# Color settings
#############################
myyellow <- "#FFFF66"
mygreen <- brewer.pal(3, "Greens")[2]
myblue <- brewer.pal(3, "Blues")[2]
mypurple <- brewer.pal(3, "Purples")[2]

mygreen2 <- brewer.pal(3, "Greens")[1]
myblue2 <- brewer.pal(3, "Blues")[1]
mypurple2 <- brewer.pal(3, "Purples")[1]

mygreen3 <- brewer.pal(3, "Greens")[3]
myblue3 <- brewer.pal(3, "Blues")[3]
mypurple3 <- brewer.pal(3, "Purples")[3]

# colors for the other resources
parks_color <- "#fb9a99"
libraries_color <- "#e31a1c"
rec_centers_color <- "#fdbf6f"
playgrounds_color <- "#ff7f00"
museums_color <- "#ffff99"
fields_color <- "#b15928"

#############################
# Simple helpers 
#############################
wrap_text <- function(s, offset) {
  gsub('(.{1,50})(\\s|$)', '\\1<br/>',s)
}

calculate_aggregated_index <- function(df, types, cost) {
  
  val <- mean(df[grep(),])
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
              color = "black",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0,
              fillColor = "#999",
              fillOpacity = 0.7,
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
add_colored_polygon_map <- function(map, spdf, legend_titles, pal_type, label_type, 
                                    column_name=NULL, vals=NULL){
  if (is.null(vals)) {vals <- spdf@data[,column_name]}
  addPolygons(map, data = spdf,
              fillColor = ~pal_type(vals),
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
                direction = "right",
                offset = c(35,0)
              )
  ) %>% 
    addLegend(pal = pal_type,
              values = vals,
              opacity = 0.7,
              title = as.character(legend_titles[column_name]),
              position = "bottomright"
    )
}

# Function to add circle markers to the map
add_circle_markers <- function(map, data, legend_title, color_code, popup_text, opacity = 0.5){
  addCircleMarkers(map, 
                   lng = jitter(data$long, factor = 1, amount = 0.0005), 
                   lat = jitter(data$lat, factor = 1, amount = 0.0005), 
                   radius = 4,
                   stroke = TRUE,
                   weight = 0.5,
                   color = 'gray',
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

# Function to draw the base map + demographics + program markers
make_reschool_map <- function(df, popup_text, palette, col_name = NULL) {
  if (is.null(col_name)) {
    make_base_map() %>%
      add_blank_map() %>%
      add_circle_markers(df, "program", myyellow, popup_text)
  }
  else{
    make_base_map() %>%
      add_demographic_map(palette,col_name,nbhd_labels) %>%
      add_circle_markers(df, "program", myyellow, popup_text)
  }
}

####### SUBSETTING FUNCTIONS  #######

# Function to subset all the resource datasets based on the neighborhood selected
subset_for_neighborhoods <- function(df, neighborhoods_list){
    if(neighborhoods_list != "No neighborhood selected" ) {
      a <- df[which(df[, "nbhd_name"] == neighborhoods_list),]
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