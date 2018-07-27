#############################
# Simple helpers 
#############################
wrap_text <- function(s, offset) {
  gsub('(.{1,50})(\\s|$)', '\\1<br/>',s)
}

#############################
# Mapping helpers 
#############################

# Function to make a base map
make_base_map <- function() {
  leaflet()  %>% 
    setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
    #addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
}

add_blank_map <- function(map) {
  addPolygons(map, data = shape_census,
              color = "black",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0,
              fillColor = "#999",
              fillOpacity = 0.5,
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
                direction = "right",
                offset = c(35,0)
              )
  ) %>% 
    addLegend(pal = pal_type,
              values = shape_census@data[,column_name],
              opacity = 0.7,
              title = as.character(legendTitles[column_name]),
              position = "bottomright"
    )
}

# Function to add circle markers to the map
add_circle_markers <- function(map, data, legend_title, color_code, popup_text){
  addCircleMarkers(map, 
                   lng = jitter(data$long, factor = 1, amount = 0.0005), 
                   lat = jitter(data$lat, factor = 1, amount = 0.0005), 
                   radius = 4,
                   stroke = FALSE,
                   weight = 1,
                   fillColor = color_code,
                   fillOpacity = 0.5,
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
      opacity = 0.5,
      labels = legend_title
    )
}

# Function to draw the base map + demographics + program markers
make_reschool_map <- function(df, popup_text, palette, col_name) {
  make_base_map() %>%
    add_demographic_map(palette,col_name,nbhd_labels) %>%
    add_program_markers(df, popup_text)
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
  a <- df[apply(as.data.frame(df[,col])==1,1,any), c(1:12,col)]
  return(a)
}

#subset_for_cost <- function(df, )

####### STUFF TO CREATE THE BASIC MAPS W/ DEMOGRAPHICS  #######

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