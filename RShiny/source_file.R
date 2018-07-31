# Source file to generate data for plotting in RShiny app.
# Note: all file paths are relative to LOCATION OF THIS FILE

library(dplyr)
library(rgdal)
library(tigris)
library(rgeos)
library(RPostgreSQL)
library(RColorBrewer)
library(leaflet)

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

other_resources_colors <- brewer.pal(6, "Blues")
parks_color <- mygreen
libraries_color <- myblue
rec_centers_color <- myblue3
playgrounds_color <- mypurple
museums_color <- mypurple3
fields_color <- mygreen3

################## Getting data from the database e#############################################

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# load credentials for the connection: dbname, host, port, user, password
# looks for cred.txt in parent dir to cloned github repo
source('../../cred.txt')

# create a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = dbname,
                 host = host, port = port,
                 user = user, password = password)

# get the required tables from the sql database 
reschool_summer_program = dbGetQuery(con, "SELECT * from shiny.summer_programs")
aggregate_session_nbhds = dbGetQuery(con, "SELECT * from shiny.aggregate_programs_nbhd")
aggregate_dps_student_nbhds = dbGetQuery(con, "SELECT * from shiny.dps_student_aggregate_nbhd")
fields = dbGetQuery(con, "SELECT * from shiny.fields")
museums = dbGetQuery(con, "SELECT * from shiny.museums")
libraries = dbGetQuery(con, "SELECT * from shiny.libraries")
playgrounds = dbGetQuery(con, "SELECT * from shiny.playgrounds")
rec_centers = dbGetQuery(con, "SELECT * from shiny.rec_centers")
parks = dbGetQuery(con, "SELECT * from shiny.parks")
all_neighbourhoods = dbGetQuery(con, "SELECT * from clean.blockgroup_nbhds")
google_analytics = dbGetQuery(con, "SELECT * from clean.google_analytics")

nbhd_program_summary <- dbGetQuery(con, "SELECT * from shiny.nbhd_program_summary")

driving_index = dbGetQuery(con, "SELECT * from clean.driving_index")
transit_index = dbGetQuery(con, "SELECT * from clean.transit_index")

# when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)

####################### Getting the shape files to plot the bock groups on the map ##############################

########################
# Block group (access index) stuff
########################
shape_census_block <- readOGR(dsn = "../data/census_block_groups", layer = "shape_census")
access_driving <- geo_join(shape_census_block,driving_index, "Id2", "Id2", how="inner")
access_transit <- geo_join(shape_census_block,transit_index, "Id2", "Id2", how="inner")

########################
# Neighborhood stuff
########################
shape_census <- readOGR(dsn = file.path("..", "data", "nbhd_dem_shapes"), layer = "nbhd_dem_shapes")

# Joining the 'number of sessions' information with the census shape file
shape_census <- geo_join(shape_census, aggregate_session_nbhds, 
                         "NBHD_NA", "nbhd_name", how = "left")

# Joining the aggregate dps students information to the census shape file
shape_census <- geo_join(shape_census, aggregate_dps_student_nbhds, 
                         "NBHD_NA", "nbhd_name", how = "left")

###################### Creating filter variables for the sidebar panels #########################

# Creating filter variables: distinct zipcode, minimum cost, maximum cost, program type
# (for the ReSchool tab sidebar panel)
neighborhoods_reshoolprograms = unique(reschool_summer_program$nbhd_name)
minprice_reschoolprograms = min(reschool_summer_program$session_cost)
maxprice_reschoolprograms = max(reschool_summer_program$session_cost)

#Defining the variables to be used in the program search tab sidebar panel
#Convert the cost column  to numeric
google_analytics$mincost = as.numeric(google_analytics$mincost)
google_analytics$maxcost = as.numeric(google_analytics$maxcost)

#Get the min and max cost to be used in the input slider
minprice_search = min(google_analytics$mincost, na.rm = TRUE)
maxprice_search = max(google_analytics$maxcost, na.rm = TRUE)

#Creating unique zipcodes for the third tab search data
#We take only the locations which have zipcodes which make sense
google_analytics[-grep("80\\d{3}",google_analytics$location),"location"] <- NA 
google_analytics[grep("80\\d{3}",google_analytics$location),"location"] <- 
  gsub(".*(80\\d{3}).*","\\1",google_analytics[grep("80\\d{3}",google_analytics$location),"location"])
zipcode_searchdata = unique(google_analytics$location)

#Replacing empty category values with 'None'
google_analytics$category[google_analytics$category == ''] <- NA

# Creating variables for the second tab 'other out-of-school resources'
neighborhoods_other = unique(all_neighbourhoods$nbhd_name)

# Defining variables for choosing demographic information
demographic_filters = c("Median Income", "Percent below poverty level")

############################## Racial distributions variables ####################################

# Creating majority race variables for each neighborhood
# (could probably do this ahead of time)
shape_census@data$majority_race <- max.col(as.matrix(
          shape_census@data[ ,c("PCT_HIS", "PCT_WHI", "PCT_BLA","PCT_NAT","PCT_ASI")]
                     ))
shape_census@data$majority_race <- gsub(1, "Hispanic", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(2, "White", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(3, "Black", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(4, "Native", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(5, "Asian", shape_census@data$majority_race)

# Adding a custom html label for the racial distributions
shape_census@data$racial_dist_html <- mapply(
  
  # Inputs: 
  # neighborhood name (string),
  # percents of different races (numerics),
  # color palette generated from brewer.pal()
  function(nbhd, pct_hisp, pct_white, pct_black, pct_native, pct_asian){
    
    pal <- brewer.pal(4, "Set2")  # color palette - match to the server.R code
    color1 <- pal[1]
    color2 <- pal[2]
    color3 <- pal[3]
    color4 <- pal[4]
    
    sprintf(
      "<div style='font-size:12px;width:180px;float:left'>
            <span style='font-size:16 px;font-weight:bold'>%s</span><br/>
            <div style='width:100%%'>
              <span style='background:%s;width:%s%%;position:absolute;left:0'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <br/>
              <span style='color:%s;float:left'>%.2f%% Black</span><br/>
              <span style='color:%s;float:left'>%.2f%% Hispanic</span><br/>
              <span style='color:%s;float:left'>%.2f%% White</span><br/>
              <span style='color:%s;float:left'>%.2f%% Other</span><br clear='all'/>
            </div>
        </div>",
      nbhd,
      color1, pct_black, 
      color2, pct_hisp, pct_black,
      color3, pct_white, pct_hisp + pct_black,
      color4, 100 - (pct_white + pct_hisp + pct_black), pct_white + pct_hisp + pct_black,
      color1, pct_black, 
      color2, pct_hisp, 
      color3, pct_white, 
      color4, 100 - (pct_white + pct_hisp + pct_black)
    ) %>% lapply(htmltools::HTML)
  },
  
  shape_census@data$NBHD_NA,
  shape_census@data$PCT_HIS,
  shape_census@data$PCT_WHI,
  shape_census@data$PCT_BLA,
  shape_census@data$PCT_NAT,
  shape_census@data$PCT_ASI
  
)

####### STUFF TO CREATE THE BASIC MAPS W/ DEMOGRAPHICS  #######

# Legend titles for demographic maps
legend_titles_demographic <- list(MED_HH_ = "Median HH Income ($)",
                                  PCT_LES = "Less Than <br> HS Degree (%)",
                                  PCT_COL = "College <br> Graduates (%)",
                                  PCT_HIS = "% Hispanic",
                                  PCT_BLA = "% Black",
                                  PCT_WHI = "% White",
                                  PCT_NON = "Lang. Besides <br>English (%)",
                                  majority_race = "Most Common<br>Race/Ethnicity"
)

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
bins_income <- c(0, 25000, 50000, 75000, 100000, Inf)
pal_income <- colorBin("Greys", domain = shape_census@data$MED_HH_, bins = bins_income)
pal_edu <- colorBin("Greys", domain = shape_census@data$PCT_LES, bins = 5)
pal_edu2 <- colorBin("Greys", domain = shape_census@data$PCT_COL, bins = 5)
# bins_language <- c(0, 15, 30, 45, 60, 75)
pal_language <- colorBin("Greys", domain = shape_census@data$PCT_NON, bins = 5)

# colorful ones for racial demographics
pal_hispanic <- colorBin("Greens", domain = shape_census@data$PCT_HIS, bins = 5)
pal_black <- colorBin("Blues", domain = shape_census@data$PCT_BLA, bins = 5)
pal_white <- colorBin("Purples", domain = shape_census@data$PCT_WHI, bins = 5)

pal_all_races <- colorFactor(c(myblue, mygreen, mypurple), 
                             domain = shape_census@data$majority_race)
