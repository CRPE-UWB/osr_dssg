# Source file to generate data for plotting in RShiny app.
# Note: all file paths are relative to LOCATION OF THIS FILE

library(RPostgreSQL)
library(dplyr)
library(rgdal)
library(rgeos)
library(tigris)

################## Getting data from the database e#############################################

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# load credentials for the connection: dbname, host, port, user, password
# looks for cred.txt in parent dir to cloned github repo
source('cred.txt')

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

##################### Getting shape files to plot block groups, nbhds on the map ##########################

# Get block group shape file (for access index stuff)
shape_census_block <- readOGR(dsn = "../data/census_block_groups", layer = "shape_census")
shape_census_block@data$Id2 <- as.numeric(as.character(shape_census_block@data$Id2))
shape_census_block <- shape_census_block[order(shape_census_block@data$Id2),]

# Get neighborhood shape file (for everything else)
shape_census <- readOGR(dsn = file.path("..", "data", "nbhd_dem_shapes"), layer = "nbhd_dem_shapes")

# Join the 'number of sessions' information with the census shape file
shape_census <- geo_join(shape_census, aggregate_session_nbhds, 
                         "NBHD_NA", "nbhd_name", how = "left")

# Join the aggregate dps students information to the census shape file
shape_census <- geo_join(shape_census, aggregate_dps_student_nbhds, 
                         "NBHD_NA", "nbhd_name", how = "left")

######################## Creating filter variables for the sidebar panels ############################

# Filter variables for 'B4S programs' tab
neighborhoods_list <- sort(unique(as.character(shape_census$NBHD_NA)))
#neighborhoods_reshoolprograms = unique(reschool_summer_program$nbhd_name)
minprice_reschoolprograms = min(reschool_summer_program$session_cost)
maxprice_reschoolprograms = max(reschool_summer_program$session_cost)

# Filter variables for 'other resources' tab
neighborhoods_other = unique(all_neighbourhoods$nbhd_name)

demographic_filters = c("Median Income", "Percent below poverty level")

######## Filter variables for the B4S Search Data tab (Google Analytics) ########

# Convert the necessary columns to numeric
google_analytics$mincost = as.numeric(google_analytics$mincost)
google_analytics$maxcost = as.numeric(google_analytics$maxcost)
google_analytics$minage = as.numeric(google_analytics$minage)
google_analytics$maxage = as.numeric(google_analytics$maxage)
google_analytics$distance = as.numeric(google_analytics$distance)

# Create unique zipcodes (ignore locations that aren't sensible zipcodes)
google_analytics[-grep("80\\d{3}",google_analytics$location),"location"] <- NA 
google_analytics[grep("80\\d{3}",google_analytics$location),"location"] <- 
                gsub(".*(80\\d{3}).*","\\1", 
                     google_analytics[grep("80\\d{3}",google_analytics$location),"location"]
                     )
zipcode_searchdata = unique(google_analytics$location)

# Replacing empty category values with NAs
google_analytics$category[google_analytics$category == ''] <- NA

# Creating necessary summary tables to be used in the visualization tab in the search data tab
search_sort_summary = google_analytics %>% 
                              select(sort, users) %>% 
                              filter(sort != '') %>% 
                              group_by(sort) %>% 
                              summarize(total_searches = sum(users))

search_distance_summary = google_analytics %>% 
                              select(distance, users) %>% 
                              filter(distance != '') %>% 
                              group_by(distance) %>% 
                              summarize(total_searches = sum(users)) %>% 
                              arrange(distance) %>% 
                              filter(distance <= 100)

############################ Creating racial distributions variables ##################################

# Creating majority (really most common) race variables for each neighborhood
shape_census@data$majority_race <- max.col(as.matrix(
          shape_census@data[ ,c("PCT_HIS", "PCT_WHI", "PCT_BLA","PCT_NAT","PCT_ASI")]
                     ))
shape_census@data$majority_race <- gsub(1, "Hispanic", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(2, "White", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(3, "Black", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(4, "Native", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(5, "Asian", shape_census@data$majority_race)
