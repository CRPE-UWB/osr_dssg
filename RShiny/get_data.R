# Source file to generate data for plotting in RShiny app.
# Note: all file paths are relative to LOCATION OF THIS FILE

library(RPostgreSQL)
library(dplyr)
library(rgdal)
library(rgeos)
library(tigris)

# specify where the shiny tables live
shiny_data_folder <- file.path('..', 'data', 'shiny_tables')

################## Getting data from the database e#############################################

####### WILL TAKE OUT ONCE MOVED TO GITHUB ##########

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

# ReSchool Program data
aggregate_session_nbhds = dbGetQuery(con, "SELECT * from shiny.aggregate_programs_nbhd")

# when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)

###############################################################
# Load Access Index data

driving_index <- read.csv( file.path(shiny_data_folder, "access_indices", "driving_index.csv") )
driving_index_disability <- read.csv( file.path(shiny_data_folder, "access_indices", "driving_index_disability.csv") )
transit_index <- read.csv( file.path(shiny_data_folder, "access_indices", "transit_index.csv") )
transit_index_disability <- read.csv( file.path(shiny_data_folder, "access_indices", "transit_index_disability.csv") )
driving_index_nbhd <- read.csv( file.path(shiny_data_folder, "access_indices", "driving_index_nbhd.csv") )
driving_index_disability_nbhd <- read.csv( file.path(shiny_data_folder, "access_indices", "driving_index_disability_nbhd.csv") )
transit_index_nbhd <- read.csv( file.path(shiny_data_folder, "access_indices", "transit_index_nbhd.csv") )
transit_index_nbhd_disability_nbhd <- read.csv( file.path(shiny_data_folder, "access_indices", "transit_index_disability_nbhd.csv") )

###############################################################
# Load B4S Program data

reschool_summer_program <- read.csv( file.path(shiny_data_folder, 'b4s_programs.csv'), stringsAsFactors = FALSE )

# drop columns without block groups
reschool_summer_program <- reschool_summer_program[!is.na(reschool_summer_program$bgroup_id2), ]

# make aggregate data by neighborhood as well
cols_to_sum <- c("has_academic", "has_arts", "has_cooking", "has_dance",
                 "has_drama", "has_music", "has_nature", "has_scholarships", 
                 "has_special_needs_offerings", "has_sports", "has_stem")
nbhd_program_summary <- aggregate(reschool_summer_program[,cols_to_sum],
                                  by = list(nbhd_id=reschool_summer_program$nbhd_id,
                                            nbhd_name=reschool_summer_program$nbhd_name),
                                  FUN = sum
                                  )
colnames(nbhd_program_summary) <- c("nbhd_id", "nbhd_name",
                                    "total_academic", "total_arts", "total_cooking", "total_dance",
                                    "total_drama", "total_music", "total_nature",
                                    "total_scholarships", "total_special_needs",
                                    "total_sports", "total_stem")

###############################################################
# Load Search Data and zip codes to plot it

google_analytics = read.csv( file.path(shiny_data_folder, "google_analytics.csv") )
relevant_zip_codes = readOGR(dsn =  file.path("..", "data", "zip_codes") )
total_denver_zipcodes = read.csv( file.path("..", "data", "denver_zip_codes.csv") )

###############################################################
# Load Other Resources data (from Denver Open Data)

fields = read.csv( file.path(shiny_data_folder, 'fields.csv') )
museums = read.csv( file.path(shiny_data_folder, 'museums.csv') )
libraries = read.csv( file.path(shiny_data_folder, 'libraries.csv') )
playgrounds = read.csv( file.path(shiny_data_folder, 'playgrounds.csv') )
rec_centers = read.csv( file.path(shiny_data_folder, 'rec_centers.csv') )
parks = read.csv( file.path(shiny_data_folder, 'parks.csv') )

###############################################################
# Load aggregated DPS student data for demographics

aggregate_dps_student_nbhds = read.csv( file.path(shiny_data_folder, 
                                                  "aggregate_dps_student_nbhds.csv"), 
                                        check.names=FALSE )

###############################################################
# Load shape files to plot block groups, nbhds on the map 

# Get block group shape file (for access index stuff)
shape_census_block <- readOGR(dsn = file.path("..", "data", "census_block_groups"), layer = "shape_census")
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
shape_census <- shape_census[order(as.character(shape_census@data$NBHD_NA)),]

###############################################################
# Creating filter variables for the sidebar panels

# Filter variables for 'B4S programs' tab
neighborhoods_list <- sort(unique(as.character(shape_census$NBHD_NA)))
#neighborhoods_reshoolprograms = unique(reschool_summer_program$nbhd_name)
minprice_reschoolprograms = min(reschool_summer_program$session_cost)
maxprice_reschoolprograms = max(reschool_summer_program$session_cost)

# Filter variables for 'other resources' tab
neighborhoods_other = shape_census@data$NBHD_NA #unique(all_neighbourhoods$nbhd_name)

demographic_filters = c("Median Income", "Percent below poverty level")

# Filter variables for the B4S Search Data tab (Google Analytics)

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

# distance searched stuff
search_distance_summary = google_analytics %>% 
  select(distance, users) %>% 
  filter(distance != '') %>% 
  group_by(distance) %>% 
  summarize(total_searches = sum(users)) %>% 
  arrange(total_searches, distance) %>% 
  filter(distance <= 100) %>% 
  filter(distance != 20) %>% 
  filter(total_searches > 20) %>% arrange(distance)

search_distance_summary$distance = as.character(search_distance_summary$distance)

#Creating the number of searches by program category
search_programtype_summary = google_analytics %>% select(category, users) %>% 
  filter(category != '') %>% group_by(category) %>% 
  summarize(total_searches = sum(users)) 

#Now we will be comparing the percentage of searches by category to the percentage of programs existing by category
#Getting the percentage of programs by category from reschool search data 
number_of_sessions = numeric()

program_col_names <- c("has_special_needs_offerings", "has_scholarships", "has_academic", 
                       "has_arts", "has_cooking", "has_dance", "has_drama", "has_music", 
                       "has_nature", "has_sports", "has_stem")

j =1
for(program_col_name in program_col_names){
  a = subset(reschool_summer_program, reschool_summer_program[,program_col_name] == TRUE )
  number_of_sessions[j] = nrow(a)
  j = j+1
}

category = program_col_names #colnames(reschool_summer_program)[program_col_names]

session_numberby_category = data.frame(number_of_sessions, category, stringsAsFactors=FALSE)

session_numberby_category$category = substring(session_numberby_category$category, 5)

#Merging the two datasets
programs_sessions = merge(search_programtype_summary, session_numberby_category, by = "category")

#Calculating the relavent metrics
programs_sessions$total_searches_perc = round((programs_sessions$total_searches * 100)/sum(programs_sessions$total_searches),2)
programs_sessions$number_of_searches_perc = round((programs_sessions$number_of_sessions * 100)/sum(programs_sessions$number_of_sessions),2)
programs_sessions$total_gap = programs_sessions$total_searches - programs_sessions$number_of_sessions
programs_sessions$perc_gap = programs_sessions$total_searches_perc - programs_sessions$number_of_searches_perc   

#Renaming the columns to ensure understandable texts
colnames(programs_sessions)[4:7] = c("Percentage of searches", "Percentage of programs", "Total gap", "Percentage gap")

#Summary data for searches made by session time
search_sessiontimes_summary = google_analytics %>% select(sessiontimes, users) %>% 
  filter(sessiontimes != '') %>% group_by(sessiontimes) %>% 
  summarize(total_searches = sum(users))

#Summary data of searches made by zipcode
search_zipcode_summary = google_analytics %>% 
  select(location, users) %>% 
  filter(location != '') %>% 
  group_by(location) %>% 
  summarize(total_searches = sum(users)) %>% 
  arrange(total_searches, location) %>% top_n(n = 20)

search_zipcode_summary$location = as.character(search_zipcode_summary$location)

#Summary daat of searches and programs by zipcode
reschool_summer_program$session_zip = as.character(reschool_summer_program$session_zip)
zipcode_programs = reschool_summer_program %>% 
  select(session_zip) %>% 
  filter(session_zip != '') %>% 
  group_by(session_zip) %>% 
  summarize(total_sessions = n()) %>% filter(session_zip %in% c(search_zipcode_summary$location))
colnames(zipcode_programs) = c("location", "total_sessions")

final_zipcode_searches_programs = merge(search_zipcode_summary, zipcode_programs, all.x = TRUE)


target <- c(search_zipcode_summary$location)
final_zipcode_searches_programs = final_zipcode_searches_programs[match(target, final_zipcode_searches_programs$location),]
final_zipcode_searches_programs = final_zipcode_searches_programs %>% filter(is.na(location) == FALSE)


# Search data map by zipcode
# Aggregate the searches by zipcode
search_zipcode_summary_map = google_analytics %>% 
  select(location, users) %>% 
  filter(location != '') %>% 
  group_by(location) %>% 
  summarize(total_searches = sum(users)) %>% 
  arrange(total_searches, location) %>% filter(location %in% total_denver_zipcodes$zipcode)

search_map_data <- geo_join(relevant_zip_codes, search_zipcode_summary_map, 
                            "GEOID10", "location", how = "inner")

# Get the exhaustive zipcodes from the reschool dataset from the clean schema 
subset_denver_zipcodes = reschool_summer_program[reschool_summer_program$session_city == 'Denver',]
subset_denver_zipcodes = relevant_zip_codes[relevant_zip_codes$GEOID10 %in% c(subset_denver_zipcodes$session_zip), ]

###############################################################
# Creating racial distributions variables 

# Creating majority (really most common) race variables for each neighborhood
shape_census@data$majority_race <- max.col(as.matrix(
          shape_census@data[ ,c("PCT_HIS", "PCT_WHI", "PCT_BLA")]
                     ))
shape_census@data$majority_race <- gsub(1, "Hispanic", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(2, "White", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(3, "Black", shape_census@data$majority_race)