#####################################################################
# This file contains necessary pre-processing of data for the Shiny
# app, and saves everything in .RData formats for quick loading in 
# the app.

# Note: all file paths are relative to LOCATION OF THIS FILE

library(dplyr)
library(rgdal)
library(rgeos)
library(tigris)

# Specify locations of files (updated)
osr_folder <- dirname(getwd())
open_data_path <- file.path(osr_folder, 'DATA', 'Open_Denver')
google_analytics_path <- file.path(osr_folder, 'DATA', "google_analytics", "google_analytics_clean.csv")
zipcode_path <- file.path(osr_folder, "DATA", "zip_codes")
denver_zipcode_path <- file.path(osr_folder, "DATA", "denver_zipcodes.csv")
census_bg_path <- file.path(osr_folder, "DATA", "Census_demo", "shape_census")
nbhd_shape_path <- file.path(osr_folder, "DATA", "Open_Denver", "nbhd_dem_shapes")
access_path <- file.path(osr_folder, 'DATA', "access_index")

geocoded_b4s_path <- file.path(osr_folder, 'DATA', 'B4Sprograms', 'geocoded_data', 'complete_geocoded_sessions.csv')
shape_census_folder <- file.path(osr_folder, 'DATA', 'Census_demo', 'shape_census')
bg_nbhds_file_path <- file.path(osr_folder, 'DATA', 'Census_demo', 'bg_nbhds.csv')

# SPECIFY WHERE TO SAVE PROCESSED FILES
dataDir <- file.path(getwd(), "shiny_data")
if (!dir.exists(dataDir)) {
  dir.create(dataDir)
}

#####################################################################
# processing the summer programs data

raw_df <- read.csv(file = geocoded_b4s_path, na.strings = "", stringsAsFactors = FALSE)
source( file.path(osr_folder, "data_processing", "b4s_programs", "geocoded_to_shiny_b4s.R") )
reschool_summer_program <- geocoded_to_shiny_b4s(raw_df, shape_census_folder, bg_nbhds_file_path)

# drop any programs without block groups - not in the confines of Denver
reschool_summer_program <- reschool_summer_program[!is.na(reschool_summer_program$bgroup_id2),]

# aggregate data by neighborhood
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

aggregate_session_nbhds <- aggregate(reschool_summer_program$session_id,
                                     by = list(nbhd_name=reschool_summer_program$nbhd_name),
                                     FUN = n_distinct
)

reschool_summer_program$session_zip = as.character(reschool_summer_program$session_zip)

save(reschool_summer_program, nbhd_program_summary, aggregate_session_nbhds,
     file = file.path("shiny_data", "summer_programs.RData"))


#####################################################################
#####################################################################
#####################################################################
# processing the google analytics data

google_analytics = read.csv(google_analytics_path)

# Convert the necessary columns to numeric
google_analytics$mincost = as.numeric(google_analytics$mincost)
google_analytics$maxcost = as.numeric(google_analytics$maxcost)
google_analytics$minage = as.numeric(google_analytics$minage)
google_analytics$maxage = as.numeric(google_analytics$maxage)
google_analytics$distance = as.numeric(google_analytics$distance)

# fixing some new formatting things to be consistent with old style
colnames(google_analytics)[colnames(google_analytics)=="Users"] <- "users"
google_analytics$Date <- NULL
google_analytics$X <- 1:nrow(google_analytics)
google_analytics$lat <- NA
google_analytics$long <- NA

col_order <- c("X", "gender", "distance", "category", "mincost", "maxcost", "sessiontimes", 
               "minage", "maxage", "sort", "location", "scholarships", "specialneeds", "beforeaftercare",
               "gifted", "keywords", "lat", "long", "users")
google_analytics <- google_analytics[,col_order]

# Create unique zipcodes (ignore locations that aren't sensible zipcodes)
google_analytics[-grep("80\\d{3}",google_analytics$location),"location"] <- NA
google_analytics[grep("80\\d{3}",google_analytics$location),"location"] <-
  gsub(".*(80\\d{3}).*","\\1",
       google_analytics[grep("80\\d{3}",google_analytics$location),"location"]
  )
zipcode_searchdata = unique(google_analytics$location)

# Replacing empty category values with NAs
google_analytics$category[google_analytics$category == ''] <- NA

# Create sorted search summary
search_sort_summary = google_analytics %>% 
  select(sort, users) %>% 
  filter(sort != '') %>% 
  group_by(sort) %>% 
  summarize(total_searches = sum(users))

# Create distance search summary
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

# Create program category search summary
search_programtype_summary = google_analytics %>% select(category, users) %>% 
  filter(category != '') %>% group_by(category) %>% 
  summarize(total_searches = sum(users)) 

# Create session time search summary
search_sessiontimes_summary = google_analytics %>% select(sessiontimes, users) %>% 
  filter(sessiontimes != '') %>% group_by(sessiontimes) %>% 
  summarize(total_searches = sum(users))

#Create summary data of searches made by zipcode
search_zipcode_summary = google_analytics %>% 
  select(location, users) %>% 
  filter(location != '') %>% 
  group_by(location) %>% 
  summarize(total_searches = sum(users)) %>% 
  arrange(total_searches, location) %>% top_n(n = 20)
search_zipcode_summary$location = as.character(search_zipcode_summary$location)

# Save all to RData file for quick loading
save(google_analytics, zipcode_searchdata, search_sort_summary, search_distance_summary, 
     search_programtype_summary, search_sessiontimes_summary, search_zipcode_summary,
     file = file.path("shiny_data", "google_analytics.RData"))

#####################################################################
#####################################################################
#####################################################################
# Preparing the Denver Open Data for Shiny

fields = read.csv( file.path(open_data_path, 'fields.csv') )
libraries = read.csv( file.path(open_data_path, 'libraries.csv') )
playgrounds = read.csv( file.path(open_data_path, 'playgrounds.csv') )
rec_centers = read.csv( file.path(open_data_path, 'rec_centers.csv') )
parks = read.csv( file.path(open_data_path, 'parks.csv') )
pools = read.csv( file.path(open_data_path, 'pools.csv') )

save(fields, libraries, playgrounds, rec_centers, parks, pools,
     file = file.path("shiny_data", "denver_open_data.RData"))

###############################################################
###############################################################
###############################################################
# Prepare Access Index data for Shiny

driving_index <- read.csv( file.path(access_path, "driving_index.csv") )
driving_index_disability <- read.csv( file.path(access_path, "driving_index_disabillity.csv") )
#driving_index_nbhd <- read.csv( file.path(access_path, "driving_index_nbhd.csv") )
#driving_index_disability_nbhd <- read.csv( file.path(access_path, "driving_index_disability_nbhd.csv") )

transit_index <- read.csv( file.path(access_path, "transit_index.csv") )
transit_index$X <- NULL
transit_index_disability <- read.csv( file.path(access_path, "transit_index_disabillity.csv") )
#transit_index_nbhd <- read.csv( file.path(access_path, "transit_index_nbhd.csv") )
#transit_index_nbhd_disability_nbhd <- read.csv( file.path(access_path, "transit_index_disability_nbhd.csv") )

# transit index may have block groups missing (no transit data available) - add NAs
full_bgs <- unique(driving_index$Id2)
missing_bgs <- full_bgs[which(! full_bgs %in% unique(transit_index$Id2))]
for (bg in missing_bgs){
  row <- list(Id2=bg)
  transit_index[nrow(transit_index) + 1, names(row)] <- row
}

missing_bgs_disability <- full_bgs[which(! full_bgs %in% unique(transit_index_disability$Id2))]
for (bg in missing_bgs_disability){
  row <- list(Id2=bg)
  transit_index_disability[nrow(transit_index_disability) + 1, names(row)] <- row
}


save(driving_index, driving_index_disability, transit_index, transit_index_disability, 
     #driving_index_nbhd, driving_index_disability_nbhd, transit_index_nbhd, transit_index_nbhd_disability_nbhd,
     file = file.path("shiny_data", "access_index.RData"))

###############################################################
###############################################################
###############################################################
# Format census demographic and neighborhood data

# Block group shape file (for access index)
shape_census_block <- readOGR(dsn = census_bg_path, layer = "shape_census")
shape_census_block@data$Id2 <- as.numeric(as.character(shape_census_block@data$Id2))
shape_census_block <- shape_census_block[order(shape_census_block@data$Id2),]

# Neighborhood shape file (for everything else)
shape_census <- readOGR(dsn = nbhd_shape_path, layer = "nbhd_dem_shapes")

# Join the 'number of sessions' information with the census shape file
shape_census <- geo_join(shape_census, aggregate_session_nbhds, 
                         "NBHD_NA", "nbhd_name", how = "left")

# Order the census shape file by neighborhood name
shape_census <- shape_census[order(as.character(shape_census@data$NBHD_NA)),]

# Create "most common race" variables for each neighborhood
shape_census@data$majority_race <- max.col(as.matrix(
  shape_census@data[ ,c("PCT_HIS", "PCT_WHI", "PCT_BLA")]
))
shape_census@data$majority_race <- gsub(1, "Hispanic", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(2, "White", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(3, "Black", shape_census@data$majority_race)

relevant_zip_codes = readOGR(dsn=zipcode_path)
total_denver_zipcodes = read.csv(denver_zipcode_path)

# Save as .RData
save(shape_census, shape_census_block, file = file.path("shiny_data", "census.RData"))
save(relevant_zip_codes, total_denver_zipcodes, file = file.path("shiny_data", "zipcodes.RData"))

###############################################################
###############################################################
###############################################################
# Misc. other pre-processing

# Compute % of existing programs by category from reschool search data 
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

# Compare % existing to % searches for each program category
programs_sessions = merge(search_programtype_summary, session_numberby_category, by = "category")
programs_sessions$total_searches_perc = round((programs_sessions$total_searches * 100)/sum(programs_sessions$total_searches),2)
programs_sessions$number_of_searches_perc = round((programs_sessions$number_of_sessions * 100)/sum(programs_sessions$number_of_sessions),2)
programs_sessions$total_gap = programs_sessions$total_searches - programs_sessions$number_of_sessions
programs_sessions$perc_gap = programs_sessions$total_searches_perc - programs_sessions$number_of_searches_perc   

# Renaming the columns to ensure understandable texts
colnames(programs_sessions)[4:7] = c("Percentage of searches", "Percentage of programs", "Total gap", "Percentage gap")

save(programs_sessions, file = file.path("shiny_data", "program_sessions.RData"))


###############################################################
###############################################################
###############################################################
# Misc. other pre-processing, part 2

# Summary data of searches and programs by zipcode
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

save(final_zipcode_searches_programs, file = file.path("shiny_data", "final_zipcode_searches_programs.RData"))

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

save(search_map_data, subset_denver_zipcodes, file = file.path("shiny_data", "search_map_data.RData"))
