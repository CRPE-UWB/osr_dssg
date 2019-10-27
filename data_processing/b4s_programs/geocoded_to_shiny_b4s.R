# Function to process the B4S programs data, for use in the Shiny app (and calculating the access index)

# INPUT: geocoded dataframe of raw B4S session data (csv).
# MUST INCLUDE the columns: 
# "session_id", "camp_name", "session_name", "session_short_description", "session_categories",  
# "session_zip", "session_size", "session_min_age", "session_max_age","session_cost", 
# "session_date_start", "session_date_end", "calendar_date",
# "start.time.hours", "end.time.hours", "duration_minutes", "length",
# "lat", "lon"

# OLD OUTPUTS: dataframe of Shiny-relevant B4S programs data, with (at least) columns:
# "lat", "long", "session_zip", "camp_name", "session_name",
# "session_short_description", "session_cost",
# "session_start_date", "session_end_date", "first_session_start_time",
# "first_session_end_time",  "session_count", "has_academic",
# "has_arts", "has_cooking", "has_dance", "has_drama", "has_music", "has_nature",
# "has_sports", "has_stem", "has_scholarships", 
# "has_special_needs_offerings", "cost_per_day"

library(tidyr)
library(splitstackshape)  # for splitting the categories column
library(ggmap)
library(rgdal)
library(rgeos)

geocoded_to_shiny_b4s <- function(raw_df, shape_census_folder, bg_nbhds_file_path) {
  
  cols_to_keep <- c("session_id", "camp_name", "session_name", "session_short_description", "session_categories",  
                    "session_zip", "session_size", "session_min_age", "session_max_age","session_cost", 
                    "session_date_start", "session_date_end", "calendar_date",
                    "start.time.hours", "end.time.hours", "duration_minutes", "length", # length is number of hours
                    "lat", "lon", "session_address_1", "session_city")  #session_count, start_time_minutes
  
  programdata <- raw_df[ , cols_to_keep]
  
  ######## fix column types ########
  integer_cols <- c("session_cost", "session_zip", "session_id", "session_size", "session_min_age", "session_max_age",
                     "duration_minutes")
  
  float_cols <- c("length", "lat", "lon")
  
  char_cols <- c("camp_name", "session_name", "session_short_description", "session_date_start", 
                 "session_date_end", "calendar_date", "session_categories", "start.time.hours", "end.time.hours")
  
  for (col in integer_cols) {
    programdata[[col]] <- as.integer(programdata[[col]])
  }
  
  for (col in float_cols) {
    programdata[[col]] <- as.numeric(programdata[[col]])
  }
  
  for (col in char_cols) {
    # delete non-utf-8 characters as well
    programdata[[col]] <- iconv(as.character(programdata[[col]]), "UTF-8", "UTF-8",sub='')
  }
  
  ######## Format start and end dates appropriately, as yyyy-mm-dd ########
  programdata$session_date_start <- strptime(programdata$session_date_start, "%m/%d/%Y %H:%M")
  programdata$session_date_start <- format(programdata$session_date_start, "%Y-%m-%d")
    
  programdata$session_date_end <- strptime(programdata$session_date_end, "%m/%d/%Y %H:%M")
  programdata$session_date_end <- format(programdata$session_date_end, "%Y-%m-%d")
  
  ######## Drop any programs that we couldn't successfully geocode #######
  programdata <- programdata[!is.na(programdata$lat),]
  
  ######## Split the session categories into different columns ########
  
  programdata_final <- cSplit_e(programdata, "session_categories", sep=",", mode = "binary",
                                type = "character", fill = 0, drop = TRUE)
  # Change 0/1 encoding to true/false in the new split columns
  newCols <- grep("session_categories", colnames(programdata_final))
  for (colnum in newCols) {
    programdata_final[,colnum] <- as.logical(programdata_final[,colnum])
  }
  
  ######## Simplify column names ########
  
  dbSafeNames = function(names) {
    names = gsub('[^a-z0-9]+','_',tolower(names))
    names = make.names(names, unique=TRUE, allow_=TRUE)
    names = gsub('.','_',names, fixed=TRUE)
    names
  }
  colnames(programdata_final) = dbSafeNames(colnames(programdata_final))
  
  for (x in c("academic", "academics", "arts", "cooking", "dance", "drama", "music", "nature", "sports", "stem")) {
    oldName <- paste("session_categories_", x, sep="")
    newName <- paste("has_", x, sep="")
    if ( length(colnames(programdata_final)[colnames(programdata_final) == oldName]) > 0 ){
      colnames(programdata_final)[colnames(programdata_final) == oldName] <- newName
    }
    else{
      programdata_final[[newName]] <- FALSE
    }
    
  }
  colnames(programdata_final)[colnames(programdata_final) 
                              == "session_categories_scholarshipsavailable"] <- "has_scholarships"
  colnames(programdata_final)[colnames(programdata_final) 
                              == "session_categories_offersbeforeaftercare"] <- "has_before_after_care"
  colnames(programdata_final)[colnames(programdata_final) 
                              == "session_categories_specialneedsstudent"] <- "has_special_needs_offerings"
  
  colnames(programdata_final)[colnames(programdata_final) 
                             == "lon"] <- "long"
  
  programdata_final$has_academic <- as.logical(programdata_final$has_academic + programdata_final$has_academics)
  programdata_final$has_academics <- NULL
  
  ######## Add the block group ids to the dataset ########
  
  # get block group polygons
  blockGroups <- readOGR(dsn = shape_census_folder, 'shape_census')
  
  # figure out which block group each program is in
  coords <- data.frame(programdata_final$long, programdata_final$lat)
  coords <- unique(coords)
  colnames(coords) <- c("long", "lat")
  spatialReschool <- SpatialPoints(coords, proj4string = CRS(proj4string(blockGroups)))
  nests <- gIntersects(spatialReschool, blockGroups, byid = TRUE)  # rows=bgroups, cols=programs
  
  # each program should be in at most one block group!! check:
  #print(paste( sum(colSums(nests) > 1), "errors" ))
  
  true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = bgroup idx, col2 = program idx
  bgroup_idxs  <- true_idxs[,1]
  program_idxs <- true_idxs[,2]
  
  reschool_bgs <- data.frame(coords[program_idxs,],
                             blockGroups$Id2[bgroup_idxs]
  )
  colnames(reschool_bgs) <- c("long", "lat", "bgroup_id2")
  
  # strip leading 0's in block group ids
  reschool_bgs$bgroup_id2 <- as.character(as.numeric(as.character(reschool_bgs$bgroup_id2))) 
  
  programdata_final_bgs <- merge(programdata_final, reschool_bgs, 
                                 by.x=c("lat", "long"), 
                                 by.y=c("lat", "long"), 
                                 all.x=TRUE, all.y=FALSE)
  
  ######## Add nbhd names and ids to dataset ########
  
  bg_nbhds <- read.csv(bg_nbhds_file_path)
  
  programdata_final_nbhds <- merge(programdata_final_bgs, bg_nbhds, 
                                   by.x=c("bgroup_id2"), 
                                   by.y=c("bgroup_id2"), 
                                   all.x=TRUE, all.y=FALSE)
  # fix types
  programdata_final_nbhds$nbhd_name <- as.character(programdata_final_nbhds$nbhd_name)
  programdata_final_nbhds$bgroup_id2 <- as.numeric(programdata_final_nbhds$bgroup_id2)
  
  ######## Create Cost Per Day Variable ########

  # number of hours and days
  # assuming here that each day is the same number of hours
  total_session_hours <- programdata_final_nbhds %>% group_by(session_id) %>% summarize(total_hours = sum(length))
  total_session_days <- programdata_final_nbhds %>% group_by(session_id) %>% count()
  colnames(total_session_days) <- c("session_id", "total_days")
  aggregate_session_data <- merge(programdata_final_nbhds, total_session_hours, by.x="session_id", by.y="session_id", 
                                  all.x=TRUE, all.y=FALSE)
  aggregate_session_data <- merge(aggregate_session_data, total_session_days, by.x="session_id", by.y="session_id", 
                                  all.x=TRUE, all.y=FALSE)
  
  # get rid of redundant rows 
  aggregate_session_data <- aggregate_session_data %>% distinct(session_id, .keep_all = TRUE)
  
  ######## Cost per hour ########
  aggregate_session_data$cost_per_hour <- aggregate_session_data$session_cost/aggregate_session_data$total_hours
  
  ############################# DELETE UNNECESSARY COLUMNS AND FINISH! #############################
  
  final_cols_to_keep <- c("session_name", "session_date_start", "session_date_end", "session_cost",
                          "lat", "long", "nbhd_id", "nbhd_name", "camp_name", "session_short_description", 
                          "has_special_needs_offerings", "has_scholarships", "has_academic", "has_arts",
                          "has_cooking", "has_dance", "has_drama", "has_music", "has_nature", "has_sports", 
                          "has_stem", "bgroup_id2", "session_address_1", "session_zip", "session_city", 
                          "session_id", "cost_per_hour", "total_days", "length")
  
  shiny_df <- aggregate_session_data[ , final_cols_to_keep]
  
  return(shiny_df)
  
}