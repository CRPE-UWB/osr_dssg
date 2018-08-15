# Function to process the B4S programs data, for use in the Shiny app

# INPUT: dataframe of raw B4S programs data (csv) with at least the following columns (more is okay):
          # "camp_name", "session_name", "session_short_description", "session_cost", 
          # "first_session_date", "last_session_date", "first_session_start_time", "first_session_end_time"
          # "session_count", "session_address_name", "session_address_1", "session_address_2", 
          # "session_city", "session_state", "session_zip", "session_categories"

# OUTPUT: dataframe of Shiny-relevant B4S programs data, with columns:
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

raw_to_shiny_b4s <- function(raw_df) {
  
  cols_to_keep <- c("camp_name", "session_name", "session_short_description", "session_cost", 
                    "first_session_date", "last_session_date", "first_session_start_time", "first_session_end_time", 
                    "session_count", "session_address_name", "session_address_1", "session_address_2", 
                    "session_city", "session_state", "session_zip", "session_categories")
  
  programdata <- raw_df[, cols_to_keep]
  
  ######## fix column types ########
  integer_cols <- c("session_cost", "session_zip")
  
  char_cols <- c("camp_name", "session_name", "session_short_description", "first_session_date", 
                 "last_session_date", "session_address_name", "session_address_1", "session_address_2",
                 "session_city", "session_state", "session_categories")
  
  for (col in integer_cols) {
    programdata[[col]] <- as.integer(programdata[[col]])
  }
  
  for (col in char_cols) {
    programdata[[col]] <- as.character(programdata[[col]])
  }
  
  ######## get rid of leading/trailing whitespace in session_city, session_address1 ########
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  programdata$session_city <- trim(programdata$session_city)
  programdata$session_address_1 <- trim(programdata$session_address_1)
  programdata$session_address_1 =   gsub("\\.$", " ", programdata$session_address_1)

  ####### if any addresses have NA values, fill with zipcode associated with them in another row #######
  
  programdata <- programdata[order(programdata$session_city, programdata$session_address_1) , ]
  programdata = fill(programdata, session_zip)
  
  ######## remove non-utf characters at the beginning of addresses ########
  
  programdata$session_address_1 <- gsub('*\xa0', '', programdata$session_address_1)
  programdata$session_address_1 <- gsub('*\xe5\xca', '', programdata$session_address_1)
  
  ######## Getting the unique addresses of programs for geocoding ########
  
  camp_address = unique(programdata[c("session_address_1",  "session_city", "session_state", "session_zip")])
  camp_address = camp_address[order(camp_address$session_address_1) , ]
  
  # manually replace bad zipcodes
  for (i in 2:nrow(camp_address)){
    if(camp_address[i,1] == camp_address[i-1,1]){
      camp_address[i,3] = camp_address[i-1,3]
    }
  }
  
  camp_address = unique(camp_address)
  
  # Get all the pieces of the session address in one column
  camp_address$complete_session_address <- paste(camp_address$session_address_1, 
                                                 camp_address$session_city, "CO", 
                                                 camp_address$session_zip)
  
  ######## Geocode program addresses so that we have lat/long coordinates ########
  
  # Initialize the lat/longs to NAs
  camp_address$long <- NA
  camp_address$lat <- NA
  
  # Run the geocoding!
  max_runs <- 5  # sometimes don't get the lat/long on the first run - try again
  
  for (k in 1:max_runs) {
    notGeocoded <- which( is.na(camp_address$lat) | is.na(camp_address$long) )
    for (i in notGeocoded) {
      result <- geocode(camp_address$complete_session_address[i], 
                        output="latlona", 
                        source="google")
      
      camp_address$long[i] <- as.numeric(result[1])
      camp_address$lat[i] <- as.numeric(result[2])
      
      Sys.sleep(0.5)  # prevent over 50 google maps queries per second
    }
  }

  ######## Join geocodings to big dataset ########
  
  camp_address_intermediate <- camp_address[,c("session_address_1",
                                               "session_city", "session_state", 
                                               "session_zip", 
                                               "lat","long")]
  
  programdata_final <- merge(x = programdata, y = camp_address_intermediate, sort=FALSE)
  
  ######## Split the session categories into different columns ########
  
  programdata_final <- cSplit_e(programdata_final, "session_categories", sep=",", mode = "binary",
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
  
  for (x in c("academic", "arts", "cooking", "dance", "drama", "music", "nature", "sports", "stem")) {
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
  
  ######## Add the block group ids to the dataset ########
  
  # get block group polygons
  dataPath <- file.path(github_dir, "data", "census_clean", "shape_census")
  blockGroups <- readOGR(dsn = dataPath, 'shape_census')
  
  # figure out which block group each program is in
  coords <- data.frame(programdata_final$long, programdata_final$lat)
  coords <- unique(coords)
  colnames(coords) <- c("long", "lat")
  spatialReschool <- SpatialPoints(coords, proj4string = CRS(proj4string(blockGroups)))
  nests <- gIntersects(spatialReschool, blockGroups, byid = TRUE)  # rows=bgroups, cols=programs
  
  # each program should be in at most one block group!! check:
  print(paste( sum(colSums(nests) > 1), "errors" ))
  
  true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = bgroup idx, col2 = program idx
  bgroup_idxs  <- true_idxs[,1]
  program_idxs <- true_idxs[,2]
  
  reschool_bgs <- data.frame(coords[program_idxs,],
                             blockGroups$Id2[bgroup_idxs]
  )
  colnames(reschool_bgs) <- c("long", "lat", "bgroup_id2")
  
  # strip leading 0's in block group ids
  reschool_bgs$bgroup_id2 <- as.character(as.numeric(as.character(reschool_bgs$bgroup_id2))) 
  print(reschool_bgs)
  
  programdata_final_bgs <- merge(programdata_final, reschool_bgs, 
                                 by.x=c("lat", "long"), 
                                 by.y=c("lat", "long"), 
                                 all.x=TRUE, all.y=FALSE)
  
  ######## Create Cost Per Day Variable ########
  
  programdata_final_bgs$Posfirst_session_start_time <- strptime(programdata_final_bgs$first_session_start_time, 
                                                                "%I:%M:%S %p")
  programdata_final_bgs$Posfirst_session_end_time <- strptime(programdata_final_bgs$first_session_end_time, 
                                                              "%I:%M:%S %p")
  programdata_final_bgs$Hours_Day <- (programdata_final_bgs$Posfirst_session_end_time - 
                                        programdata_final_bgs$Posfirst_session_start_time)/(60*60) 
  # differences are stored numerically in seconds, counting up from a static date. 
  # Thus we do last minus first for the difference in seconds, divide by 60*60 for to convert to hours
  
  # spot checking: 0s (0 hours per day) are a for a camp, so we treat those as 24 hours.
  # 11:59s are for an online course, so we drop em.
  programdata_final_bgs$Hours_Day[programdata_final_bgs$Hours_Day>23] <- NA
  programdata_final_bgs$Hours_Day[programdata_final_bgs$Hours_Day==0] <- 24
  
  ######## Get number of days for each program ########
  
  # for sessions less than one week
  programdata_final_bgs$first_session_date <- as.Date(programdata_final_bgs$first_session_date, format = "%m /%d /%Y ")
  programdata_final_bgs$last_session_date <- as.Date(programdata_final_bgs$last_session_date, format = "%m /%d /%Y")
  programdata_final_bgs$raw_days <- programdata_final_bgs$last_session_date - programdata_final_bgs$first_session_date + 1
  
  # also count number of weeks
  programdata_final_bgs$raw_weeks <- ifelse(programdata_final_bgs$raw_days<7,0,programdata_final_bgs$raw_days/7)
  programdata_final_bgs$days <- ifelse(programdata_final_bgs$raw_week==0, 
                                       programdata_final_bgs$raw_days, 
                                       programdata_final_bgs$raw_weeks*5)
  
  
  ######## Approximate cost per day - update if we get better data on dates!!! ########
  programdata_final_bgs$cost_per_day <- programdata_final_bgs$session_cost/programdata_final_bgs$days
  
  ############################# DELETE UNNECESSARY COLUMNS AND FINISH! #############################
  
  shiny_df <- programdata_final_bgs
  
  final_cols_to_keep <- c("lat", "long", "session_zip", "camp_name", "session_name",
                          "session_short_description", "session_cost",
                          "first_session_date", "last_session_date", "first_session_start_time",
                          "first_session_end_time",  "session_count", "has_academic",
                          "has_arts", "has_cooking", "has_dance", "has_drama", "has_music", "has_nature",
                          "has_sports", "has_stem", "has_scholarships",
                          "has_special_needs_offerings", "cost_per_day"
                          )
  shiny_df <- programdata_final_bgs[ , final_cols_to_keep]

  colnames(shiny_df) <- c("lat", "long", "session_zip", "camp_name", "session_name",
                          "session_short_description", "session_cost",
                          "session_start_date", "session_end_date", "first_session_start_time",
                          "first_session_end_time",  "session_count", "has_academic",
                          "has_arts", "has_cooking", "has_dance", "has_drama", "has_music", "has_nature",
                          "has_sports", "has_stem", "has_scholarships", 
                          "has_special_needs_offerings", "cost_per_day"
                          )
  
  return(shiny_df)
 
}