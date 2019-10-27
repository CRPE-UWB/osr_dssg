# Create a directory in the current working directory, if one with the given name doesn't exist yet
#    input:   name of directory to create (string)
#    output:  n/a
MakeDir <- function(dirName) {
  dataDir <- file.path(getwd(), dirName)
  if (!dir.exists(dataDir)) {
    dir.create(dataDir)
  }
}

# Function to load data from Denver Open Data shapefiles
#     input:   name of zip file - see url where data resides to find this (string)
#     output:  SpatialPointsDataFrame or SpatialPolygonsDataFrame with shapefile data
GetOpenData <- function(zipname) {
  MakeDir("raw_data")  # make a raw data directory, if one doesn't exist yet
  
  # Download .zip to raw data directory, then unzip in temporary directory.
  url <- paste("https://www.denvergov.org/media/gis/DataCatalog/", zipname, "/shape/", zipname, ".zip", sep="")
  tempDir <- tempdir()
  file <- file.path("raw_data", paste(zipname, ".zip", sep=""))
  download.file(url, file)
  unzip(file, exdir = tempDir)
  
  # Read in shapefile from unzipped data and return result
  spdf <- readOGR(dsn = tempDir, zipname)
  return(spdf)
}

# Function to turn a SpatialPolygonsDataFrame into a flat csv
#    input:   SpatialPolygonsDataFrame and filename (string) for the saved csv
#    output:  n/a
#    note:    saves the csv to clean_data directory (within current working directory), 
#             csv has SPDF data + centroid lat/long + polygon area in square feet
SavePolygonsAsCSV <- function(spdf, dirname, filename) {
  # Compute centroids of polygons and save to data frame
  centroids <- SpatialPointsDataFrame(gCentroid(spdf, byid=TRUE), spdf@data, match.ID=FALSE) 
  centroids <- as.data.frame(centroids)
  colnames(centroids)[colnames(centroids)=="x"] <- "long"  # for consistency across files
  colnames(centroids)[colnames(centroids)=="y"] <- "lat"  # for consistency across files
  
  # Compute area in sqft of each polygon and add to data frame
  equalAreaProj <- spTransform(spdf, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=ft +no_defs"))
  centroids$sqft <- gArea(equalAreaProj, byid=TRUE)
  
  # Save result to csv
  write.csv(centroids, file=file.path(dirname,filename), row.names=FALSE, na = "")
}

# Function to turn a SpatialPointsDataFrame into a flat csv
#    input:   SpatialPointsDataFrame and filename (string) for the saved csv
#    output:  n/a
#    note:    saves the csv to clean_data directory (within current working directory), 
#             csv has SPDF data + lat/long
SavePointsAsCSV <- function(spdf, dirname, filename) {
  # Get and format lat/long info
  df <- as.data.frame(spdf)
  colnames(df)[colnames(df)=="coords.x1"] <- "long"  # for consistency across files
  colnames(df)[colnames(df)=="coords.x2"] <- "lat"  # for consistency across files
  
  # Save result to csv
  write.csv(df, file=file.path(dirname,filename), row.names=FALSE, na = "")
}

# Function to split a column that contains comma-separated lists into separate binary columns
#    input:   dataframe, column name (string) containing comma-separated lists to split
#    output:  dataframe, with binary (0/1) columns replacing the specified column
SplitCommas <- function(df, colname) {
  df[[colname]] <- as.character(df[[colname]])
  dfSplit <- cSplit_e(df, colname, sep = ",", mode = "binary",
                      type = "character", fill = 0, drop = TRUE)
  return(dfSplit)
}

# Function for getting the unique values in a column, which will be useful for the "values" 
#section in the codebook (but only for the variables for which this is relevant: i.e. the factors)

# Function to get codes from a particular column in a data frame
#    input:   data frame, column name (string)
#    output:  list of codes used in that column
GetCodes <- function(df, colName) {
  vals <- sort(unique(df[[colName]]))
  print(vals)
}

# Function to load nbhd demographic data from Denver Open Data
#     input:   name of zip file - see url where data resides to find this (string)
#     output:  SpatialPointsDataFrame or SpatialPolygonsDataFrame with shapefile data
GetNbhdDemog <- function(){
  nbhds <- GetOpenData('american_community_survey_nbrhd_2011_2015')
  names_wanted <- c("NBHD_NAME",  # neighborhood name
                    "TTL_POPULA",  # total population - used for racial demographics
                    "PCT_HISPAN",  # percent hispanic
                    "PCT_WHITE",  # percent white
                    "PCT_BLACK",  # percent black
                    "PCT_NATIVE",  # percent native american
                    "PCT_ASIAN",  # percent asian
                    "PCT_HAWAII",  # percent hawaiian
                    "PCT_OTHERR",  # percent other race
                    "PCT_TWOORM",  # percent two or more races
                    "MED_HH_INC",  # median househol income
                    "LESS_THAN_",  # population 25+ with les than hs diploma
                    "HSGRAD_OR_",  # population 25+ with hs diploma or equivalent
                    "SOMECOLLEG",  # population 25+ with some college
                    "BACHELORS_",  # population 25+ with bachelors degree
                    "TTLPOP_25P",  # total population 25+ - used for education data
                    "AGE_5_TO_9", # for total students in age range 5-18
                    "AGE_10_TO_", # for total students in age range 5-18
                    "AGE_15_TO_", # ages 15-17
                    "TTLPOP_5PL",  # total population 5 plus years - used for language data
                    "ONLY_ENGLI"  # total population 5 plus years with only english
  )
  
  # subset to the columns we need
  nbhds_small <- nbhds[,names_wanted]
  
  ## DECIDING TO KEEP: LESS THAN HS DIPLOMA, and BACHELORS OR ABOVE
  nbhds_small@data$PCT_LESS_HS <- 100 * nbhds_small@data$LESS_THAN_ / nbhds_small@data$TTLPOP_25P
  nbhds_small@data$PCT_COLL_GRAD <- 100 * nbhds_small@data$BACHELORS_ / nbhds_small@data$TTLPOP_25P
  
  # get total students in age range 5-17
  nbhds_small@data$AGE_5_TO_17 <- nbhds_small@data$AGE_5_TO_9 + nbhds_small@data$AGE_10_TO_ +
    nbhds_small@data$AGE_15_TO_
  
  # get percent non english speakers
  nbhds_small@data$PCT_NON_ENGL <- 100 * (nbhds_small@data$TTLPOP_5PL - 
                                            nbhds_small@data$ONLY_ENGLI) / nbhds_small@data$TTLPOP_5PL
  
  # combine small percentage native races: native, hawaiian
  nbhds_small@data$PCT_NATIVE <- nbhds_small@data$PCT_NATIVE + nbhds_small@data$PCT_HAWAII
  
  #colnames(nbhds_small@data)
  final_colnames <- c("NBHD_NAME",
                      "MED_HH_INC", 
                      "TTL_POPULA",
                      "PCT_HISPAN", 
                      "PCT_WHITE",
                      "PCT_BLACK",
                      "PCT_ASIAN",
                      "PCT_NATIVE",
                      "PCT_OTHERR",
                      "PCT_TWOORM",
                      "TTLPOP_25P",
                      "PCT_LESS_HS", 
                      "PCT_COLL_GRAD",
                      "AGE_5_TO_17",
                      "TTLPOP_5PL",
                      "PCT_NON_ENGL")
  
  finalspdf <- nbhds_small[,final_colnames]
  
  simpler_colnames <- c("NBHD_NAME",
                        "MED_HH_INC", 
                        "POP_RACE",
                        "PCT_HISPANIC", 
                        "PCT_WHITE",
                        "PCT_BLACK",
                        "PCT_ASIAN",
                        "PCT_NATIVE",
                        "PCT_OTHER_RACE",
                        "PCT_TWO_OR_MORE_RACES",
                        "POP_EDUC",
                        "PCT_LESS_HS", 
                        "PCT_COLL_GRAD", 
                        "AGE_5_TO_17",
                        "POP_LANG",
                        "PCT_NON_ENGL")
  
  colnames(finalspdf@data) <- simpler_colnames
  
  return(finalspdf)
}