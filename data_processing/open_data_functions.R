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
SavePolygonsAsCSV <- function(spdf, filename) {
  # Compute centroids of polygons and save to data frame
  centroids <- SpatialPointsDataFrame(gCentroid(spdf, byid=TRUE), spdf@data, match.ID=FALSE) 
  centroids <- as.data.frame(centroids)
  colnames(centroids)[colnames(centroids)=="x"] <- "long"  # for consistency across files
  colnames(centroids)[colnames(centroids)=="y"] <- "lat"  # for consistency across files
  
  # Compute area in sqft of each polygon and add to data frame
  equalAreaProj <- spTransform(spdf, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=ft +no_defs"))
  centroids$sqft <- gArea(equalAreaProj, byid=TRUE)
  
  # Save result to csv
  MakeDir("clean_data")  # make a clean data directory, if one doesn't exist yet
  write.csv(centroids, file=file.path("clean_data",filename), row.names=FALSE, na = "")
}

# Function to turn a SpatialPointsDataFrame into a flat csv
#    input:   SpatialPointsDataFrame and filename (string) for the saved csv
#    output:  n/a
#    note:    saves the csv to clean_data directory (within current working directory), 
#             csv has SPDF data + lat/long
SavePointsAsCSV <- function(spdf, filename) {
  # Get and format lat/long info
  df <- as.data.frame(spdf)
  colnames(df)[colnames(df)=="coords.x1"] <- "long"  # for consistency across files
  colnames(df)[colnames(df)=="coords.x2"] <- "lat"  # for consistency across files
  
  # Save result to csv
  MakeDir("clean_data") # make a clean data directory, if one doesn't exist yet 
  write.csv(df, file=file.path("clean_data",filename), row.names=FALSE, na = "")
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