# The following R code can be used for locating students inside 
# Census blocks, and matching each student's residential address 
# to the center-point (centroid) of their block.

# Original code:
# Patrick Denice, 
# pdenice@uw.edu
# Modified and adapted for UW DSSG2018 OSR by:
# Jose Hernandez
# joseh@uw.edu

# Updated: 2018-06-28

##################################################################

# STEP 0: Load packages

	## Notes: You may have to install these packages before 
	## loading them here.
	
library(XML)
library(RCurl)
library(sp)
library(rgdal)
library(rgeos)
library(maps)
library(maptools)
library(foreign)
library(tidyverse)

# STEP 1: Read in student data

	## Notes: Student data should have students' residential 
	## location in longitude and latitude format

#student_location <- read student file from raw

# STEP 2: Read in Census block shapefiles

	## Notes: These can be downloaded from the Census Tiger/Line
	## website. All shapefiles should be placed in the same 
	## file folder, which gets called below.
	## https://www.census.gov/geo/maps-data/data/tiger-line.html
  ## for Denver:
  ## https://www.denvergov.org/media/gis/DataCatalog/census_blocks_2010/shape/census_blocks_2010.zip

blocks <- readOGR("census_blocks_2010", "census_blocks_2010")
head(blocks@data)
# STEP 3: Find center-points (centroids) of blocks
plot(blocks)
## Identify the centroids using the 'blocks' data (read in above in step 2)
centroid <- SpatialPointsDataFrame(gCentroid(blocks, byid=T), blocks@data, match.ID=F) 


## Convert block identifiers into numeric format

centroid$geoid10 <- as.numeric(as.character(centroid$GEOID10))
centroid$geoid_num <- as.numeric(as.character(centroid$GEOID_NUM))


## Set 'centroid' as a dataframe
centroid <- as.data.frame(centroid)
names(centroid)

## Pare down, and rename variables
myvars <- c("geoid10", "geoid_num", "x", "y")
centroid <- centroid[myvars]
names(centroid) <- c("geoid10", "geoid_num", "blocklon", "blocklat")

# STEP 4: Locate students inside blocks

## Tell R that school coordinates are in the same lat/long reference system as the blocks data
student_location$LAT[is.na(student_location$LAT)] <- 0
student_location$LON[is.na(student_location$LON)] <- 0
coordinates(student_location) <- c("LON", "LAT")
proj4string(student_location) <- proj4string(blocks)

## Combine is.na() with over() to do a containment test (note that we need to "demote" blocks to a SpatialPolygons object)
inside.place <- !is.na(over(student_location, as(blocks, "SpatialPolygons")))

## Use "over" again, this time with blocks as a SpatialPolygonsDataFrame object, to determine which blocks (if any) contains each student and store the blocks name as attribute of the student data (do this for blocks, tracts, and counties)
student_location$block <- rep(NA, nrow(student_location))
student_location$block <- over(student_location, blocks)$GEOID10
head(student_location)

## Save as new dataframe
data1 <- as.data.frame(student_location)
dim(data1)
names(data1)
data1$block <- as.numeric(as.character(data1$block))


## Attach block centroid coordinates
data2 <- left_join(data1, centroid, by=c( "block"="geoid10"))
dim(data2)
head(data2)
student_block = data2 %>% select(StudentNumber, LAT, LON, block, geoid_num, blocklon, blocklat)
## save file to RDS
## see ETL_block.r
