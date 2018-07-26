# This file creates a dataframe with columns
#     block group id | nbhd id | nbhd name
# for Denver block groups (from our ACS data)
# and Denver neighborhoods (from Denver Open Data).

# Author: Kellie

## Set up workspace
library(rgdal)  # for working with spatial data frames
library(rgeos)  # for working with spatial data frames
library(ggmap)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mypath <- getwd()

source('open_data_functions.R')  # functions for loading denver open data

## Load in data
dataPath <- file.path(dirname(mypath), "data", "census_clean", "shape_census")
blockGroups <- readOGR(dsn = dataPath, 'shape_census')
nbhds <- GetOpenData('statistical_neighborhoods')

## Get centroids of block groups
blockGroupCentroids <- gCentroid(blockGroups, byid=TRUE)

# If any centroids are outside of their block group, replace by 
# a random point inside block group (e.g. if the block group is L-shaped)
test <- gIntersects(blockGroupCentroids, blockGroups, byid = TRUE)
badCentroids <- which(colSums(test)!=1)
altCentroids <- gPointOnSurface(blockGroups, byid = TRUE)

coordinates <- as.data.frame(blockGroupCentroids)
for (i in badCentroids) {
  print(i)
  coordinates[i,] <- altCentroids[i]@coords[]
}
blockGroupCentroids <- SpatialPoints(
  coordinates, proj4string = CRS(proj4string(blockGroupCentroids)))

## Nest block groups inside neighborhoods using centroid locations
nbhdsProj <- spTransform(nbhds, proj4string(blockGroups))  # make sure CRS's are the same
nests <- gIntersects(blockGroupCentroids, nbhdsProj, byid = TRUE)  # rows=nbhds, cols=centroids
true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = nbhd idx, col2 = bgroup idx
nbhd_idxs <- true_idxs[,1]
bgroup_idxs <- true_idxs[,2]

dfFinal <- data.frame(nbhdsProj@data$NBHD_ID[nbhd_idxs],
                   nbhdsProj@data$NBHD_NAME[nbhd_idxs],
                   blockGroups@data$Id2[bgroup_idxs]
                   )
colnames(dfFinal) <- c("nbhd_id", "nbhd_name", "bgroup_id2")
dfFinal$nbhd_id <- as.integer(dfFinal$nbhd_id)
dfFinal$nbhd_name <- as.character(dfFinal$nbhd_name)
dfFinal$bgroup_id2 <- as.character(as.numeric(as.character(dfFinal$bgroup_id2))) # strip leading 0's
head(dfFinal)

# Uploading dfFinal to the database
#source( file.path(dirname(mypath), "update_rds.R") )
#update_rds(dfFinal, "clean", "blockgroup_nbhds")