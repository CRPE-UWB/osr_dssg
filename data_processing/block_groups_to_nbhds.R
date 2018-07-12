# This file converts demographics shapefile by census block groups
# to a demographics shapefile by NEIGHBORHOODS
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

# ## are statistical nbhds and census demographics nbhds the same?
# nbhds_dem <- GetOpenData('census_neighborhood_demographics_2010')
# nbhds@proj4string
# nbhds_dem@proj4string
# plot(nbhds, border = "red", col = "grey")
# plot(nbhds_dem, border = "blue", add = TRUE)
# 
# small_dem <- nbhds_dem@data[,1:2]
# colnames(small_dem)[2] <- "NBHD_NAME"
# merged <- merge(nbhds, small_dem, by = c("NBHD_ID", "NBHD_NAME"))

# answer: yes they are the same. 
# (expect that NBHD_NAME in one is NBRHD_NAME in the other)
# we will just use the statistical nbhds, for simplicity

## Get centroids of block groups
blockGroupCentroids <- gCentroid(blockGroups, byid=TRUE)

# check if any centroids are outside of their block group
test <- gIntersects(blockGroupCentroids, blockGroups, byid = TRUE)
badCentroids <- which(colSums(test)!=1)

# fix it by picking another random point inside the block group
altCentroids <- gPointOnSurface(blockGroups, byid = TRUE)
coordinates <- as.data.frame(blockGroupCentroids)
for (i in badCentroids) {
  print(i)
  coordinates[i,] <- altCentroids[i]@coords[]
}
blockGroupCentroids <- SpatialPoints(
  coordinates, proj4string = CRS(proj4string(blockGroupCentroids)))

## Check that block groups, centroids, and neighborhoods line up well
plot(blockGroups, border = "blue", col = "grey", main = "Block Groups + Neighborhoods")
plot(blockGroupCentroids, col = "dark blue", pch = 1, cex = 0.2, add = TRUE)
plot(nbhds, border = "red", add = TRUE)

## Nest block groups inside neighborhoods using centroid locations
nbhdsProj <- spTransform(nbhds, proj4string(blockGroups)) # make sure CRS's are the same

# in matrix below, rows = nbhds, cols = centroids
nests <- gIntersects(blockGroupCentroids, nbhdsProj, byid = TRUE)
nests_small <- gIntersects(blockGroupCentroids, nbhdsProj, byid = TRUE, 
                           returnDense = FALSE)

# are there any centroids which intersect multiple/no nbhds?
nbhdsWrong <- which(colSums(nests)!=1) # 0 , good to go

## Output dataframe with rows (block group id, nbhd id, nbhd name)



# first get (centroid, neighborhood)

dfFinal <- blockGroups@data$Id2

# how are blockGroupsCentroids ordered? same as blockGroups?
i <- 10
plot(blockGroups[i,], border = "blue", col = "grey", main = "Block Group")
plot(blockGroupCentroids[i,], col = "dark blue", pch = 1, cex = 0.2, add = TRUE)
# yup, same ordering. phew.

# combine centroids and block group ids
df1 <- data.frame(blockGroups@data$Id2, blockGroupCentroids, unlist(nests_small))
colnames(df1) <- c('bgroup_id2', 'x', 'y', 'nbhd_id')
head(df1)

df2 <- merge(df1, nbhds@data, by.x = 'nbhd_id', by.y = 'NBHD_ID')
dfFinal <- df2[,c(1,2,5)]
colnames(dfFinal)[3] <- 'nbhd_name'
head(dfFinal)

# End by uploading dfFinal to the database!