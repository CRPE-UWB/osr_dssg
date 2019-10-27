# This function spits out a table with columns 
# nbhd_id | nbhd_name | bgroup_id2
# in order to do merging between neighborhoods and block groups

library(rgdal)
library(rgeos)

make_block_neighborhood_table <- function() {
  block_shapefile <- readOGR(dsn="data/census_block_groups", "shape_census")
  block_shapefile <- block_shapefile[,"Id2"]
  
  nbhd_shapefile <- readOGR("data_processing/raw_data/statistical_neighborhoods", "statistical_neighborhoods")
  nbhd_shapefile <- nbhd_shapefile[,c("NBHD_ID","NBHD_NAME")]
  
  #get block group centroids
  blockGroupCentroids <- gCentroid(block_shapefile, byid=TRUE)
  
  # If any centroids are outside of their block group, replace by 
  # a random point inside block group (e.g. if the block group is L-shaped)
  test <- gIntersects(blockGroupCentroids, block_shapefile, byid = TRUE)
  badCentroids <- which(colSums(test)!=1)
  altCentroids <- gPointOnSurface(block_shapefile, byid = TRUE)
  
  coordinates <- as.data.frame(blockGroupCentroids)
  for (i in badCentroids) {
  coordinates[i,] <- altCentroids[i]@coords[]
  }
  blockGroupCentroids <- SpatialPoints(
  coordinates, proj4string = CRS(proj4string(blockGroupCentroids)))
  
  ## Nest block groups inside neighborhoods using centroid locations
  nbhdsProj <- spTransform(nbhd_shapefile, proj4string(block_shapefile))  # make sure CRS's are the same
  nests <- gIntersects(blockGroupCentroids, nbhdsProj, byid = TRUE)  # rows=nbhds, cols=centroids
  true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = nbhd idx, col2 = bgroup idx
  nbhd_idxs <- true_idxs[,1]
  bgroup_idxs <- true_idxs[,2]
  
  dfFinal <- data.frame(nbhd_shapefile@data$NBHD_ID[nbhd_idxs],
                        nbhd_shapefile@data$NBHD_NAME[nbhd_idxs],
                        block_shapefile@data$Id2[bgroup_idxs]
  )
  colnames(dfFinal) <- c("nbhd_id", "nbhd_name", "bgroup_id2")
  dfFinal$nbhd_id <- as.integer(dfFinal$nbhd_id)
  dfFinal$nbhd_name <- as.character(dfFinal$nbhd_name)
  dfFinal$bgroup_id2 <- as.character(as.numeric(as.character(dfFinal$bgroup_id2))) # strip leading 0's
  
  return(dfFinal)
}

write.csv(make_block_neighborhood_table(), "block_neighborhood.csv")
