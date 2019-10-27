# Function to convert lat / long to Denver block group
#   input: dataframe with lat, long columns
#   ouput: new dataframe with block groups column appended

coords_to_blockgroups <- function(df, shape_census_location) {
  # get block group polygons
  blockGroups <- readOGR(dsn = shape_census_location, 'shape_census')
  
  # get the lat / long coordinates to code as spatial points
  coords <- data.frame(df$long, df$lat)
  colnames(coords) <- c("long", "lat")
  spdf <- SpatialPoints(coords, proj4string = CRS(proj4string(blockGroups)))
  
  # figure out which block group each lat / long is in
  nests <- gIntersects(spdf, blockGroups, byid = TRUE)  # rows = bgroups, cols = df entries
  true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = bgroup idx, col2 = df entries
  bgroup_idxs  <- true_idxs[,1]
  df_idxs <- true_idxs[,2]
  
  # make a dataframe with the coordinates and corresponding block groups
  df_bgroups <- data.frame(coords[df_idxs,], blockGroups$Id2[bgroup_idxs])
  colnames(df_bgroups) <- c("long", "lat", "bgroup_id2")
  
  # strip leading 0's in block groups numbers
  df_bgroups$bgroup_id2 <- as.character(as.numeric(as.character(df_bgroups$bgroup_id2))) 
  
  df_out <- merge(df, df_bgroups)
  return(df_out)
}