# TEST 0: how are blockGroupsCentroids ordered? same as blockGroups?
# check: is the given centroid actually a centroid for the polygon?
i <- 5
plot(blockGroups[i,], border = "blue", col = "grey", main = "Block Group")
plot(blockGroupCentroids[i,], col = "dark blue", pch = 1, cex = 0.2, add = TRUE)

# TEST 1: Do block groups, centroids, and neighborhoods line up well?
plot(blockGroups, border = "blue", col = "grey", main = "Block Groups + Neighborhoods")
plot(blockGroupCentroids, col = "dark blue", pch = 1, cex = 0.2, add = TRUE)
plot(nbhds, border = "red", add = TRUE)

# TEST 2: are there any centroids which intersect multiple/no nbhds?
which(colSums(nests)!=1) # if 0 , good to go

# TEST 3: are block groups actually lining up in the right neighborhood?
myNbhdName <- "DIA"  # top right corner nbhd
plot(nbhdsProj, 
     border = "blue", main = "Nbhd + Block Groups")
bgIds <- dfFinal[ (dfFinal$nbhd_name==myNbhdName), 3]
print( paste( length(bgIds), 'block groups'))
for (id in bgIds) {
  plot(blockGroups[(blockGroups@data$Id2==id), ], 
       col = "red", border = "black", add = TRUE)
}

myNbhdName <- "Regis"  # top left corner nbhd
plot(nbhds, 
     border = "blue", main = "Nbhd + Block Groups")
bgIds <- dfFinal[ (dfFinal$nbhd_name==myNbhdName), 3]
print( paste( length(bgIds), 'block groups'))
for (id in bgIds) {
  plot(blockGroups[(blockGroups@data$Id2==id), ], 
       col = "red", border = "black", add = TRUE)
}

# ## Q: are statistical nbhds and census demographics nbhds the same?
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