# add block group ids to museums dataset

library(sp)
library(rgeos)
library(rgdal)

require("RPostgreSQL")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# load credentials for the connection:
# dbname, host, port, user, password
source('/Users/kelliemacphee/Desktop/dssg2018/cred.txt')

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = dbname,
                 host = host, port = port,
                 user = user, password = password)

# get museum data from database
museums <- dbGetQuery(con,
                       "SELECT * FROM clean.museums")
head(museums)

# get block group polygons
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mypath <- getwd()
dataPath <- file.path(dirname(mypath), "data", "census_clean", "shape_census")
blockGroups <- readOGR(dsn = dataPath, 'shape_census')

# figure out which block group each program is in
coords <- data.frame(museums$long, museums$lat)
colnames(coords) <- c("long", "lat")
spMuseums <- SpatialPoints(coords, proj4string = CRS(proj4string(blockGroups)))

nests <- gIntersects(spMuseums, blockGroups, byid = TRUE)  # rows=bgroups, cols=programs
true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = bgroup idx, col2 = program idx
bgroup_idxs  <- true_idxs[,1]
museum_idxs <- true_idxs[,2]

museum_bgs <- data.frame(coords[museum_idxs,],
                           blockGroups$Id2[bgroup_idxs]
)
colnames(museum_bgs) <- c("long", "lat", "bgroup_id2")

museum_bgs$bgroup_id2 <- as.character(as.numeric(as.character(museum_bgs$bgroup_id2))) # strip leading 0's
head(museum_bgs)

museum_full <- merge(museums, museum_bgs)
head(museum_full)

dbWriteTable(con,
             c("clean", "museums"),
             value = museum_full,
             row.names = FALSE,
             overwrite = TRUE  # overwrite an existing table
)

# when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)
