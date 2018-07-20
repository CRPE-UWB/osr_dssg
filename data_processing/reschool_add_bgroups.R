# add block groups to reschool data

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

# get reschool data from database
reschool <- dbGetQuery(con,
                       "SELECT * FROM clean.reschool_summer_programs")
head(reschool)

# get block group polygons
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mypath <- getwd()
dataPath <- file.path(dirname(mypath), "data", "census_clean", "shape_census")
blockGroups <- readOGR(dsn = dataPath, 'shape_census')

# figure out which block group each program is in
coords <- data.frame(reschool$long, reschool$lat)
colnames(coords) <- c("long", "lat")
spatialReschool <- SpatialPoints(coords, proj4string = CRS(proj4string(blockGroups)))

nests <- gIntersects(spatialReschool, blockGroups, byid = TRUE)  # rows=bgroups, cols=programs
true_idxs <- which(nests==TRUE, arr.ind=TRUE)  # col1 = bgroup idx, col2 = program idx
bgroup_idxs  <- true_idxs[,1]
program_idxs <- true_idxs[,2]

reschool_bgs <- data.frame(coords[program_idxs,],
                      blockGroups$Id2[bgroup_idxs]
                      )
colnames(reschool_bgs) <- c("long", "lat", "bgroup_id2")

reschool_bgs$bgroup_id2 <- as.character(as.numeric(as.character(reschool_bgs$bgroup_id2))) # strip leading 0's
head(reschool_bgs)

reschool_full <- merge(reschool, reschool_bgs)
head(reschool_full)

# dbWriteTable(con,
#              c(schemaName, tableName),
#              value = data,
#              row.names = FALSE,
#              overwrite = TRUE  # overwrite an existing table
# )

# when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)
