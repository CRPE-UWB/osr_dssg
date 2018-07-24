#########################Connecting to the sql database#####################################################
require("RPostgreSQL")
library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(tigris)

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

## Getting the required tables from the sql database ##
reschool_summer_program = dbGetQuery(con, "SELECT * from shiny.summer_programs")
aggregate_session_nbhds = dbGetQuery(con, "SELECT * from shiny.aggregate_programs_nbhd")
aggregate_dps_student_nbhds = dbGetQuery(con, "SELECT * from shiny.dps_student_aggregate_nbhd")

## Disconnect from database and unload driver ##
dbDisconnect(con) 
dbUnloadDriver(drv)

#######################Getting the shape file to plot the bock groups on the map##############################
shape_census <- readOGR(dsn = "/Users/kelliemacphee/Desktop/dssg2018/GITHUB_osr_dssg2018/data/nbhd_dem_shapes", 
                        layer = "nbhd_dem_shapes")

#Joining the 'number of sessions' information with the census shape file
shape_census <- geo_join(shape_census, aggregate_session_nbhds, "NBHD_NA", "nbhd_name", how = "left")

#Joining the aggregate dps students information to the census shape file
shape_census <- geo_join(shape_census, aggregate_dps_student_nbhds, "NBHD_NA", "nbhd_name", how = "left")
 



#Creating filter variables distinct zipcode, minimum cost, maximum cost and the type of the program
#Defining the variables to be used in the sidebar panel
neighborhoods_reshoolprograms = unique(reschool_summer_program$nbhd_name)
minprice_reschoolprograms = min(reschool_summer_program$session_cost)
maxprice_reschoolprograms = max(reschool_summer_program$session_cost)

#Defining variables for choosing demographic information
demographic_filters = c("Median Income", "Percent below poverty level")


