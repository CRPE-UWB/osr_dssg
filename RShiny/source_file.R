#########################Connecting to the sql database#####################################################
require("RPostgreSQL")
library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(tigris)

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dssg2018uw",
                 host = "localhost", port = 22,
                 user = "dssg2018uw", password = "dssg2018UW")


#########################Getting the required tables from the sql database####################################
reschool_summer_program = dbGetQuery(con, "SELECT * from shiny.summer_programs")
aggregate_session_nbhds = dbGetQuery(con, "SELECT * from shiny.aggregate_programs_nbhd")
aggregate_dps_student_nbhds = dbGetQuery(con, "SELECT * from shiny.dps_student_aggregate_nbhd")

#######################Getting the shape file to plot the bock groups on the map##############################
shape_census <- readOGR(dsn = "C:/Users/Sreekanth/Desktop/DSSG Project/census_nbhd_dem_shapes", layer = "nbhd_dem_shapes")
#Getting the percentages for the two columns
shape_census@data$PCT_HSD = shape_census@data$PCT_HSD * 100
shape_census@data$PCT_NON = shape_census@data$PCT_NON * 100

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

