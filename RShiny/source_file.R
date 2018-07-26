# Source file to generate data for plotting in RShiny app.
#
# Note: all file paths are relative to LOCATION OF THIS FILE
#
#########################Connecting to the sql database#####################################################
library(dplyr)
library(rgdal)
library(tigris)
library(rgeos)
require("RPostgreSQL")
library(RColorBrewer)

mypath <- "/Users/josephabbate/Documents/Experiences/Applications/UWashington/Project"

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# load credentials for the connection: dbname, host, port, user, password
# looks for cred.txt in parent dir to cloned github repo
source('../../cred.txt')

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = dbname,
                 host = host, port = port,
                 user = user, password = password)

#########################Getting the required tables from the sql database####################################
reschool_summer_program = dbGetQuery(con, "SELECT * from shiny.summer_programs")
aggregate_session_nbhds = dbGetQuery(con, "SELECT * from shiny.aggregate_programs_nbhd")
aggregate_dps_student_nbhds = dbGetQuery(con, "SELECT * from shiny.dps_student_aggregate_nbhd")
fields = dbGetQuery(con, "SELECT * from shiny.fields")
museums = dbGetQuery(con, "SELECT * from shiny.museums")
libraries = dbGetQuery(con, "SELECT * from shiny.libraries")
playgrounds = dbGetQuery(con, "SELECT * from shiny.playgrounds")
rec_centers = dbGetQuery(con, "SELECT * from shiny.rec_centers")
parks = dbGetQuery(con, "SELECT * from shiny.parks")
all_neighbourhoods = dbGetQuery(con, "SELECT * from clean.blockgroup_nbhds")

#######################Getting the shape file to plot the bock groups on the map##############################

# shape_census <- readOGR(dsn = "C:/Users/Sreekanth/Desktop/osr_dssg2018-1/data/nbhd_dem_shapes", 
 #                        layer = "nbhd_dem_shapes")
#shape_census <- readOGR(dsn = "/Users/kelliemacphee/Desktop/dssg2018/GITHUB_osr_dssg2018/data/nbhd_dem_shapes",
#                        layer = "nbhd_dem_shapes")

shape_census <- readOGR(dsn = "../data/nbhd_dem_shapes", layer = "nbhd_dem_shapes")


# Joining the 'number of sessions' information with the census shape file
shape_census <- geo_join(shape_census, aggregate_session_nbhds, "NBHD_NA", "nbhd_name", how = "left")

# Joining the aggregate dps students information to the census shape file
shape_census <- geo_join(shape_census, aggregate_dps_student_nbhds, "NBHD_NA", "nbhd_name", how = "left")

#Creating filter variables distinct zipcode, minimum cost, maximum cost and the type of the program
#Defining the variables to be used in the sidebar panel
neighborhoods_reshoolprograms = unique(reschool_summer_program$nbhd_name)
minprice_reschoolprograms = min(reschool_summer_program$session_cost)
maxprice_reschoolprograms = max(reschool_summer_program$session_cost)

#Creating variables for the second tab 'other out-of-school resources'
neighborhoods_other = unique(all_neighbourhoods$nbhd_name)

#Defining variables for choosing demographic information
demographic_filters = c("Median Income", "Percent below poverty level")

# Creating majority race variables for each neighborhood
shape_census@data$majority_race <- max.col(as.matrix(
          shape_census@data[ ,c("PCT_HIS", "PCT_WHI", "PCT_BLA","PCT_NAT","PCT_ASI")]
                     ))
shape_census@data$majority_race <- gsub(1, "Hispanic", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(2, "White", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(3, "Black", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(4, "Native", shape_census@data$majority_race)
shape_census@data$majority_race <- gsub(5, "Asian", shape_census@data$majority_race)

# Adding a custom html label for the racial distributions
shape_census@data$racial_dist_html <- mapply(
  
  # Inputs: 
  # neighborhood name (string),
  # percents of different races (numerics),
  # color palette generated from brewer.pal()
  function(nbhd, pct_hisp, pct_white, pct_black, pct_native, pct_asian){
    
    pal <- brewer.pal(4, "Set2")  # color palette - match to the server.R code
    color1 <- pal[1]
    color2 <- pal[2]
    color3 <- pal[3]
    color4 <- pal[4]
    
    sprintf(
      "<div style='font-size:12px;width:180px;float:left'>
            <span style='font-size:16 px;font-weight:bold'>%s</span><br/>
            <div style='width:100%%'>
              <span style='background:%s;width:%s%%;position:absolute;left:0'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
              <br/>
              <span style='color:%s;float:left'>%.2f%% Black</span><br/>
              <span style='color:%s;float:left'>%.2f%% Hispanic</span><br/>
              <span style='color:%s;float:left'>%.2f%% White</span><br/>
              <span style='color:%s;float:left'>%.2f%% Other</span><br clear='all'/>
            </div>
        </div>",
      nbhd,
      color1, pct_black, 
      color2, pct_hisp, pct_black,
      color3, pct_white, pct_hisp + pct_black,
      color4, 100 - (pct_white + pct_hisp + pct_black), pct_white + pct_hisp + pct_black,
      color1, pct_black, 
      color2, pct_hisp, 
      color3, pct_white, 
      color4, 100 - (pct_white + pct_hisp + pct_black)
    ) %>% lapply(htmltools::HTML)
  },
  
  shape_census@data$NBHD_NA,
  shape_census@data$PCT_HIS,
  shape_census@data$PCT_WHI,
  shape_census@data$PCT_BLA,
  shape_census@data$PCT_NAT,
  shape_census@data$PCT_ASI
  
)

# when you're done, close the connection and unload the driver 
dbDisconnect(con) 
dbUnloadDriver(drv)