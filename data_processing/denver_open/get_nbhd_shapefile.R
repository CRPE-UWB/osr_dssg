###
# Old file for processing neighborhood demog data
# New version contained in open_data_functions and
# process_open_data_resources for streamlining
###

## Set up workspace
library(rgdal)  # for working with spatial data frames
library(rgeos)  # for working with spatial data frames
library(leaflet)  # for observational mapping

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only for if running rstudio
#mypath <- getwd()

## get nbhd level demographics from denver open data
## data is american community survey, 2011-2015
source('open_data_functions.R')
nbhds <- GetOpenData('american_community_survey_nbrhd_2011_2015')

colnames(nbhds@data)

names_wanted <- c("NBHD_NAME",  # neighborhood name
                  "TTL_POPULA",  # total population - used for racial demographics
                  "PCT_HISPAN",  # percent hispanic
                  "PCT_WHITE",  # percent white
                  "PCT_BLACK",  # percent black
                  "PCT_NATIVE",  # percent native american
                  "PCT_ASIAN",  # percent asian
                  "PCT_HAWAII",  # percent hawaiian
                  "PCT_OTHERR",  # percent other race
                  "PCT_TWOORM",  # percent two or more races
                  "MED_HH_INC",  # median househol income
                  "LESS_THAN_",  # population 25+ with les than hs diploma
                  "HSGRAD_OR_",  # population 25+ with hs diploma or equivalent
                  "SOMECOLLEG",  # population 25+ with some college
                  "BACHELORS_",  # population 25+ with bachelors degree
                  "TTLPOP_25P",  # total population 25+ - used for education data
                  "AGE_5_TO_9", # for total students in age range 5-18
                  "AGE_10_TO_", # for total students in age range 5-18
                  "AGE_15_TO_", # ages 15-17
                  "TTLPOP_5PL",  # total population 5 plus years - used for language data
                  "ONLY_ENGLI"  # total population 5 plus years with only english
                  )

# subset to the columns we need
nbhds_small <- nbhds[,names_wanted]

# DECIDING WHAT EDUCATION DATA TO USE

# colnames <- c("LESS_THAN_")
# colnames <- c("LESS_THAN_", "HSGRAD_OR_")
# colnames <- c("SOMECOLLEG", "BACHELORS_")
colnames <- c("BACHELORS_")

spdf <- nbhds_small[,colnames]
spdf@data$educ <- 100 * rowSums(spdf@data)/ nbhds_small@data[,"TTLPOP_25P"]
spdf <- spdf[,"educ"]
vals <- unlist(spdf@data)

mypal <- colorBin("Blues", domain = vals, bins = 5)
opacity <- 1.0

leaflet()  %>% 
  setView(lng = -104.901531, lat = 39.722043, zoom = 11) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = spdf,
              fillColor = ~mypal(vals),
              fillOpacity = opacity,
              weight = 0.5
              ) %>%
  addLegend(pal = mypal,
            values = vals,
            opacity = opacity,
            position = "bottomright")

## DECIDING TO KEEP: LESS THAN HS DIPLOMA, and BACHELORS OR ABOVE
nbhds_small@data$PCT_LESS_HS <- 100 * nbhds_small@data$LESS_THAN_ / nbhds_small@data$TTLPOP_25P
nbhds_small@data$PCT_COLL_GRAD <- 100 * nbhds_small@data$BACHELORS_ / nbhds_small@data$TTLPOP_25P

# get total students in age range 5-17
nbhds_small@data$AGE_5_TO_17 <- nbhds_small@data$AGE_5_TO_9 + nbhds_small@data$AGE_10_TO_ +
                                nbhds_small@data$AGE_15_TO_

# get percent non english speakers
nbhds_small@data$PCT_NON_ENGL <- 100 * (nbhds_small@data$TTLPOP_5PL - 
                                    nbhds_small@data$ONLY_ENGLI) / nbhds_small@data$TTLPOP_5PL

# combine small percentage native races: native, hawaiian
nbhds_small@data$PCT_NATIVE <- nbhds_small@data$PCT_NATIVE + nbhds_small@data$PCT_HAWAII

colnames(nbhds_small@data)

final_colnames <- c("NBHD_NAME",
                    "MED_HH_INC", 
                    "TTL_POPULA",
                    "PCT_HISPAN", 
                    "PCT_WHITE",
                    "PCT_BLACK",
                    "PCT_ASIAN",
                    "PCT_NATIVE",
                    "PCT_OTHERR",
                    "PCT_TWOORM",
                    "TTLPOP_25P",
                    "PCT_LESS_HS", 
                    "PCT_COLL_GRAD",
                    "AGE_5_TO_17",
                    "TTLPOP_5PL",
                    "PCT_NON_ENGL")

finalspdf <- nbhds_small[,final_colnames]

simpler_colnames <- c("NBHD_NAME",
                      "MED_HH_INC", 
                      "POP_RACE",
                      "PCT_HISPANIC", 
                      "PCT_WHITE",
                      "PCT_BLACK",
                      "PCT_ASIAN",
                      "PCT_NATIVE",
                      "PCT_OTHER_RACE",
                      "PCT_TWO_OR_MORE_RACES",
                      "POP_EDUC",
                      "PCT_LESS_HS", 
                      "PCT_COLL_GRAD", 
                      "AGE_5_TO_17",
                      "POP_LANG",
                      "PCT_NON_ENGL")

colnames(finalspdf@data) <- simpler_colnames

outputdir <- "nbhd_dem_shapes"
writeOGR(finalspdf, outputdir, "nbhd_dem_shapes", driver="ESRI Shapefile")
