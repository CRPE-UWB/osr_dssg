## Set up workspace
library(rgdal)  # for working with spatial data frames
library(rgeos)  # for working with spatial data frames

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mypath <- getwd()

## get nbhd level demographics from denver open data
source('open_data_functions.R')
nbhds <- GetOpenData('american_community_survey_nbrhd_2011_2015')

colnames(nbhds@data)

names_wanted <- c("NBHD_NAME",  # neighborhood name
                  "TTL_POPULA",  # total population
                  "PCT_HISPAN",  # percent hispanic
                  "PCT_WHITE",  # percent white
                  "PCT_BLACK",  # percent black
                  "PCT_NATIVE",  # percent native american
                  "PCT_ASIAN",  # percent asian
                  "PCT_HAWAII",  # percent hawaiian
                  "PCT_OTHERR",  # percent other race
                  "PCT_TWOORM",  # percent two or more races
                  "MED_HH_INC",  # median househol income
                  "HSGRAD_OR_",  # population 25+ with hs diploma or equivalent
                  "TTLPOP_25P",  # total population 25+
                  "AGE_5_TO_9", # for total students in age range 5-18
                  "AGE_10_TO_", # for total students in age range 5-18
                  "AGE_15_TO_", # ages 15-17
                  "TTLPOP_5PL",  # total population 5 plus years
                  "ONLY_ENGLI"  # total population 5 plus years with only english
                  )

# subset to the columns we need
nbhds_small <- nbhds[,names_wanted]

# get % population with diploma or equivalent
nbhds_small@data$PCT_HSDIPLOMA <- 100 * nbhds_small@data$HSGRAD_OR_ / nbhds_small@data$TTLPOP_5PL

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
                    "PCT_HISPAN", 
                    "PCT_WHITE",
                    "PCT_BLACK",
                    "PCT_ASIAN",
                    "PCT_NATIVE",
                    "PCT_OTHERR",
                    "PCT_TWOORM",
                    "PCT_HSDIPLOMA", 
                    "AGE_5_TO_17", 
                    "PCT_NON_ENGL")

finalspdf <- nbhds_small[,final_colnames]

simpler_colnames <- c("NBHD_NAME",
                    "MED_HH_INC", 
                    "PCT_HISPANIC", 
                    "PCT_WHITE",
                    "PCT_BLACK",
                    "PCT_ASIAN",
                    "PCT_NATIVE",
                    "PCT_OTHER_RACE",
                    "PCT_TWO_OR_MORE_RACES",
                    "PCT_HS_DIPLOMA", 
                    "AGE_5_TO_17", 
                    "PCT_NON_ENGL")

colnames(finalspdf@data) <- simpler_colnames

outputdir <- file.path(dirname(mypath), "data", "nbhd_dem_shapes")
writeOGR(finalspdf, outputdir, "nbhd_dem_shapes", driver="ESRI Shapefile")
