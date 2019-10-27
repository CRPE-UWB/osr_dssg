# Source file to read data for plotting in RShiny app.
# Note: all file paths are relative to LOCATION OF THIS FILE

library(dplyr)
library(rgdal)
library(rgeos)
library(tigris)

# Load all files in the Shiny data folder at once:
for (filename in list.files(path="shiny_data")){
  load(file.path("shiny_data",filename))
}

# # Load museums separately
# osr_folder <- dirname(getwd())
# data_folder <- file.path(osr_folder, 'data_2018')
# museums = read.csv(file.path(data_folder, 'museums.csv') )

###############################################################
###############################################################
###############################################################
# Misc. other things

# Create filter variables for sidebar in 'B4S programs' tab (Reschool Data)
neighborhoods_list <- sort(unique(as.character(shape_census$NBHD_NA)))
minprice_reschoolprograms = min(reschool_summer_program$cost_per_hour, na.rm=TRUE)
maxprice_reschoolprograms = ceiling(max(reschool_summer_program$cost_per_hour, na.rm=TRUE))
maxlength_reschoolprograms = ceiling(max(reschool_summer_program$length, na.rm=TRUE))

good_idxs <- !(is.na(reschool_summer_program$session_date_end) | is.na(reschool_summer_program$session_date_start) )
maxdays_reschoolprograms = max(reschool_summer_program$total_days[good_idxs], na.rm=TRUE)

# Create filter variables for sidebar in 'other resources' tab (Denver Open Data)
neighborhoods_other = shape_census@data$NBHD_NA #unique(all_neighbourhoods$nbhd_name)
demographic_filters = c("Median Income", "Percent below poverty level")
