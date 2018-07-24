## Set up workspace
library(rgdal)  # for working with spatial data frames
library(rgeos)  # for working with spatial data frames
library(ggmap)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mypath <- getwd()

## Load in data
dataPath <- file.path(dirname(mypath), "data","Boundaries_Current")
currentBounds <- readOGR(dsn = dataPath, 'Boundaries_CurrentPolygon')

plot(currentBounds)