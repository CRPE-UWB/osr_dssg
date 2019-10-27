setwd('~/Desktop/OSR2019')

library(tidyverse)
library(tigris)
library(rgdal)
library(ggplot2)
library(readxl)
library(leaflet)
theme_set(theme_classic())

programs  <- read.csv("DATA/2019DataExport_CO.csv")
bg <- read.csv("DATA/Denver_Demographics_BG_2017.csv")
bg_shape <- readOGR("DATA/shape_census", "shape_census")
programs_details <- read_excel("DATA/Colorado export with dates and times revised.xlsx")


# distribution of program time
programs$date_start <- strptime(x = as.character(programs$session_date_start),
                                format = "%m/%d/%Y")
programs$date_end <- strptime(x = as.character(programs$session_date_end),
                                format = "%m/%d/%Y")
programs$time <- difftime(programs$date_end, programs$date_start, units = "days") + 1

time_frequency <- data.frame(table(programs$time))
colnames(time_frequency)[1] <- "Days"

g1 <- ggplot(programs, aes(time)) +
  geom_bar() + 
  labs(title="Time for summer programs", 
       caption="Source: Blueprint4Summer",
       x="Program time(day)")

# 8.2 
# distribution of programs over summer based on calendar date 
 ## change timestrip to year-month-date
programs_details$calendar_date <- as.POSIXct(programs_details$calendar_date)
format(programs_details$calendar_date,format='%Y-%m-%d')
 ## spot some mistakes in program_details$calendar_date year"0019", change all to "2019" for accuracy
library(lubridate)
year(programs_details$calendar_date) <- 2019
 ## make frequency table 
calendar_time_freq <- data.frame(table(programs_details$calendar_date))
 ## make bar plot
g2 <- ggplot(programs_details, aes(calendar_date)) +
  geom_bar() + 
  labs(title="Distribution of out-of-school programs", 
       subtitle = "All programs",
       caption="Source: Blueprint4Summer",
       x="Program time(day)")
 ## A more detailed summer plot? 

  ## distribution of programs over summer based on session start and end times
  ggplot(programs, aes(session_date_start, session_date_end)) +
    geom_point()

# 8.5 Check data first 
# Aggregate sessions to compare with the original program data
new_programs <- programs_details %>%
  group_by(session_id) %>%
  summarise()
 # Good news! We have the same n of rows! 

# Read in the old census data 
old_bg <- read.csv("DATA/Old_Demographics_bg.csv")
old_bg_shape <- readOGR("DATA/old_shape_census", "shape_census")
# Compare racial distribution on bg level 
# 2017
# black
colors5 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026') 
leaflet(bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, bg_shape@data$PCT_Afr)(bg_shape@data$PCT_Afr))
# white
colors5 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026') 
leaflet(bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, bg_shape@data$PCT_Wht)(bg_shape@data$PCT_Wht))
# hispanic
colors5 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026') 
leaflet(bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, bg_shape@data$PCT_Hsp)(bg_shape@data$PCT_Hsp))


# 2016
#black
leaflet(old_bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, old_bg_shape@data$PCT_Afr)(old_bg_shape@data$PCT_Afr))
#white
leaflet(old_bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, old_bg_shape@data$PCT_Wht)(old_bg_shape@data$PCT_Wht))
#hispanic
leaflet(old_bg_shape) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, 
              fillColor = ~colorFactor(colors5, old_bg_shape@data$PCT_Hsp)(old_bg_shape@data$PCT_Hsp))


# Aug 31
# check in the summer_sessions data, if the day length category is the same for multiple session days under the same program.

test <- summer_sessions %>%
  group_by(session_id, camp_zip) %>%   # mind that there are 101 obs that does not have a camp id 
  mutate(dl_categories = n_distinct(day_length)) # returns 1, 2, or 3  depends on how many unique categories are there

unique(test$dl_categories) # damn we have them all 
  
data.frame(table(test$dl_categories)) # well not very frequent for 2 and 3 
# your judgement call 
  
