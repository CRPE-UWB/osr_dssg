# Color settings for the Shiny app

library(RColorBrewer)

########################## Basic colors used all over ##########################

myyellow <- "#FFFF66"
mygreen <- brewer.pal(3, "Greens")[2]
myblue <- brewer.pal(3, "Blues")[2]
mypurple <- brewer.pal(3, "Purples")[2]

mygreen2 <- brewer.pal(3, "Greens")[1]
myblue2 <- brewer.pal(3, "Blues")[1]
mypurple2 <- brewer.pal(3, "Purples")[1]

mygreen3 <- brewer.pal(3, "Greens")[3]
myblue3 <- brewer.pal(3, "Blues")[3]
mypurple3 <- brewer.pal(3, "Purples")[3]

########################## Colors for dots in 'other resources' tab ##########################

other_resources_colors <- brewer.pal(6, "Dark2")
parks_color <- other_resources_colors[1]
libraries_color <- other_resources_colors[2]
rec_centers_color <- "#984ea3"  # darker purple than other_resources_colors[3]
playgrounds_color <- other_resources_colors[4]
museums_color <- other_resources_colors[5]
fields_color <- other_resources_colors[6]

###################### Color palettes for sidebar selections on demographics ########################

# Palettes for non-racial demographics (grey)
bins_income <- c(0, 25000, 50000, 75000, 100000, max(shape_census@data$MED_HH_))
pal_income <- colorBin("Greys", domain = shape_census@data$MED_HH_, bins = bins_income)
pal_edu <- colorBin("Greys", domain = shape_census@data$PCT_LES, bins = 5)
pal_edu2 <- colorBin("Greys", domain = shape_census@data$PCT_COL, bins = 5)
# bins_language <- c(0, 15, 30, 45, 60, 75)
pal_language <- colorBin("Greys", domain = shape_census@data$PCT_NON, bins = 4)
bins_age <- c(0, 500, 1000, 1500, 2000, max(shape_census@data$AGE_5_T))
pal_age <- colorBin("Greys", domain = shape_census@data$AGE_5_T, bins = bins_age)
# pal_age <- colorQuantile("Greys", domain = shape_census@data$AGE_5_T, n = 5)

pal_disabled <- colorBin("Greys", domain = shape_census@data$perc_disable_students, bins = 5)
pal_el <- colorBin("Greys", domain = shape_census@data$perc_nonenglish_students, bins = 5)

# Palettes for racial demographics (colored)
pal_hispanic <- colorBin("Greens", domain = shape_census@data$PCT_HIS, bins = 5)
pal_black <- colorBin("Blues", domain = shape_census@data$PCT_BLA, bins = 5)
pal_white <- colorBin("Purples", domain = shape_census@data$PCT_WHI, bins = 5)
pal_hispanic_student <- colorBin("Greens", domain = shape_census@data$perc_hispanic_students, bins = 5)
pal_black_student <- colorBin("Blues", domain = shape_census@data$perc_black_students, bins = 5)
pal_white_student <- colorBin("Purples", domain = shape_census@data$perc_white_students, bins = 5)

# Palette for racial distribution selection (three colors)
colors_all_races <- brewer.pal(3, "Paired")[c(1,3,2)]
pal_all_races <- colorFactor(colors_all_races, 
                             domain = shape_census@data$majority_race)