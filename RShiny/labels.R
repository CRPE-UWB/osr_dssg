# Make labels for tooltips / popups for RShiny app
#  ***** Requires running both get_data.R and color.R first!******

# Adding a custom html tooltip/popup for the racial distributions selection
shape_census@data$racial_dist_html <- mapply(
  
  # Inputs: 
  # neighborhood name (string),
  # percents of different races (numerics),
  # color palette generated from brewer.pal()
  function(nbhd, pct_hisp, pct_white, pct_black, pct_native, pct_asian){
    
    black_color <- colors_all_races[1]
    hispanic_color <- colors_all_races[2]
    white_color <- colors_all_races[3]
    other_color <- "gray"
    
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
      black_color, pct_black, 
      hispanic_color, pct_hisp, pct_black,
      white_color, pct_white, pct_hisp + pct_black,
      other_color, 100 - (pct_white + pct_hisp + pct_black), pct_white + pct_hisp + pct_black,
      black_color, pct_black, 
      hispanic_color, pct_hisp, 
      white_color, pct_white, 
      other_color, 100 - (pct_white + pct_hisp + pct_black)
    ) %>% lapply(htmltools::HTML)
  },
  
  shape_census@data$NBHD_NA,
  shape_census@data$PCT_HIS,
  shape_census@data$PCT_WHI,
  shape_census@data$PCT_BLA,
  shape_census@data$PCT_NAT,
  shape_census@data$PCT_ASI
  
)

# Construct tooltip/popup text for hovering over neighborhoods
nbhd_labels <- sprintf(
  "<b>%s</b><br/>
  No. program sessions = %i <br/>
  No. children 5-17 yrs old = %i <br/> 
  %% Hispanic students = %g%% <br/> 
  %% English student learners = %g%% <br/> 
  %% Students who use transportation = %g%% <br/> 
  %% Students with disability = %g%% ",
  shape_census@data$NBHD_NA,
  replace(shape_census@data$count, is.na(shape_census@data$count), 0), # show 0s not NAs
  shape_census@data$AGE_5_T, 
  shape_census@data$perc_hispanic_students, 
  shape_census@data$perc_nonenglish_students,
  shape_census@data$perc_with_transport_students, 
  shape_census@data$perc_disable_students
) %>% lapply(htmltools::HTML)

# Specify legend titles for different demographic maps
legend_titles_demographic <- list(MED_HH_ = "Median HH Income",
                                  PCT_LES = "Less Than <br> HS Degree",
                                  PCT_COL = "College <br> Graduates",
                                  PCT_HIS = "% Hispanic",
                                  PCT_BLA = "% Black",
                                  PCT_WHI = "% White",
                                  PCT_NON = "Lang. Besides <br>English",
                                  AGE_5_T = "5-17 Year Olds (#)",
                                  majority_race = "Most Common<br>Race/Ethnicity"
)