# Make labels for tooltips / popups for RShiny app
#  ***** Requires running both get_data.R and color.R first!******

# Wrap text - prevents labels from being gigantic
wrap_text <- function(s, offset) {
  gsub('(.{1,50})(\\s|$)', '\\1<br/>',s)
}

# Function to make program popup text
make_program_popups <- function(program_data) {
  sprintf(
    "<b>%s</b><br/> 
    %s <br/> 
    <i>%s</i><br/>
    $%i per session<br/>
    Starts: %s, Ends: %s <br/>  
    Special needs = %s,  
    Scholarships = %s <br/>",
    wrap_text(paste("Program: ",program_data$session_name)), 
    wrap_text(paste("Organization: ",program_data$camp_name)), 
    wrap_text(paste("Description: ",program_data$session_short_description)),
    program_data$session_cost,
    program_data$session_date_start, 
    program_data$session_date_end,
    program_data$has_special_needs_offerings, 
    program_data$has_scholarships
  ) %>% lapply(htmltools::HTML)
}

############ Adding a custom html tooltip/popup for the racial distributions selection ############
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

###################### Construct tooltip/popup text for hovering over neighborhoods ######################

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

nbhd_labels_student <- sprintf(
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

######################### Specify legend titles for different demographic maps ########################

legend_titles_demographic <- list(MED_HH_ = "Median HH Income",
                                  PCT_LES = "Less Than <br> HS Degree",
                                  PCT_COL = "College <br> Graduates",
                                  PCT_HIS = "% Hispanic",
                                  PCT_BLA = "% Black",
                                  PCT_WHI = "% White",
                                  PCT_NON = "Lang. Besides <br>English",
                                  AGE_5_T = "5-17 Year Olds (#)",
                                  majority_race = "Most Common<br>Race/Ethnicity",
                                  "perc_disable_students" = "Disabled",
                                  "perc_hispanic_students" = "Hispanic Students (%)",
                                  "perc_nonenglish_students" = "EL"
)

#### TESTING - UPDATE THIS TEXT (JOE)

# Options to show in the UI for filtering by demographics
# (feel free to change these for better appearances)
demog_names <- list("None selected",
                    "Number of 5-17 year olds",
                    "Median household income ($)", 
                    "Less than high school degree (% over 25 years)",
                    "College graduates (% over 25 years)",
                    "Language other than English spoken (%)",
                    # HTML("Language other than English spoken (%)
                    #      <br><br>
                    #      <i>Race/Ethnicity Variables</i>"
                    #      ),
                    "Hispanic population (%)", 
                    "Black population (%)",
                    "White population (%)",
                    "Most common + breakdown"
)

demog_student_names <- list("None selected",
                            "Disabled student population (%)",
                            "Hispanic student population (%)",
                            "EL student population (%)")

# Internal values for demographic filtering options (correspond to demog_names above)
# (don't change these, it will make your life difficult)
demog_values <- list("none", 
                     "AGE_5_T",
                     "MED_HH_", 
                     "PCT_LES",
                     "PCT_COL",
                     "PCT_NON",
                     "PCT_HIS", 
                     "PCT_BLA",
                     "PCT_WHI",
                     "majority_race"
)

demog_student_values <- list("none",
                             "perc_disable_students",
                             "perc_hispanic_students",
                             "perc_nonenglish_students")

labFormatAge = function(type, cuts, p) {
  n = length(cuts)
  paste0(round(cuts[-n]), " &ndash; ", round(cuts[-1]))
}

lab_format_list <- list(labFormatAge,
                        labelFormat(prefix = "$ "),
                        labFormat = labelFormat(suffix = " %"),
                        labFormat = labelFormat(suffix = " %"),
                        labFormat = labelFormat(suffix = " %"),
                        labFormat = labelFormat(suffix = " %"),
                        labFormat = labelFormat(suffix = " %"),
                        labFormat = labelFormat(suffix = " %"),
                        NULL)

pal_list <- list(pal_age,
                 pal_income,
                 pal_edu,
                 pal_edu2,
                 pal_language,
                 pal_hispanic,
                 pal_black,
                 pal_white,
                 pal_all_races
)
names(pal_list) <- demog_values[demog_values!="none"]
names(lab_format_list) <- demog_values[demog_values!="none"]

