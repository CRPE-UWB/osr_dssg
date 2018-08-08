# Make labels for tooltips / popups for RShiny app
#  ***** Requires running both get_data.R and color.R first!******

# Wrap text - prevents labels from being gigantic
wrap_text <- function(s, offset) {
  gsub('(.{1,50})(\\s|$)', '\\1<br/>',s)
}

# Function to make ReSchool program popup text
make_program_popups <- function(program_data) {
  sprintf(
    "<p>
    <b>%s</b> 
    %s
    </p><p>
    $%i per session<br>
    Starts: %s, Ends: %s
    </p><p>
    <i>%s</i>
    Special needs = %s,  
    Scholarships = %s
    </p>",
    wrap_text(paste("Program: ",program_data$session_name)), 
    wrap_text(paste("Organization: ",program_data$camp_name)), 
    program_data$session_cost,
    program_data$session_date_start, 
    program_data$session_date_end,
    wrap_text(paste("Description: ",program_data$session_short_description)),
    ifelse(program_data$has_special_needs_offerings, "YES", "NO"), 
    ifelse(program_data$has_scholarships, "YES", "NO")
  ) %>% lapply(htmltools::HTML)
}

############ Adding a custom html tooltip/popup for the racial distributions selection ############
shape_census@data$racial_dist_html <- mapply(
  
  # Inputs: 
  # neighborhood name (string),
  # percents of different races (numerics),
  # color palette generated from brewer.pal()
  function(nbhd, pct_hisp, pct_white, pct_black, pct_native, pct_asian){
    
    black_color <- pal_all_races("Black")
    hispanic_color <- pal_all_races("Hispanic")
    white_color <- pal_all_races("White")
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

get_access_label <- function(index_val) {
  sprintf(
    "<b>Access index: %.2f</b><br/>",
    # No. children 5-17 yrs old = %i <br/>
    # Median Household Income = $%i <br/>
    # %% Hispanic Population = %g%% <br/>
    # %% White population = %g%% <br/>
    # %% Students with disability = %g%%"
    index_val
    # shape_census_block@data$
    # round(shape_census@data$MED_HH_),
    # shape_census@data$PCT_HIS,
    # shape_census@data$perc_with_transport_students,
    # shape_census@data$perc_disable_students,
    # shape_census_block
  ) %>% lapply(htmltools::HTML)
}

get_nbhd_census_labels <- function(val=NULL) {
    if (is.null(val)) {
      str_num_programs = ""
    } else {
      str_num_programs = paste("No. program sessions = ", val,"<br/>")
    }
    return(sprintf(
      "<b>%s</b><br/>
      %s 
      <i>Census level data:</i><br/>
      No. children 5-17 yrs old = %s <br/>
      Median Household Income = $%s <br/>
      < HS degree (%% Over 25) = %.1f%% <br/>
      College Graduates (%% over 25) = %.1f%% <br/>
      %% Language Besides English Spoken = %.1f%% <br/>
      %% Hispanic Population = %.1f%% <br/>
      %% White population = %.1f%% <br/>
      %% Black population = %.1f%% <br/>",
      shape_census@data$NBHD_NA,
      str_num_programs,
      format(shape_census@data$AGE_5_T, big.mark = ","),
      format(round(shape_census@data$MED_HH_), big.mark = ","),
      shape_census$PCT_LES,
      shape_census$PCT_COL,
      shape_census$PCT_NON,
      shape_census@data$PCT_HIS,
      shape_census@data$PCT_WHI,
      shape_census@data$PCT_BLA
    ) %>% lapply(htmltools::HTML)
  )
}

get_nbhd_student_labels <- function(val=NULL) {
  if (is.null(val)) {
    str_num_programs = ""
  } else {
    str_num_programs = paste("No. program sessions = ", val,"<br/>")
  }
  return(sprintf(
    "<b>%s</b><br/>
    %s
    <i>Student level data:</i><br/>
    %% English student learners = %g%% <br/>
    %% Students with disability = %g%% <br/>
    %% Hispanic students = %g%% <br/>
    %% White students = %g%% <br/>
    %% Black students = %g%% <br/>
    <i><font size=1>(Note: sample size = %g)</font></i>",
    shape_census@data$NBHD_NA,
    str_num_programs,
    shape_census@data$perc_nonenglish_students,
    shape_census@data$perc_disable_students,
    shape_census@data$perc_hispanic_students,
    shape_census@data$perc_white_students,
    shape_census@data$perc_black_students,
    aggregate_dps_student_nbhds$total_students
  ) %>% lapply(htmltools::HTML)
  )
}

get_block_census_labels <- function(val) {
  return(sprintf(
    "Access index: <b>%.2f</b><br/>
      No. children 5-17 yrs old = %i <br/>
      Median Household Income = $%i <br/>
      < HS degree (%% over 25) = %.2f%% <br/>
      %% Hispanic Population = %.2f%% <br/>
      %% White population = %.2f%% <br/>
      %% Black population = %.2f%% <br/>",
    val,
    shape_census_block@data$Ag_L_18-shape_census_block@data$Ag_Ls_5,
    shape_census_block@data$Mdn_HH_,
    100*shape_census_block$LESS_TH/shape_census_block@data$TTL_ppl,
    shape_census_block@data$PCT_Hsp,
    shape_census_block@data$PCT_Wht,
    shape_census_block@data$PCT_Afr
  ) %>% lapply(htmltools::HTML)
  )
}

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
                                  "perc_nonenglish_students" = "% EL",
                                  "perc_disable_students" = "% Disabled",
                                  "perc_hispanic_students" = "% Hispanic",
                                  "perc_white_students" = "% White",
                                  "perc_black_students" = "% Black"
)

#### TESTING - UPDATE THIS TEXT (JOE)

# Options to show in the UI for filtering by demographics
# (feel free to change these for better appearances)
demog_names <- list("None",
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
                    "Most common race + breakdown"
)

demog_student_names <- list("None selected",
                            "English learner student population (%)",
                            "Student with disability population (%)",
                            "Hispanic student population (%)",
                            "White student population (%)",
                            "Black student population (%)")

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
                             "perc_nonenglish_students",
                             "perc_disable_students",
                             "perc_hispanic_students",
                             "perc_white_students",
                             "perc_black_students")

labFormatAge = function(type, cuts, p) {
  n = length(cuts)
  paste0(round(cuts[-n]), " &ndash; ", round(cuts[-1]))
}

lab_format_list <- list(labFormatAge,
                        labelFormat(prefix = "$ "),
                        labelFormat(suffix = " %"),
                        labelFormat(suffix = " %"),
                        labelFormat(suffix = " %"),
                        labelFormat(suffix = " %"),
                        labelFormat(suffix = " %"),
                        labelFormat(suffix = " %"),
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

pal_list_student <- list(pal_el,
                         pal_disabled,
                         pal_hispanic_student,
                         pal_white_student,
                         pal_black_student
)

lab_format_list_student <- list(labelFormat(prefix = " %"),
                                labelFormat(suffix = " %"),
                                labelFormat(suffix = " %"),
                                labelFormat(suffix = " %"),
                                labelFormat(suffix = " %"))

names(pal_list_student) <- demog_student_values[demog_student_values!="none"]
names(lab_format_list_student) <- demog_student_values[demog_student_values!="none"]

############################## Demographic info for the ReSchool Summary Analysis Tab ##############################

get_nbhd_census_summary <- function(nbhd_list) {
  if (is.null(nbhd_list)) {
    census_summary_data <- NA
  } else {
    census_summary_data <- shape_census@data["NBHD_NA"]
    
    
    
    
    
    
  }
  
  return(sprintf(
    "<b>%s</b><br/>
    %s 
    <i>Census level data:</i><br/>
    No. children 5-17 yrs old = %i <br/>
    Median Household Income = $%i <br/>
    < HS desgree (%% over 25) = %.2f%% <br/>
    %% Hispanic Population = %g%% <br/>
    %% White population = %g%% <br/>
    %% Black population = %g%% <br/>",
    shape_census@data$NBHD_NA,
    str_num_programs,
    shape_census@data$AGE_5_T,
    round(shape_census@data$MED_HH_),
    shape_census$PCT_NON,
    shape_census@data$PCT_HIS,
    shape_census@data$PCT_WHI,
    shape_census@data$PCT_BLA
  ) %>% lapply(htmltools::HTML)
  )
}