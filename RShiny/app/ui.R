# User Interface for OSR Shiny App!

# Load libraries needed for the UI
library(shiny)
library(leaflet)
library(maptools)

library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(plotly)

# Source needed data and functions for ui and server - impt. to do in this order!!
# (note that all paths should be relative to the location of this ui.R file)
source(file.path('..', 'get_data.R'), chdir = TRUE)
source(file.path('..', 'color.R'))
source(file.path('..', 'labels.R'))
source(file.path('..', 'mapping_helpers.R'))
source(file.path('..', 'other_helpers.R'))
source(file.path('..', 'manual.R'))

########################## Start the Shiny UI ####################################################

shinyUI(
  
  fluidPage(
  
  includeCSS("style.css"),
  
  navbarPage("Denver Out-of-School Resources",
             selected = "About this Tool",
                   
########################## Blueprint4Summer Programs Tab ###########################################

                   tabPanel("B4S Programs",
                            fluidPage(
                              
                              # includeCSS("style.css"),
                              
                              sidebarLayout(
                              
                                # Sidebar panel for making selections about reschool programs
                                sidebarPanel(
                                  conditionalPanel(condition = "input.program_panel != 'Summary analysis'",
                                    checkboxGroupInput("program", "Select one or more program types:", 
                                                       choiceNames = c("Academic", "Arts", 
                                                                   "Cooking", "Dance", 
                                                                   "Drama", "Music", 
                                                                   "Nature", "Sports", 
                                                                   "STEM"), 
                                                       choiceValues = c("has_academic", "has_arts", 
                                                                        "has_cooking", "has_dance", 
                                                                        "has_drama","has_music",
                                                                        "has_nature", "has_sports",
                                                                        "has_stem"),
                                                       selected = "has_academic",
                                                       inline = TRUE
                                                       )
                                  ),
                                conditionalPanel(condition = "input.program_panel != 'Summary analysis'",
                                  checkboxGroupInput("special_needs", NULL, 
                                                     choiceNames = "Only show programs that allow special needs", 
                                                     choiceValues = TRUE,
                                                     selected = NULL
                                  )
                                ),
                                  conditionalPanel(condition = "input.program_panel != 'Summary analysis'",
                                    sliderInput("slider", "Select a range for program cost:", 
                                                min = minprice_reschoolprograms, 
                                                max = maxprice_reschoolprograms , 
                                                value = c(minprice_reschoolprograms, 
                                                          maxprice_reschoolprograms),
                                                pre = "$"
                                                )
                                    ),
                                  radioButtons("school_or_census", "Select whether you'd like to see demographics 
                                               for students (from DPS data) or for the general population 
                                               (from census data)",
                                               choiceNames = c("Census","Student"),
                                               choiceValues = c("census_dems", "student_dems"),
                                               selected = "census_dems"
                                               
                                  ),
                                  conditionalPanel(condition = "input.program_panel != 'Data' & input.program_panel != 'Summary analysis' & input.school_or_census == 'census_dems'",
                                    radioButtons("demographics", 
                                                 "Select a demographics variable to visualize:", 
                                                 choiceNames = demog_names,
                                                 choiceValues = demog_values,
                                                 selected = "none"
                                                 )
                                    ),
                                  conditionalPanel(condition = "input.program_panel != 'Data' & input.program_panel != 'Summary analysis' & input.school_or_census == 'student_dems'",
                                                   radioButtons("student_demographics", 
                                                                "Select a demographics variable to visualize:", 
                                                                choiceNames = demog_student_names,
                                                                choiceValues = demog_student_values,
                                                                selected = "none"
                                                    )
                                                   ),
                                  br(),
                                  selectInput("neighborhoods", "Focus on neighborhoods:", 
                                              choices = c("All neighborhoods", 
                                                          neighborhoods_list
                                                          ),
                                              multiple = TRUE,
                                              selected = "All neighborhoods"
                                              ),
                                  width = 4
                                  ),
                                
                                # Main panel for reschool programs: map tab + data tab + analysis tab
                                mainPanel(
                                  tabsetPanel(type = "tab",
                                              tabPanel("Map",
                                                       leafletOutput("mymap", height = 520),
                                                       downloadButton('reschool_map_down', label = "Download Map")
                                                       ),
                                              tabPanel("Data",
                                                       DT::dataTableOutput("datatable")
                                                       ),
                                              tabPanel(
                                                "Summary analysis",
                                                 uiOutput("summary_title"),
                                                 fluidRow(
                                                   column(6, plotOutput("program_type_summary", height = "250px")),
                                                   column(6, plotOutput("program_cost_summary", height = "250px"))
                                                 ),
                                                br(),
                                                 uiOutput("program_special_cats"),
                                                 DT::dataTableOutput("nbhd_summary")
                                              ),
                                              id = "program_panel"
                                              
                                    )
                                  ) 
                                )
                              ) # end sidebar layout and fluidPage
                            ),  # end B4S programs tab
                   
                   
####################### Other Resources / Open Data Tab - Parks, Libraries, etc. #######################

                   tabPanel("Other Resources",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                checkboxGroupInput("program_other", 
                                                   "Select one or more resource types:", 
                                                   choices = c("Parks", "Playgrounds", 
                                                               "Rec Centers", "Libraries", 
                                                               "Museums", "Fields"), 
                                                   selected = "Parks", 
                                                   inline = TRUE
                                                   ),
                                br(),
                                radioButtons("school_or_census_other", "Select whether you'd like to see demographics 
                                               for students (from DPS data) or for the general population 
                                             (from census data)",
                                             choiceNames = c("Census","Student"),
                                             choiceValues = c("census_dems", "student_dems"),
                                             selected = "census_dems"
                                             
                                ),
                                conditionalPanel(condition = "input.program_other_panel != 'Data' & input.program_other_panel != 'Summary analysis' & input.school_or_census_other == 'census_dems'",
                                                 radioButtons("demographics_other", 
                                                              "Select a demographics variable to visualize:", 
                                                              choiceNames = demog_names,
                                                              choiceValues = demog_values,
                                                              selected = "none"
                                                 )
                                ),
                                conditionalPanel(condition = "input.program_other_panel != 'Data' & input.program_other_panel != 'Summary analysis' & input.school_or_census_other == 'student_dems'",
                                                 radioButtons("student_demographics_other", 
                                                              "Select a demographics variable to visualize:", 
                                                              choiceNames = demog_student_names,
                                                              choiceValues = demog_student_values,
                                                              selected = "none"
                                                 )
                                ),
                                selectInput("neighborhoods_other", 
                                            "Focus on neighborhoods:", 
                                            choices = c("All neighborhoods", 
                                                            neighborhoods_list
                                                        ),
                                            multiple = TRUE,
                                            selected = "All neighborhoods"
                                ),
                                br()
                              ),
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap_other", height = 520),
                                                     downloadButton('other_map_down', label = "Download Map")
                                                     ),
                                            tabPanel("Data",
                                                     uiOutput("dt")),
                                            tabPanel("Summary analysis"),
                                            id = "program_other_panel"
                                )
                              )  # end mainPanel of open data tab
                              
                            ))
                            
                   ),  # end open data tab

########################## Blueprint4Summer Search Data Tab ###########################################

                   tabPanel("B4S Searches",
                      
                      fluidPage(sidebarLayout(
                        
                        sidebarPanel(
                          
                          conditionalPanel(condition = "input.conditionedPanels == 'Summary'",
                          
                          fluidRow(
                        
                          column(6, textInput("minprice_search", "Enter Min Cost:","")),
                          column(6, textInput("maxprice_search", "Enter Max Cost:","")),
                          br(),
                          column(6, textInput("minage_search", "Enter Min Age:","")),
                          column(6, textInput("maxage_search", "Enter Max Age:","")),
                          
                          selectInput("zipcode_searchprog", "Restrict to one zipcode:", 
                                      choices = c("No zipcode selected", 
                                                  sort(zipcode_searchdata)
                                                  )
                                      ),
                          br(),
                          selectInput("sessiontimes_searchprog", "Restrict to one session time:", 
                                      choices = c("No session time selected", 
                                                  sort(unique(google_analytics$sessiontimes))
                                                  )
                                      ), 
                          br(),
                          checkboxGroupInput("program_search", 
                                             "Select one or more program type:", 
                                             choices = sort(unique(google_analytics$category)), 
                                             selected = character(0), 
                                             inline = TRUE
                                             ),br(),
                          radioButtons("specialneeds_search", 
                                       "Other selections", 
                                       choices = c("Special needs students", "Scholarships Available", "None Selected"),
                                       selected = "None Selected"
                                     

                          ))),
                          
                          conditionalPanel(condition = "input.conditionedPanels == 'Visualization'",
                                           selectInput("specific_search_questions", "Select:", 
                                                       choices = c("Insights about the number of searches made by program category",
                                                                   "Number of searches made by different variables",
                                                                   "Number of searches made by zipcode")))
                          ),

                        
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Summary",
                                               
                                               fluidRow(

                                                 column(6, uiOutput("totalsearches", 
                                                                    style = "background-color:lightblue; 
                                                                    height:100px; padding:20px;
                                                                    border:solid", align = "center")),
                                                 column(6, uiOutput("percentagesearches", style = "background-color:lightblue; 
                                                                    height:100px; padding:20px;
                                                                    border:solid", align = "center")),
                                                 
                                                 DT::dataTableOutput("datatable_search")
                                               )
                                      ),

                                      tabPanel("Visualization",
                                              
                                               
                                              conditionalPanel('input.specific_search_questions=="Number of searches made by different variables"',
                                                          fluidRow(
                                                          column(6,div(plotlyOutput("search_sort_plot", height = "300px"))),
                                                          column(6,div(plotlyOutput("search_sessiontimes_plot", height = "300px")))),
                                                          div(plotlyOutput("search_distance_plot", height = "400px"))
                                                          ),
                                              
                                              conditionalPanel('input.specific_search_questions=="Insights about the number of searches made by program category"',
                                                               fluidRow(
                                                                 div(plotlyOutput("search_prog_category", height = "300px")), br(),
                                                                 div(plotlyOutput("search_compare_prog_category", height = "350px"))
                                                                 
                                                               )
                                                               ) ,
                                              conditionalPanel('input.specific_search_questions=="Number of searches made by zipcode"',
                                                                 fluidRow(
                                                                 div(plotlyOutput("search_zipcode_plot", height = "300px")), br(),
                                                                 div(plotlyOutput("search_programs_zipcode_plot", height = "300px")))
                                                                 
                                                               
                                              )),
                                      id = "conditionedPanels"
                          ) 
                        )#end of main panel

                          ))  
                        

                      ),  # end reschool search data tab
                   
################################### Access Index Tab #################################################

                   tabPanel("Access Index",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                checkboxGroupInput("type_access", 
                                                   "Select one or more program types:", 
                                                   choiceNames = c("Academic", "Arts", "Athletic", "Nature"),
                                                   choiceValues = list("academic","art","sports","nature"),
                                                   inline = TRUE,
                                                   selected = c("academic","art","sports","nature")
                                                   ),
                                br(),
                                radioButtons("cost_access", 
                                             "Select a cost range for programs:", 
                                             choiceNames = list("Free", "Free to Low Cost", "All Programs"),
                                             choiceValues = list("free", "low", "any"),
                                             selected = "any"
                                             ),
                                br(),
                                radioButtons("drive_or_transit",
                                             "Drive or transit?",
                                             choiceNames = list("Drive", "Transit"),
                                             choiceValues = list("drive", "transit"),
                                             selected = "drive"
                                             ),
                                br(),
                                selectInput("neighborhoods_access", "Focus on neighborhoods:", 
                                            choices = c("All neighborhoods", 
                                                        neighborhoods_list),
                                            multiple = TRUE,
                                            selected = "All neighborhoods"
                                ),
                                br()
                              ),  # end sidebarPanel for access index
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap_access", height = 520),
                                                     downloadButton('access_map_down', label = "Download Map")
                                                     )
                                            # tabPanel("Data",
                                            #          uiOutput("dt")),
                                            # tabPanel("Summary analysis")
                                )
                              )  # end main panel for access index
                              
                            ))  # end sidebar layout and access index fluidPage
                  ),  # end access index tab

############################### Manual Tab ################################################
                  tabPanel("About this Tool",
                           fluidPage( 
                             
                             HTML( manual )
                             
                           )
                  )

########################## Ending the Shiny UI ###########################################

             )  # end navbarPage 
  )
)  # end fluidPage for whole UI, and shinyUI
