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
             selected = "B4S Programs",
                   
########################## Blueprint4Summer Programs Tab ###########################################

                   tabPanel("B4S Programs",
                            fluidPage(
                              
                              # includeCSS("style.css"),
                              
                              sidebarLayout(
                              
                                # Sidebar panel for making selections about reschool programs
                                sidebarPanel(
                                  conditionalPanel(condition = "input.program_panel != 'Summary Analysis'",
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
                                                       selected = c("has_academic", "has_arts", 
                                                                    "has_cooking", "has_dance", 
                                                                    "has_drama","has_music",
                                                                    "has_nature", "has_sports",
                                                                    "has_stem"),
                                                       inline = TRUE
                                                       )
                                  ),
                                conditionalPanel(condition = "input.program_panel != 'Summary Analysis'",
                                  checkboxGroupInput("special_needs", NULL, 
                                                     choiceNames = "Only show programs that allow special needs", 
                                                     choiceValues = TRUE,
                                                     selected = NULL
                                  )
                                ),
                                  conditionalPanel(condition = "input.program_panel != 'Summary Analysis'",
                                    sliderInput("slider", "Select a range for program cost:", 
                                                min = minprice_reschoolprograms, 
                                                max = maxprice_reschoolprograms , 
                                                value = c(minprice_reschoolprograms, 
                                                          maxprice_reschoolprograms),
                                                pre = "$"
                                                )
                                    ),
                                  conditionalPanel(condition = "input.program_panel == 'Map'",
                                    radioButtons("school_or_census", "Select a data source for demographic data:",
                                               choiceNames = c("Census (General Population)","DPS (Student Data)"),
                                               choiceValues = c("census_dems", "student_dems"),
                                               selected = "census_dems")
                                    ),
                                               
                                  conditionalPanel(condition = "input.program_panel != 'Data' & input.program_panel != 'Summary Analysis' & input.school_or_census == 'census_dems'",
                                    radioButtons("demographics", 
                                                 "Select a demographics variable to visualize:", 
                                                 choiceNames = demog_names,
                                                 choiceValues = demog_values,
                                                 selected = "none"
                                                 )
                                    ),
                                  conditionalPanel(condition = "input.program_panel != 'Data' & input.program_panel != 'Summary Analysis' & input.school_or_census == 'student_dems'",
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
                                  conditionalPanel(condition = "input.program_panel == 'Summary Analysis'",
                                                   radioButtons("program_analysis", 
                                                                "In these neighborhoods, what is the distribution of program", 
                                                                choiceNames = c("categories?",
                                                                                "costs?",
                                                                                "dates?"),
                                                                choiceValues = c("category_question",
                                                                                 "cost_question",
                                                                                 "date_question"),
                                                                selected = "category_question"
                                                   )
                                  ),
                                  width = 4
                                  ),
                                
                                
                                # Main panel for reschool programs: map tab + data tab + analysis tab
                                mainPanel(
                                  tabsetPanel(type = "tab",
                                              tabPanel("Map",
                                                       leafletOutput("mymap", height = 520),
                                                       br(),
                                                       downloadButton('reschool_map_down', label = "Download Map (Takes About 10 Seconds)"),
                                                       br(), br()
                                                       ),
                                              tabPanel("Data",
                                                       br(),
                                                       DT::dataTableOutput("datatable"),
                                                       downloadButton("download_reschool_data", "Download Data"),
                                                       br(), br()
                                                       ),
                                              tabPanel(
                                                "Summary Analysis",
                                                br(), br(),
                                                uiOutput("summary_title"),
                                                div(plotlyOutput("program_summary_plot", height = "250px")),
                                                br(),
                                                fluidRow(
                                                  column(6, uiOutput("nbhd_census_demog_summary")),
                                                  column(6, uiOutput("nbhd_student_demog_summary"))
                                                ),
                                                br(),
                                                plotlyOutput("med_income_summary", height = "80px", width = 300),
                                                br()
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
                                conditionalPanel(condition = "input.program_other_panel != 'Summary Analysis'",
                                                checkboxGroupInput("program_other", 
                                                                   "Select one or more resource types:", 
                                                                   choices = c("Parks", "Playgrounds", 
                                                                               "Rec Centers", "Libraries", 
                                                                               "Museums", "Fields"), 
                                                                   selected = "Parks", 
                                                                   inline = TRUE
                                                                   ),
                                                br()
                                                ),
                                conditionalPanel(condition = "input.program_other_panel == 'Map'",
                                                radioButtons("school_or_census_other", "Select a data source for demographic data:",
                                                             choiceNames = c("Census (General Population)","DPS (Student Data)"),
                                                             choiceValues = c("census_dems", "student_dems"),
                                                             selected = "census_dems"
                                                )
                                ),
                                conditionalPanel(condition = "input.program_other_panel != 'Data' & input.program_other_panel != 'Summary Analysis' & input.school_or_census_other == 'census_dems'",
                                                 radioButtons("demographics_other", 
                                                              "Select a demographics variable to visualize:", 
                                                              choiceNames = demog_names,
                                                              choiceValues = demog_values,
                                                              selected = "none"
                                                 )
                                ),
                                conditionalPanel(condition = "input.program_other_panel != 'Data' & input.program_other_panel != 'Summary Analysis' & input.school_or_census_other == 'student_dems'",
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
                                                     br(),
                                                     downloadButton('other_map_down', label = "Download Map (Takes About 10 Seconds)"),
                                                     br(), br()
                                                     ),
                                            tabPanel("Data",
                                                     br(),
                                                     uiOutput("dt")),
                                            tabPanel("Summary Analysis",
                                                     br(),
                                                     uiOutput("summary_title_other"),
                                                     br(),
                                                     plotlyOutput('other_resources_summary', height = "200px"),
                                                     br(), br(),
                                                     fluidRow(
                                                       column(6, uiOutput('nbhd_census_demog_summary_other')),
                                                       column(6, uiOutput('nbhd_student_demog_summary_other'))
                                                     ),
                                                     br(),
                                                     plotlyOutput("med_income_summary_other", height = "80px", width = 300),
                                                     br()
                                                     ),
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
                          
                          #fluidRow(
                          checkboxGroupInput("program_search", 
                                             "Filter down to one or more program types:", 
                                             choiceNames = c("Academic", "Arts", "Cooking", "Dance", "Drama", "Music", "Nature", "Sports", "STEM"),
                                             choiceValues = sort(unique(google_analytics$category)), 
                                             selected = character(0), 
                                             inline = TRUE
                          ),
                          # br(),
                          fluidRow(
                          column(6, textInput("minprice_search", "Enter Min Cost:","")),
                          column(6, textInput("maxprice_search", "Enter Max Cost:",""))
                                 ),
                          # br(),
                          fluidRow(
                          column(6, textInput("minage_search", "Enter Min Age:","")),
                          column(6, textInput("maxage_search", "Enter Max Age:",""))
                                 ),
                          
                          selectInput("zipcode_searchprog", "Filter down to one zipcode:", 
                                      choices = c("N/A", 
                                                  sort(zipcode_searchdata)
                                                  )
                                      ),
                          # br(),
                          selectInput("sessiontimes_searchprog", "Filter down to one session time:", 
                                      choices = c("N/A", 
                                                  sort(unique(google_analytics$sessiontimes))
                                                  )
                                      ), 
                          # br(),
                          radioButtons("specialneeds_search", 
                                       "Other filters:", 
                                       choices = c("Special needs students", "Scholarships available", "N/A"),
                                       selected = "N/A"
                                     

                          )),
                          
                          conditionalPanel(condition = "input.conditionedPanels == 'Visualization'",
                                           radioButtons("specific_search_questions", "Choose a question about the Blueprint4Summer Search Data to investigate:", 
                                                       choices = c("What program categories do people search for the most?",
                                                                   "What distances and session times do people search for, and how do they sort their results?",
                                                                   "What locations are people searching for?",
                                                                   "What locations are people searching for? - spatial analysis")))
                          ),

                        
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Summary",
                                               br(),
                                               fluidRow(
                                                 column(6, uiOutput("totalsearches", 
                                                                    style = "background-color:#c6dbef; 
                                                                    height:100px; padding:20px;
                                                                    border:solid", align = "center")),
                                                 column(6, uiOutput("percentagesearches", style = "background-color:#c6dbef; 
                                                                    height:100px; padding:20px;
                                                                    border:solid", align = "center"))
                                               ),
                                               br(),
                                               DT::dataTableOutput("datatable_search"),
                                               downloadButton("download_search_data", label = "Download Data"),
                                               br(), br()
                                      ),

                                      tabPanel("Visualization",
                                              
                                              conditionalPanel('input.specific_search_questions==
                                                               "What distances and session times do people search for, and how do they sort their results?"',
                                                          br(),
                                                          div(plotlyOutput("search_distance_plot", height = "250px")),
                                                          HTML("<i>Note: Searches with distance set to 20 miles are excluded in this plot, 
                                                               as 20 miles was the default search distance and appeared abnormally often.</i>"),
                                                          br(),
                                                          br(),
                                                          fluidRow(
                                                                  column(6,div(plotlyOutput("search_sessiontimes_plot", height = "250px"))),
                                                                  column(6,div(plotlyOutput("search_sort_plot", height = "250px")))
                                                                  )
                                                          ),
                                              
                                              conditionalPanel('input.specific_search_questions=="What program categories do people search for the most?"',
                                                               br(),
                                                               div(plotlyOutput("search_prog_category", height = "250px")), 
                                                               br(),
                                                               div(plotlyOutput("search_compare_prog_category", height = "350px")),
                                                               br()
                                                               ) ,
                                              conditionalPanel('input.specific_search_questions=="What locations are people searching for?"',
                                                               br(),
                                                               div(plotlyOutput("search_zipcode_plot", height = "200px")), 
                                                               br(),
                                                               div(plotlyOutput("search_programs_zipcode_plot", height = "200px"))
                 
                                              ),
                                              conditionalPanel('input.specific_search_questions=="What locations are people searching for? - spatial analysis"',
                                                               leafletOutput("search_mymap", height = 520),
                                                               HTML("<i>Note: Only top 20 zipcodes in terms of the number of searches are shown.</i>")
                                                               
                                              )
                                              ),
                                      id = "conditionedPanels"
                          ) 
                        )#end of main panel

                          ))  
                        

                      ),  # end reschool search data tab
                   
################################### Access Index Tab #################################################

                   tabPanel("Access Index",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                conditionalPanel(condition = "input.access_panel == 'Summary Analysis'",
                                                 radioButtons("specific_access_questions", "Choose a question to investigate about student access to Blueprint4Summer programs:", 
                                                              choiceNames = c("How equally is access distributed among neighborhoods?",
                                                                          "How do the number of programs correspond to the number of students in neighborhoods?",
                                                                          "How does access differ among demographic groups?"),
                                                              choiceValues = c("lorenz", "scatter", "demog")),
                                                 br(), 
                                                 br()
                                ),
                                checkboxGroupInput("type_access", 
                                                   "Select one or more program types to include:", 
                                                   choiceNames = c("Academic", "Arts", "Athletic", "Nature"),
                                                   choiceValues = list("academic","art","sports","nature"),
                                                   inline = TRUE,
                                                   selected = c("academic","art","sports","nature")
                                                   ),
                                br(),
                                radioButtons("cost_access", 
                                             "Select a cost range for programs to include:", 
                                             choiceNames = list("Free", "Free to Low Cost", "All Programs"),
                                             choiceValues = list("free", "low", "any"),
                                             selected = "any"
                                             ),
                                br(),
                                radioButtons("drive_or_transit",
                                             "Calculate distances to programs based on driving or 
                                             public transit?",
                                             choiceNames = list("Drive", "Transit"),
                                             choiceValues = list("drive", "transit"),
                                             selected = "drive"
                                             ),
                                radioButtons("disability",
                                             "Access specifically for students with disability?",
                                             choiceNames = c("Any student", "Students with disability"),
                                             choiceValues = c(FALSE,TRUE),
                                             selected = FALSE),
                                br(),
                                conditionalPanel(condition = "input.access_panel == 'Map'",
                                  selectInput("neighborhoods_access", "Focus on neighborhoods:", 
                                              choices = c("All neighborhoods", 
                                                          neighborhoods_list),
                                              multiple = TRUE,
                                              selected = "All neighborhoods"
                                  ))
                              ),  # end sidebarPanel for access index
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap_access", height = 520),
                                                     br(),
                                                     downloadButton('access_map_down', label = "Download Map (Takes About 10 Seconds)"),
                                                     br(), br()
                                                     ),
                                            tabPanel("Data",
                                                     br(),
                                                     DT::dataTableOutput("datatable_access"),
                                                     downloadButton("download_access_data", "Download Data"),
                                                     br(), br()
                                            ),
                                            tabPanel("Summary Analysis",
                                                     conditionalPanel(condition = "input.specific_access_questions == 'lorenz'",
                                                                      plotOutput("lorenz")
                                                     ),
                                                     conditionalPanel(condition = "input.specific_access_questions == 'scatter'",
                                                                      plotOutput("access_scatter")
                                                     ),
                                                     conditionalPanel(condition = "input.specific_access_questions == 'demog'",
                                                                      plotlyOutput("access_demog")
                                                     )
                                                     ),
                                            id = "access_panel"
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
