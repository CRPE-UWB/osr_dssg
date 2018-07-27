# User Interface for Shiny App

library(shiny)
library(leaflet)

# Source needed data for ui and server
source('../source_file.R', chdir = TRUE)  # temp change working dir to same as source_file.R

shinyUI(navbarPage("Denver Out-of-School Resources",
                   
                   ## RESCHOOL PROGRAMS TAB
                   tabPanel("B4S Programs",
                            fluidPage(sidebarLayout(
                              
                              # Sidebar panel for making selections about reschool programs
                              sidebarPanel(
                                checkboxGroupInput("program", "Select one or more program types:", 
                                                   choices = c("Academic" = 13, "Arts" = 14, 
                                                               "Cooking" = 15, "Dance" = 16, 
                                                               "Drama" = 17, "Music" = 18, 
                                                               "Nature" = 19, "Sports" = 20, 
                                                               "STEM" = 21), 
                                                   selected = 13,
                                                   inline = TRUE
                                                   ),
                                #br(),
                                sliderInput("slider", "Select a range for program cost:", 
                                            min = minprice_reschoolprograms, 
                                            max = maxprice_reschoolprograms , 
                                            value = c(minprice_reschoolprograms, 
                                                      maxprice_reschoolprograms),
                                            pre = "$"
                                ),
                                #br(),
                                radioButtons("demographics", 
                                             "Select a demographics variable to visualize:", 
                                             choiceNames = list("Median household income ($)", 
                                                         "High school degree or equivalent (%)",
                                                         HTML("Language other than English spoken (%)<br><br>
                                                              <i>Race/Ethnicity Variables</i>"),
                                                         "Hispanic population (%)", 
                                                         "Black population (%)",
                                                         "White population (%)",
                                                         "Majority + breakdown"
                                                         ),
                                             choiceValues = list("Median household income ($)", 
                                                         "High school degree or equivalent (%)",
                                                         "Non-English speakers (%)",
                                                         "Hispanic population (%)", 
                                                         "Black population (%)",
                                                         "White population (%)",
                                                         "All races"
                                             ),
                                             selected = character(0)
                                             ),
                                br(),
                                selectInput("neighborhoods", "Restrict to one neighborhood:", 
                                            choices = c("No neighborhood selected", 
                                                        sort(neighborhoods_reshoolprograms))
                                ),
                                width = 4
                              ),
                              
                              # Main panel for reschool programs: map tab + data tab + analysis tab
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap", height = 520)
                                                     ),
                                            tabPanel("Data",
                                                     DT::dataTableOutput("datatable")
                                                     ),
                                            tabPanel("Summary analysis")
                                            )
                                ) 
                              ))
                            ),
                   
                   
                   ## OPEN DATA TAB - Parks, Libraries, etc.
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
                                radioButtons("demographics_other", 
                                             "Select a demographics variable to visualize:", 
                                             choiceNames = list("Median household income ($)", 
                                                                "High school degree or equivalent (%)",
                                                                HTML("Language other than English spoken (%)<br><br>
                                                              <i>Race/Ethnicity Variables</i>"),
                                                                "Hispanic population (%)", 
                                                                "Black population (%)",
                                                                "White population (%)",
                                                                "Majority + breakdown"
                                             ),
                                             choiceValues = list("Median household income ($)", 
                                                                 "High school degree or equivalent (%)",
                                                                 "Non-English speakers (%)",
                                                                 "Hispanic population (%)", 
                                                                 "Black population (%)",
                                                                 "White population (%)",
                                                                 "All races"
                                             ),
                                             selected = character(0)
                                ),
                                br(),
                                selectInput("neighborhoods_other", 
                                            "Restrict to one neighborhood:", 
                                            choices = c("No neighborhood selected", 
                                                        sort(neighborhoods_other)
                                                        )
                                ),
                                br()
                              ),
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap_other", height = 520)),
                                            tabPanel("Data",
                                                     uiOutput("dt")),
                                            tabPanel("Summary analysis")
                                )
                              )
                            ))
                            ), 

                   ## RESCHOOL SEARCH DATA TAB
                   tabPanel("ReSchool Program Searches",
                            
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                sliderInput("slider_searchprog", "Select a range for program cost:", 
                                            min = minprice_search, 
                                            max = maxprice_search , 
                                            value = c(minprice_search, 
                                                      maxprice_search),
                                            pre = "$"
                                ),
                                br()),
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Summary"),
                                            tabPanel("Visualization")
                                )
                              )
                            ))
                            
                            
                            ),
                   
                   ## ACCESS INDEX TAB
                   tabPanel("Access Index")
)
)

