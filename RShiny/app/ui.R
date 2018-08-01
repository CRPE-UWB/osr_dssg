# User Interface for OSR Shiny App

library(shiny)
library(leaflet)
# Source needed data and functions for ui and server
source('color.R')
source('../source_file.R', chdir = TRUE)  # temp changes working dir to same as source_file.R
source('helpers.R')

# UI options for filtering by demographics
demog_names <- list("None selected",
                    "Number of 5-17 year olds",
                    "Median household income ($)", 
                    "Less than high school degree (% over 25 years)",
                    "College graduates (% over 25 years)",
                     HTML("Language other than English spoken (%)
                          <br><br>
                          <i>Race/Ethnicity Variables</i>"
                          ),
                    "Hispanic population (%)", 
                    "Black population (%)",
                    "White population (%)",
                    "Majority + breakdown"
                    )

# internal values for options for filtering by demographics
demog_values <- list("None selected", 
                     "Number of 5-17 year olds",
                     "Median household income ($)", 
                     "Less than high school degree (%)",
                     "College graduates (%)",
                     "Non-English speakers (%)",
                     "Hispanic population (%)", 
                     "Black population (%)",
                     "White population (%)",
                     "All races"
                     )

shinyUI(
  
  fluidPage(
  
  includeCSS("style.css"),
  
  navbarPage("Denver Out-of-School Resources",
                   
                   ## RESCHOOL PROGRAMS TAB
                   tabPanel("B4S Programs",
                            fluidPage(
                              
                              # includeCSS("style.css"),
                              
                              sidebarLayout(
                              
                                # Sidebar panel for making selections about reschool programs
                                sidebarPanel(
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
                                                     ),
                                  sliderInput("slider", "Select a range for program cost:", 
                                              min = minprice_reschoolprograms, 
                                              max = maxprice_reschoolprograms , 
                                              value = c(minprice_reschoolprograms, 
                                                        maxprice_reschoolprograms),
                                              pre = "$"
                                              ),
                                  radioButtons("demographics", 
                                               "Select a demographics variable to visualize:", 
                                               choiceNames = demog_names,
                                               choiceValues = demog_values,
                                               selected = "None selected"
                                               ),
                                  br(),
                                  selectInput("neighborhoods", "Outline neighborhood:", 
                                              choices = c("No neighborhood selected", 
                                                          neighborhoods_list
                                                          )
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
                                                   column(6, plotOutput("program_type_summary")),
                                                   column(6, plotOutput("program_cost_summary"))
                                                 ),
                                                 uiOutput("program_special_cats"),
                                                 DT::dataTableOutput("nbhd_summary")
                                              )
                                              
                                    )
                                  ) 
                                )
                              ) # end sidebar layout and fluidPage
                            ),  # end B4S programs tab
                   
                   
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
                                # br(),
                                radioButtons("demographics_other", 
                                             "Select a demographics variable to visualize:", 
                                             choiceNames = demog_names,
                                             choiceValues = demog_values,
                                             selected = "None selected"
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
                                                     leafletOutput("mymap_other", height = 520),
                                                     downloadButton('other_map_down', label = "Download Map")
                                                     ),
                                            tabPanel("Data",
                                                     uiOutput("dt")),
                                            tabPanel("Summary analysis")
                                )
                              )  # end mainPanel of open data tab
                              
                            ))
                            
                   ),  # end open data tab

                   ## RESCHOOL SEARCH DATA TAB
                   tabPanel("ReSchool Program Searches",
                      
                      fluidPage(sidebarLayout(
                        
                        sidebarPanel(fluidRow(
                          # column(6,selectInput("minprice_search", "Select Min Price:", 
                          #             choices = c("No min price selected", 
                          #                         sort(unique(google_analytics$mincost)))
                          # )),
                          # column(6,selectInput("maxprice_search", "Select Max Price:", 
                          #             choices = c("No max price selected", 
                          #                         sort(unique(google_analytics$maxcost)))
                          # )),
                          # br(),
                          
                          column(6, textInput("minprice_search", "Enter Min Cost:","")),
                          column(6, textInput("maxprice_search", "Enter Max Cost:","")),
                          
                          # column(6,selectInput("minage_search", "Select Min Age:", 
                          #                      choices = c("No min age selected", 
                          #                                  sort(unique(google_analytics$minage)))
                          # )),
                          # column(6,selectInput("maxage_search", "Select Max Age:", 
                          #                      choices = c("No max age selected", 
                          #                                  sort(unique(google_analytics$maxage)))
                          # )),
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
                                             )
                          ),
                          radioButtons("specialneeds_search", 
                                       "Other selections", 
                                       choices = c("Special needs students", "Scholarships Available", "None Selected"),
                                       selected = "None Selected"
                                     
                          )),
                        
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
                                      tabPanel("Visualization")
                          ))  # end main panel of reschool search data tab
                        
                      ))
                      ),  # end reschool search data tab
                   
                   ## ACCESS INDEX TAB
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
                                selectInput("neighborhoods_access", "Restrict to one neighborhood:", 
                                            choices = c("No neighborhood selected", 
                                                        neighborhoods_list)
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
                              
                            ))  # end sidebar layrout and access index fluidPage
                  )  # end access index tab
             
             )  # end navbarPage 
  
  ))  # end fluidPage for whole UI, and shinyUI
