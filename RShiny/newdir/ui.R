# User Interface for Shiny App

library(shiny)

shinyUI(navbarPage("Denver Out-of-School Resources",
                   
                   ## RESCHOOL PROGRAMS TAB
                   tabPanel("B4S Programs",
                            fluidPage(sidebarLayout(
                              
                              # Sidebar panel for making selections about reschool programs
                              sidebarPanel(
                                selectInput("neighborhoods", "Select a neighborhood in Denver:", 
                                            choices = c("No neighborhood selected", 
                                                        neighborhoods_reshoolprograms)
                                            ),
                                br(),
                                sliderInput("slider", "Select a range for program cost:", 
                                            min = minprice_reschoolprograms, 
                                            max = maxprice_reschoolprograms , 
                                            value = c(minprice_reschoolprograms, 
                                                      maxprice_reschoolprograms),
                                            pre = "$"
                                            ),
                                br(),
                                checkboxGroupInput("program", "Select one or more program types:", 
                                                   choices = c("Academic" = 13, "Arts" = 14, 
                                                               "Cooking" = 15, "Dance" = 16, 
                                                               "Drama" = 17, "Music" = 18, 
                                                               "Nature" = 19, "Sports" = 20, 
                                                               "STEM" = 21), 
                                                   selected = 13,
                                                   inline = TRUE
                                                   ),
                                br(),
                                radioButtons("demographics", 
                                             "Select a demographics variable to visualize for each neighborhood:", 
                                             choices = c("Median household income ($)", 
                                                         "High school degree or equivalent (%)",
                                                         "Hispanic population (%)", 
                                                         "Non-native English speakers (%)"),
                                             selected = character(0)
                                             ),
                                br(),
                                width = 3
                              ),
                              
                              # Main panel for reschool programs: map tab + data tab + analysis tab
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap", height = 500)
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
                   tabPanel("Other Resources"),
                   
                   ## RESCHOOL SEARCH DATA TAB
                   tabPanel("B4S Searches"),
                   
                   ## ACCESS INDEX TAB
                   tabPanel("Access Index")
)
)

