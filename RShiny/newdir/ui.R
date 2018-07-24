
#Creating a new column the calculates median income and 
library(shiny)

shinyUI(navbarPage("Out of School Resource Project",
                   tabPanel("ReSchool Programs",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput("neighborhoods", "Select the neighborhood from the dataset", choices = c("No neighborhood selected", neighborhoods_reshoolprograms)),
                                br(),
                                sliderInput("slider", "Select the cost of the program", min = minprice_reschoolprograms , max = maxprice_reschoolprograms , value = c(minprice_reschoolprograms, maxprice_reschoolprograms)  ),
                                br(),
                                checkboxGroupInput("program", "Select the type of the program", choices = c("Academic" = 13, "Arts" = 14,"Cooking" = 15,"Dance" =16, 
                                                                                                               "Drama" =17, "Music" = 18, "Nature" = 19, "Sports" = 20, "STEM" = 21), selected = 13),
                                br(),
                                radioButtons("demographics", "Select the demographics variable", choices = c("Median household income ($)","High school degree or equivalent(%)",
                                                                                                                "Hispanic population (%)", "Non native English speakers (%)"), selected = character(0)),
                                br()
                              ),
                              
                              
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap", height = 650)),
                                            tabPanel("Data",
                                                     DT::dataTableOutput("datatable")),
                                            tabPanel("Summary analysis")
                                )
                              ))
                            )),
                   
                   tabPanel("Other out-of-school resources",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput("neighborhoods_other", "Select the neighborhood from the dataset", choices = c("No neighborhood selected", neighborhoods_other)),
                                br(),
                                checkboxGroupInput("program_other", "Select the type of the program", choices = c("Parks", "Playgrounds", "Rec Centers", "Libraries", 
                                                                                                            "Museums", "Fields"), selected = "Parks"),
                                br(),
                                radioButtons("demographics_other", "Select the demographics variable", choices = c("Median household income ($)","High school degree or equivalent(%)",
                                                                                                             "Hispanic population (%)", "Non native English speakers (%)"), selected = character(0)),
                                br()
                              ),
                              
                              
                              
                              mainPanel(
                                tabsetPanel(type = "tab",
                                            tabPanel("Map",
                                                     leafletOutput("mymap_other", height = 650)),
                                            tabPanel("Data",
                                                     DT::dataTableOutput("datatable_other")),
                                            tabPanel("Summary analysis")
                                )
                              ))
                            )
                            ) 
                   
                   
)
)



