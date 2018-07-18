
#Creating a new column the calculates median income and 
library(shiny)

shinyUI(navbarPage("Out of School Resource Project",
                   tabPanel("ReSchool Programs",
                            fluidPage(sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput("neighborhoods", "1. Select the neighborhood from the dataset", choices = c("No neighborhood selected", neighborhoods_reshoolprograms)),
                                br(),
                                sliderInput("slider", "2. Select the cost of the program", min = minprice_reschoolprograms , max = maxprice_reschoolprograms , value = c(minprice_reschoolprograms, maxprice_reschoolprograms)  ),
                                br(),
                                checkboxGroupInput("program", "3. Select the type of the program", choices = c("Academic" = 9, "Arts" = 10,"Cooking" = 11,"Dance" =12, 
                                                                                                               "Drama" =13, "Music" = 14, "Nature" = 15, "Sports" = 16, "STEM" = 17), selected = 9),
                                br(),
                                radioButtons("demographics", "3. Select the demographics variable", choices = c("Median household income ($)","Above 25 high school degree holders(%)",
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
                   
                   tabPanel("Libraries and Parks") 
                   
                   
)
)



