



#############################################################################################################################################################################


library(shiny)
library(dplyr)
library(leaflet)

library(shiny)



shinyUI(fluidPage(
  
  #Header for title panel  
  titlePanel(title = h4("Out of School Resource Project", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("zipcode", "1. Select the zipcode from the dataset", choices = c("No zipcode selected", vars)),
      br(),
      sliderInput("slider", "2. Select the cost of the program", min = minprice , max = maxprice , value = c(minprice, maxprice)  ),
      br(),
      
      checkboxGroupInput("var", "3. Select the type of the program", choices = c("Academic" = 5, "Cooking" = 6, "Arts" = 7, "Dance" = 8, 
                                                                                 "Drama" = 9, "Music" = 10, "Nature" = 11)),
      br()
      
    ),
    
    
    
    mainPanel(
      
      tabsetPanel(type = "tab",
                  
                  tabPanel("Map",
                           
                           leafletOutput("mymap", height = 650)),
                           
                  
                  tabPanel("Data",
                           
                
                             
                             DT::dataTableOutput("datatable")
                             
                           
                  )
                  
                  
                  ))
      
      
    )
  )
)



