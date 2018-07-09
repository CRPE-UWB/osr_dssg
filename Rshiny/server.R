
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(leaflet)
library(sp)

shinyServer(
  
  function(input, output) {
    
    
    colm = reactive({
      
      as.numeric(input$var)
      
    })
    
    program_category_data = reactive({
      
     
     
        
      
        a = summer_program_data[apply(summer_program_data[,colm()] == 1, 1, any), c(1,2,3,4,colm())]
        
  
      
      return(a) 
      
     
      
    })
    
    
    zipcode_data = reactive({
      
      summer_program_data = program_category_data()
      
      if(input$zipcode != "No zipcode selected" ) {
        
        a = subset(summer_program_data, summer_program_data$session_zip == input$zipcode & 
                     summer_program_data$session_cost  >= input$slider[1] & summer_program_data$session_cost  <= input$slider[2] )
        
      }
      
      else {
        
        a = subset(summer_program_data,  
                   summer_program_data$session_cost  >= input$slider[1] & summer_program_data$session_cost  <= input$slider[2]  )
        
      }
      
      return(a) 
      
      
    })
    
    
    
    
    output$datatable = DT::renderDataTable({
      
      
      data_table1 = zipcode_data()
      
      DT::datatable(data_table1, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      
      
    })
    

    
    
    output$mymap = renderLeaflet({
      
      zipcode_data1 = zipcode_data()
      
      leaflet(data = zipcode_data1) %>% addTiles() %>%
        addMarkers(~lon, ~lat)
      
      
  

      
    })
    
    
  })





