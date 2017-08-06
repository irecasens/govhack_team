# server.R

library(shiny)
library(maps)
library(mapproj)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggmap)
library(ggthemes)

tidy_viz = read.csv('tidy_viz.csv')
map <- get_map(location = 'Melbourne', maptype = "satellite", zoom = 15)
create_map <- function(data)
{
    ggmap(map, extent = "device") + geom_density2d(data = data, 
                                                   aes(x = LNG, y = LAT), size = 0.3) + stat_density2d(data = data, 
                                                                                                       aes(x = LNG, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                       bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0, 0.3), guide = FALSE) 
    
}


shinyServer(
    function(input, output) {
        
        output$map <- renderPlot({
            var <- switch(input$Measure_ID, 
                              "Pedestrian Count" = "hourly_count",
                              "Vehicles Count" = "vehicles_count",
                              "Vehicle and Pedestrian Density" = "veh_ped_density")
            
           
           
            filtered <- filter(tidy_viz, day_type ==  input$Day_ID 
                               & risk >= input$Risk_ID[1] & risk <= input$Risk_ID[2] 
                               & year >= input$Year_ID[1] & year <= input$Year_ID[2] 
                               & time >= input$Time_ID[1] & time <= input$Time_ID[2]) 
            
            if(var != "veh_ped_density"){
               filtered <- filter(filtered, measure == var)
            }
            
            
            create_map(filtered)
            
  
        })
    }
)