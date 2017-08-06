# ui.R

shinyUI(fluidPage(
    titlePanel("GovHack 2017 - Melbourne Growth"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Melbourne map with vehicles and pedestrian density"),
            
            radioButtons("Measure_ID", 
                        label = "Choose a variable to display",
                        choices = c("Pedestrian Count", "Vehicles Count", 
                                    "Vehicle and Pedestrian Density"),
                        selected = "Pedestrian Count"),
            
            
            radioButtons("Day_ID", 
                         label = "Choose day type",
                         choices = c("workday", "weekend"),
                         selected = "workday"),
            
            sliderInput("Risk_ID", 
                        label = "Risk Level:",
                        min = 1, max = 5, value = c(4, 5)),
   
            
            sliderInput("Year_ID", 
                        label = "Years",
                        min = 2014, max = 2017, value = c(2014, 2017)),
            
            sliderInput("Time_ID", 
                        label = "Time of Day:",
                        min = 0, max = 23, value = c(0,23))
            
            
            ),
        
        mainPanel(plotOutput("map"))
    )
))