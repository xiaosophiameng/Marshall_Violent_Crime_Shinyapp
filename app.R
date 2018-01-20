# load necessary libraries

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(colourpicker)
library(lubridate)

# read data and initial data wrangling 

crimeData <- read.csv("ucr_crime_1975_2015.csv")

crimeData2 <-
  crimeData %>% 
  select(-source,-url) %>% 
  na.omit() %>% 
  rename("sum_violent"="violent_crime") #make the format of names consistent; easy for future work

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   
   
   # Sidebar with a slider input for number of bins 

      
      # Show a plot of the generated distribution
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

