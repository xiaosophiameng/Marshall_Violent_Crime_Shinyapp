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
  titlePanel(title = "Marshall Violent Crime Analysis"),
   
   
   # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 "Welcome to an exploration of Marshall Violent Crime data",
                 hr(),
                 checkboxGroupInput("crime_type",
                                    "Crime Type",
                                    choices = c("Total Violent" = "sum_violent",
                                                "Homicide" = "homs_sum",
                                                "Rape" = "rape_sum",
                                                "Robbery" = "rob_sum",
                                                "Aggravated Assault" = "agg_ass_sum"),
                                    selected = "violent_crime"),
                 selectInput("department_name", "Select Regions:", choices = crimeData2$department_name),
                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015,
                             value = c(2000,2010)),
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Line" = "geom_line",
                                          "Bar" = "geom_bar"), 
                              selected = "geom_point"),
                 textInput("title", "Plot Title", value = "Crime Data vs Year")
    ),
      
      # Show a plot of the generated distribution
    mainPanel(
      plotOutput("theFirstPlot"),
      
      plotOutput("theSecondPlot"),
      tableOutput("checkboxValue")
    ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #set up the output table
  output$checkboxValue <- renderTable({
    crimeData3 <- 
      crimeData2 %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) %>%
      filter(department_name %in% input$department_name) 
  }
)
}

# Run the application 
shinyApp(ui = ui, server = server)

