# load all necessary libraries

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
  na.omit()

# set up ui

ui <- fluidPage( # Application title
  titlePanel(title = "Marshall Violent Crime Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
                 "Welcome to an exploration of Marshall Violent Crime data",
                 hr(),
                 checkboxGroupInput("crime_type",
                                    "Crime Type",
                                    choices = c("Total Violent" = "violent_crime",
                                                "Homicide" = "homs_sum",
                                                "Rape" = "rape_sum",
                                                "Robbery" = "rob_sum",
                                                "Aggravated Assault" = "agg_ass_sum"),
                                    selected = "violent_crime"),
                 selectInput("department_name", "Select Regions:", choices = crimeData$department_name, selected = "Chicago"),

                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015,
                             value = c(2000,2010)),
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Line" = "geom_line",
                                          "Bar" = "geom_bar"), 
                              selected = "geom_point"),
                 colourInput('pointColor', 'Point Color', value = '#000'),
                 numericInput("alpha", "Transparency",
                              min = 0, max = 1,
                              value = 0.5, step = 0.1),
                 textInput("title", "Plot Title", value = "Crime Data vs Year")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("theFirstPlot"),
      
      plotOutput("theSecondPlot"),
      tableOutput("checkboxValue")
    ))
)

server <- function(input, output) {
  
  output$checkboxValue <- renderTable({
    crimeDataFilt <- crimeData2 %>%
      rename("violent_sum" = "violent_crime") %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) %>%
      filter(department_name %in% input$department_name) 
    
    
    columns <- c()
    
    for (ict in 1:length(input$crime_type)) {
      col <- strsplit(input$crime_type[ict], '_')
      if (length(col[[1]]) <= 2) {
        col <- col[[1]][1]
      } else {
        col <- paste0(col[[1]][1], '_', col[[1]][2])
      }
      columns <- c(columns, paste0(col, '_sum'), paste0(col, '_per_100k'))
    }
    
    crimeDataFilt[, c('year', columns)]
  })
  

  
}

shinyApp(ui = ui, server = server)