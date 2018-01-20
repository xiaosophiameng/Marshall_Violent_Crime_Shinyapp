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
                 selectInput("department_name", "Select Regions:", choices = crimeData2$department_name, selected = "Chicago"),

                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015,
                             value = c(2000,2010)),
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Smooth" = "geom_smooth"), 
                              selected = "geom_point"),
                 numericInput("alpha", "Transparency",
                              min = 0, max = 1,
                              value = 0.5, step = 0.1),
                 textInput("title", "Plot Title", value = "Crime Data vs Year")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h4('Selected Region vs All'),
      plotOutput("theFirstPlot"),
      
      h4('Crime Rate of Selected Region'),
      plotOutput("theSecondPlot"),
      
      h4('Crime Details of Selected Region'),
      tableOutput("checkboxValue")
    ))
)

server <- function(input, output) {
  
  # set up the output table
  output$checkboxValue <- renderTable({
    crimeData3 <- crimeData2 %>%
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
    
    crimeData3[, c('year', columns)]
  })
  
  # set up the second output plot
  output$theSecondPlot <- renderPlot({
    
    crimeData3 <- crimeData2 %>%
      rename("violent_sum" = "violent_crime") %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) %>%
      filter(department_name %in% input$department_name) 

    
    crimeDataCountPlot <- ggplot(crimeData3)
    colors <- c('red', 'blue', 'green', 'yellow', 'cyan')
    for (ict in 1:length(input$crime_type)) {
      col <- strsplit(input$crime_type[ict], '_')
      
      if (length(col[[1]]) <= 2) {
        col <- col[[1]][1]
      } else {
        col <- paste0(col[[1]][1], '_', col[[1]][2])
      }
      y <- paste0(col, '_sum')
      title <- paste0('Count of ', col, ' crime')
      

      if(input$geom == "geom_point"){
        crimeDataCountPlot <- crimeDataCountPlot + geom_point(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha)
      }else if (input$geom == "geom_smooth"){
        crimeDataCountPlot <- crimeDataCountPlot + geom_smooth(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha)
        }
      

    }
    
    crimeDataCountPlot + scale_color_manual("crime type", values=c(violent_sum=colors[1],
                                                                   homs_sum=colors[2],
                                                                   rape_sum=colors[3],
                                                                   rob_sum=colors[4],
                                                                   agg_ass_sum=colors[5]))
  })
  
  # set up the first plot
  output$theFirstPlot <- renderPlot({
    crimeData3 <- crimeData2 %>%
      rename("violent_sum" = "violent_crime")
    crimeData3 %>%
      filter(violent_sum < 1e6) %>%
      group_by(department_name) %>%
      ggplot(aes(year, violent_sum, color = department_name)) +
      geom_path() +
      theme(legend.position = 'none')

    
  })
  
}

shinyApp(ui = ui, server = server)