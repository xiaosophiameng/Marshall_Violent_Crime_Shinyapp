# load all necessary libraries

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(colourpicker)
library(lubridate)
library(plotly)

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
                 selectInput("department_name", "Region in interest:", 
                             choices = as.character(crimeData2$department_name), selected = "Chicago"),

                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015,
                             value = c(2000,2010)),
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Smooth" = "geom_smooth"), 
                              selected = "geom_point"),
                 numericInput("alpha", "Transparency",
                              min = 0, max = 1,
                              value = 0.5, step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h4('Violent Crime Rate of Selected Region vs All'),
      #fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           #plotlyOutput("theFirstPlot"), plotlyOutput("theThirdPlot"))),
                
      
      h4('Violent Crime Rate of Selected Region vs All'),
      plotlyOutput("theFirstPlot"),
      
      #h4('lala'),
      #plotOutput("theThirdPlot"),
      
      h4('Crime Rate of Selected Region'),
      plotOutput("theSecondPlot"),
      
      h4('Crime Details of Selected Region'),
      tableOutput("checkboxValue")
    ))
)

#set up server

server <- function(input, output) {
  
  # set up the first plot
  output$theFirstPlot <- renderPlotly({
    
    crimeData3 <- crimeData2 %>%
      rename("violent_sum" = "violent_crime") %>% 
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) 
    
    
    crimeData4 <- crimeData3 %>%
      filter(violent_sum < 1e5) %>%
      group_by(department_name) 
    
    
    #plot all and deffrenciate the selected region
    
    
    
    plot2 <-
      ggplot(crimeData3, aes(year,violent_per_100k,,text=(department_name))) +
      geom_path(aes(group=department_name, colour=department_name==input$department_name),
                se=FALSE,size=0.4)+
      scale_colour_manual("",
                          labels=c("other",input$department_name),
                          values = c("Grey","Red")
      )+
      theme(legend.position="bottom")
    
    
    
    plot3 <- ggplotly(plot2,tooltip=c("x","text")) 
    
    
    plot3
    
  })
  
  # set up the second output plot
  output$theSecondPlot <- renderPlot({
    
    if (is.null(input$department_name)){
      return(NULL)
    }
    
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
      y <- paste0(col, '_per_100k')
      title <- paste0('Rate of ', col, ' crime')
      

      if(input$geom == "geom_point"){
        crimeDataCountPlot <- crimeDataCountPlot + geom_point(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha)
      }else if (input$geom == "geom_smooth"){
        crimeDataCountPlot <- crimeDataCountPlot + geom_smooth(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha)
        }
      

    }
    
    crimeDataCountPlot + scale_color_manual("crime type", values=c(violent_per_100k=colors[1],
                                                                   homs_per_100k=colors[2],
                                                                   rape_per_100k=colors[3],
                                                                   rob_per_100k=colors[4],
                                                                   agg_ass_per_100k=colors[5]))
  })
 
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
  
}

#launch the app
shinyApp(ui = ui, server = server)