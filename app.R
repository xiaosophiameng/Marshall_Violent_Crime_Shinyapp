# load all necessary libraries

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(colourpicker)
library(lubridate)
library(plotly)
library(viridis)

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

                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015, step = 1,
                             value = c(2000,2010), sep = ""),
                 h4("Inputs for Figure 1:"),
                 radioButtons("crime_type_only_one",
                              label = "Select Crime Type:",
                              choices = c("Total Violent" = "violent_crime",
                                          "Homicide" = "homs_sum",
                                          "Rape" = "rape_sum",
                                          "Robbery" = "rob_sum",
                                          "Aggravated Assault" = "agg_ass_sum"),
                              selected = "violent_crime"),
                 selectInput("city", "Select Region:", multiple = FALSE,
                             #choices = unique(as.character(crimeData2$department_name)), 
                             sort(unique(crimeData2$department_name)),selected = "Chicago"),
                 hr(),
                 h4("Inputs for Figure 2:"),
                 checkboxGroupInput("crime_type",
                                    "Select Crime Type (multiple selections allowed):",
                                    choices = c("Total Violent" = "violent_crime",
                                                "Homicide" = "homs_sum",
                                                "Rape" = "rape_sum",
                                                "Robbery" = "rob_sum",
                                                "Aggravated Assault" = "agg_ass_sum"),
                                    selected = "violent_crime"),
                 selectInput("department_name", "Select Region (multiple selections allowed):", multiple = TRUE,
                             #choices = unique(as.character(crimeData2$department_name)), 
                             sort(unique(crimeData2$department_name)),selected = "Chicago"),
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Smooth" = "geom_smooth"), 
                              selected = "geom_smooth"),
                 numericInput("alpha", "Choose transparency for point:",
                              min = 0, max = 1,
                              value = 0.5, step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h4('Violent Crime Rate of Selected Region vs All'),
      #fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           #plotlyOutput("theFirstPlot"), plotlyOutput("theThirdPlot"))),
                
      
      h4('Selected Region in All'),
      plotlyOutput("theFirstPlot"),
      
      h4('Crime Rate of Selected Region'),
      plotOutput("theSecondPlot")
      
    ))
)

#set up server

server <- function(input, output) {
  
  # set up the first plot
  output$theFirstPlot <- renderPlotly({
    
    crimeData3 <- crimeData2 %>%
      #rename("violent_sum" = "violent_crime") %>% 
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) 
      
    
    
    #plot all and deffrenciate the selected region
    
    
    if (input$crime_type_only_one == "violent_crime"){
    plot2 <-
      ggplot(crimeData3, aes(year,violent_per_100k,text=(department_name))) +
      geom_path(aes(group=department_name, colour=department_name==input$city),
                se=FALSE,size=0.4)+
      scale_colour_manual("",
                          labels=c("other",input$city),
                          values = c("Grey","Red")
      )+
      theme(legend.position = "none")+
      ggtitle("Violent Crime Rate of Selected Region vs All")
    
    
    
    plot3 <- ggplotly(plot2,tooltip=c("x","text"))}
    
    if (input$crime_type_only_one == "homs_sum"){
      plot2 <-
        ggplot(crimeData3, aes(year,homs_per_100k,text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme(legend.position = "none")+
        ggtitle("Homicide Crime Rate of Selected Region vs All")
      
      
      
      plot3 <- ggplotly(plot2,tooltip=c("x","text"))}    

    if (input$crime_type_only_one == "rape_sum"){
      plot2 <-
        ggplot(crimeData3, aes(year,rape_per_100k,text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme(legend.position = "none")+
        ggtitle("Rape Crime Rate of Selected Region vs All")
      
      
      
      plot3 <- ggplotly(plot2,tooltip=c("x","text"))}            
    
    if (input$crime_type_only_one == "rob_sum"){
      plot2 <-
        ggplot(crimeData3, aes(year,rob_per_100k,text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme(legend.position = "none")+
        ggtitle("Robbery Crime Rate of Selected Region vs All")
      
      
      
      plot3 <- ggplotly(plot2,tooltip=c("x","text"))}      
    
    if (input$crime_type_only_one == "agg_ass_sum"){
      plot2 <-
        ggplot(crimeData3, aes(year,agg_ass_per_100k,text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme(legend.position = "none")+
        ggtitle("Aggravated Assault Crime Rate of Selected Region vs All")
      
      
      
      plot3 <- ggplotly(plot2,tooltip=c("x","text"))} 
    
    plot3
    
  })
  
  # set up the second output plot
  output$theSecondPlot <- renderPlot({
    
    if (is.null(input$department_name)){
      return(NULL)
    }
    
    crimeData3 <- crimeData2 %>%
      #rename("violent_sum" = "violent_crime") %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) %>%
      filter(department_name %in% input$department_name) 

    
    crimeDataCountPlot <- ggplot(crimeData3)
    colors <- c('#dd1c77', '#c994c7', '#e7e1ef', '#e6550d', '#fdae6b')
    for (ict in 1:length(input$crime_type)) {
      col <- strsplit(input$crime_type[ict], '_')
      
      if (length(col[[1]]) <= 2) {
        col <- col[[1]][1]
      } else {
        col <- paste0(col[[1]][1], '_', col[[1]][2])
      }
      y <- paste0(col, '_per_100k') #output crime rates
      title <- paste0('Rate of ', col, ' crime')
      

      if(input$geom == "geom_point"){
        crimeDataCountPlot <- 
          crimeDataCountPlot + 
          geom_point(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha) +
          facet_wrap(~ input$department_name)
      }else if (input$geom == "geom_smooth"){
        crimeDataCountPlot <- 
          crimeDataCountPlot + 
          geom_smooth(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha, se = FALSE )+
          facet_wrap(~ input$department_name)
        }
      

    }
    
    crimeDataCountPlot + 
      theme(legend.position = "bottom") 
  })

  
}

#launch the app
shinyApp(ui = ui, server = server)