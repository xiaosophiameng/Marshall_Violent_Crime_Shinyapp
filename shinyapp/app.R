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
                #select year range
                 sliderInput("year", "Select Years:",
                             min = 1975, max = 2015, step = 1,
                             value = c(2000,2010), sep = ""),
                 hr(),
                 h4("Inputs for Figure 1:"),
                #select crime type
                 radioButtons("crime_type_only_one",
                              label = "Select Crime Type:",
                              choices = c("Total Violent" = "violent_crime",
                                          "Homicide" = "homs_sum",
                                          "Rape" = "rape_sum",
                                          "Robbery" = "rob_sum",
                                          "Aggravated Assault" = "agg_ass_sum"),
                              selected = "violent_crime"),
                #select region
                 selectInput("city", "Select Region:", multiple = FALSE,
                             #choices = unique(as.character(crimeData2$department_name)), 
                             sort(unique(crimeData2$department_name)),selected = "Chicago"),
                 hr(),
                 h4("Inputs for Figure 2:"),
                #select crime type
                 checkboxGroupInput("crime_type",
                                    "Select Crime Type (multiple selections allowed):",
                                    choices = c("Total Violent" = "violent_crime",
                                                "Homicide" = "homs_sum",
                                                "Rape" = "rape_sum",
                                                "Robbery" = "rob_sum",
                                                "Aggravated Assault" = "agg_ass_sum"),
                                    selected = "violent_crime"),
                #select region
                 selectInput("department_name", "Select Region (multiple selections allowed):", multiple = TRUE,
                             #choices = unique(as.character(crimeData2$department_name)), 
                             sort(unique(crimeData2$department_name)),selected = "Chicago"),
                #select plot type
                 radioButtons("geom", "Geom", 
                              choices = c("Point" = "geom_point",
                                          "Smooth" = "geom_smooth"), 
                              selected = "geom_smooth"),
                #select transparency
                 numericInput("alpha", "Choose transparency for point:",
                              min = 0, max = 1,
                              value = 0.7, step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
                
      h4('Selected Region vs All'),
      plotlyOutput("oneRegionandAllPlot"),
      
      h4('Selected Regions Comparison'),
      plotOutput("RegionsComparisonPlot")
      
    ))
)

#set up server

server <- function(input, output) {
  
  # set up the first plot
  output$oneRegionandAllPlot <- renderPlotly({
    
    crimeData3 <- crimeData2 %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) 
      
    
    
    #plot all and deffrenciate the selected region
    
    #when users select violent_crime
    if (input$crime_type_only_one == "violent_crime"){
    comparison_plot <-
      ggplot(crimeData3, aes(year,round(violent_per_100k),text=(department_name))) +
      geom_path(aes(group=department_name, colour=department_name==input$city),
                se=FALSE,size=0.4)+
      scale_colour_manual("",
                          labels=c("other",input$city),
                          values = c("Grey","Red")
      )+
      theme_bw()+
      theme(legend.position = "none")+
      ggtitle("Violent Crime Rate of Selected Region vs All")+
      labs(y="Violent Crime per 100k")+
      theme(plot.title = element_text(hjust=0.5))
    
    
    # add tooltip
    tooltip_comparison_plot <- ggplotly(comparison_plot,tooltip=c("x","text","y"))}
    
    #when users select homs_sum
    if (input$crime_type_only_one == "homs_sum"){
      comparison_plot <-
        ggplot(crimeData3, aes(year,round(homs_per_100k),text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme_bw()+
        theme(legend.position = "none")+
        ggtitle("Homicide Crime Rate of Selected Region vs All")+
        labs(y="Homicide Crime per 100k")+
        theme(plot.title = element_text(hjust=0.5))
      
      
      # add tooltip 
      tooltip_comparison_plot <- ggplotly(comparison_plot,tooltip=c("x","text","y"))}    
    
    #when users select rape_sum
    if (input$crime_type_only_one == "rape_sum"){
      comparison_plot <-
        ggplot(crimeData3, aes(year,round(rape_per_100k),text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme_bw()+
        theme(legend.position = "none")+
        ggtitle("Rape Crime Rate of Selected Region vs All")+
        labs(y="Rape Crime per 100k")+
        theme(plot.title = element_text(hjust=0.5))
      
      
      # add tooltip
      tooltip_comparison_plot <- ggplotly(comparison_plot,tooltip=c("x","text","y"))}            
    
    #when users select rob_sum
    if (input$crime_type_only_one == "rob_sum"){
      comparison_plot <-
        ggplot(crimeData3, aes(year,round(rob_per_100k),text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme_bw()+
        theme(legend.position = "none")+
        ggtitle("Robbery Crime Rate of Selected Region vs All")+
        labs(y="Robbery Crime per 100k")+
        theme(plot.title = element_text(hjust=0.5))
      
      
      # add tooltip
      tooltip_comparison_plot <- ggplotly(comparison_plot,tooltip=c("x","text","y"))}      
    
    #when users select agg_ass_sum
    if (input$crime_type_only_one == "agg_ass_sum"){
      comparison_plot <-
        ggplot(crimeData3, aes(year,round(agg_ass_per_100k),text=(department_name))) +
        geom_path(aes(group=department_name, colour=department_name==input$city),
                  se=FALSE,size=0.4)+
        scale_colour_manual("",
                            labels=c("other",input$city),
                            values = c("Grey","Red")
        )+
        theme_bw()+
        theme(legend.position = "none")+
        ggtitle("Aggravated Assault Crime Rate of Selected Region vs All") +
        labs(y="Aggravated Assault Crime per 100k")+
        theme(plot.title = element_text(hjust=0.5))
        
      
      
      # add tooltip
      tooltip_comparison_plot <- ggplotly(comparison_plot,tooltip=c("x","text","y"))} 

    
    tooltip_comparison_plot
    

    
    
  })
  
  # set up the second output plot
  output$RegionsComparisonPlot<- renderPlot({
    
    if (is.null(input$department_name)){
      return(NULL)
    }
    
    crimeData3 <- crimeData2 %>%
      filter(year >= as.numeric(input$year[1]) & year <= as.numeric(input$year[2])) %>%
      filter(department_name %in% input$department_name) 

   #get y-axis 
    crimeDataCountPlot <- ggplot(crimeData3)
    for (ict in 1:length(input$crime_type)) {
      col <- strsplit(input$crime_type[ict], '_')
      
      if (length(col[[1]]) <= 2) {
        col <- col[[1]][1]
      } else {
        col <- paste0(col[[1]][1], '_', col[[1]][2])
      }
      y <- paste0(col, '_per_100k') #output crime rates
      title <- paste0('Rate of ', col, ' crime')
      
      #when users choose scatter plot
      if(input$geom == "geom_point"){
        crimeDataCountPlot <- 
          crimeDataCountPlot + 
          geom_point(aes_string('year', y, color = shQuote(y)),size = 2, alpha = input$alpha) +
          facet_wrap(~ input$department_name) 
      #when users choose smooth line plot   
      }else if (input$geom == "geom_smooth"){
        crimeDataCountPlot <- 
          crimeDataCountPlot + 
          geom_smooth(aes_string('year', y, color = shQuote(y)),size = 0.8, alpha = input$alpha, se = FALSE )+
          facet_wrap(~ input$department_name)
        }
      

    }
    
    # add colorblind-friendly colors
    crimeDataCountPlot + 
      scale_color_brewer(palette = "Set2")+
      theme_bw()+
      theme(legend.position = "bottom") +
      labs(y="crime rate")+
      ggtitle("Crime rates in selected regions")+
      theme(plot.title = element_text(hjust=0.5,face = "bold"),
            axis.title=element_text(size=12,face = "bold"))
  })

}

#launch the app
shinyApp(ui = ui, server = server)