## Write Up for Milestone 2

### The app

[xiaosophiameng.shinyapps.io/Sophiashinyapp](https://xiaosophiameng.shinyapps.io/Sophiashinyapp/)

### Rationale for my design choices

#### As a whole

Background Recall: 
 
The app is based on Marshall Violent Crime Data. The dataset  contains the number and rate of 4 types of violent crimes in 68 police jurisdictions from year 1975 to year 2015, with population and other information.

I choose to display crime rates instead of the number of crimes in my app, because crime rate is the ratio of the number of crimes and total population of a region at a time. Crime rates better describes how safe/unsafe a region was at a time. Since the crime rate can be viewed as a continuous variable, smooth line can be used to show the distribution. 

#### Plot type and color/theme choice for the first plot

Plot type:

I choose the path line because it enables users to get the whole trend as well as detailed yearly change/figure for a region. Since violent crime rates of all regions are plotted in one graph, only one type of plot should be inserted. Path line is my best choice since it provides the big picture and necessary details for users to compare.

Color:

I choose red to represent the selected region and grey for the rest, because red and grey provide sufficient contrast. Compared to the color black, grey is more eye-friendly. 

#### Plot type for the second plot

I provide users with two types of plots: point and smooth line. Points allow users to view a solid number of the crime rate for a region at a particular year. Smooth line allows users to view the entire trend of a crime type. Users can modify the choices based on their needs of detail levels. Lines/Paths are not applied here since lines/paths provide unnecessary details when showing the entire trend in this case. 
  
### My visual has changed since my proposal


#### How have my visualization goals changed?

In my proposal, I planned to plot the crime rate and the number of crimes in a region. However, after thinking through, they provide similar information and the crime rate is a better choice since it incorporates the number of crimes and the population in a region at a time. Thus, I plots for crime rate only in this version. 

#### Does my app enable the tasks I set out to facilitate?

In addition, my proposal app does not facilitate users to visualize comparison. In this new version, I added a plot (*Violent Crime Rate of Selected Region vs All shown*) showing the rates of violent crime for all the other regions in color grey and the user-selected region in color red. This plot helps user compare the region he/she is interested in with the rest of the US, and get a big picture of the risk level of violent crime in the user's interested region. 
With the new version, Lingling (one of the users) is able to compare the crime rates of the region she is thinking of to the rest regions in the US. For instance, if her selected region locates on the bottom part in the plot, the risk level of violent crime in that region is lower than average. 

There are still limitation in my app. For example, Lingling cannot compare the crime rates of multiple regions she is interested in and visualize the trends of multiple regions in one plot. However, compared to my proposal, this new version improves and enables Lingling to compare one particular region to the rest of the US and get a high-level idea.    
 
