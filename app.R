require(shiny)
require(curl)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(plyr)
library(jsonlite)
library(ggplot2)
library(scales)

#Create object for "Crime" dropdown menu
labels <- c("All","Homicide","Rape","Robbery","Aggravated Assault","Burglary","Larceny","Auto Theft","Assault","Sex Offense","DUI")

#Create object to use for map legend
maplabels <- c("Homicide","Rape","Robbery","Aggravated Assault","Burglary","Larceny","Auto Theft","Assault","Sex Offense","DUI")
cl <- c("01","02","03","04","05","06","07","08","17","28")
labeldf <- data.frame(maplabels, cl)

#Define the UI
ui <- bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("map", width = "100%", height = "100%"),
      #Left Panel
      absolutePanel(top = 10, left = 50, class = "panel panel-default", 
                    bottom = "auto", height="auto", fixed = TRUE,
                    style = "opacity: 0.80;padding: 8px; border-bottom: 1px solid #CCC; background: #e5f2ff;",
                    h2("MoCo Crime Explorer"),
                    h4(textOutput("txt")),
                    dateRangeInput("date", "Date Range:",
                                   start  = Sys.Date()-30,
                                   end    = date(),
                                   min    = "2013-07-01",
                                   max    = date(),
                                   separator = " - "),
                    selectInput("class", "Crime", labels, selected = "All"),
                    plotOutput("dayofweek", height = 200),
                    plotOutput("hourofday", height = 200, width=320),
                    tags$b("Source: "), 
                    tags$a(href="https://data.montgomerycountymd.gov/developers/docs/crime","dataMontgomery"), tags$br(),
                    tags$br(), tags$i("This page is a ", tags$a(href="http://www.brandonkopp.com","brandonkopp.com"), " creation.")
                    ),
      #Right panel
      absolutePanel(top = 10, right = 10, width=320, class = "panel panel-default", 
                    bottom = "auto", height="auto", fixed = FALSE, draggable = TRUE,
                    style = "opacity: 0.80;padding: 2px; border-bottom: 1px solid #CCC; background: #e5f2ff;",
                    plotOutput("city", height = 400)
                    )
                )

#Define Server Function
server <- function(input, output) {
  
  #Download and clean data
  crime <- reactive({
    #Translate text entries in the dropdown menu to strings that can be used to filter dataset
    dataIn <- switch(input$class,
                     "All" = c("01","02","03","04","05","06","07","08","17","28"),
                     "Homicide" = "01",
                     "Rape" = "02",
                     "Robbery" = "03",
                     "Aggravated Assault" = "04",
                     "Burglary" = "05",
                     "Larceny" = "06",
                     "Auto Theft" = "07",
                     "Assault" = "08",
                     "Sex Offense" = "17",
                     "DUI" = "28") 
    
    #Write API Query. For more information see: https://dev.socrata.com/docs/queries/index.html
    #Query selects based on date range set by user and removes several high-frequency, less interesting crime types like minor drug offenses
    #The query was made more specific to limit the total number of records that had to be downloaded in order to get the subset of interest
    url <- paste0("https://data.montgomerycountymd.gov/resource/crime.json?$where=incident_type!=%272938%27%20AND%20incident_type!=%272942%27%20AND%20incident_type!=%272941%27%20AND%20incident_type!=%271834%27%20AND%20incident_type!=%271031%27%20AND%20start_date%20>%20%27",input$date[1],"%27%20AND%20start_date%20<%20%27",input$date[2],"%27&$limit=50000")
    #Retrieve data
    crime <- fromJSON(url)
    #Remove records that do not have a geotag
    crime <- crime[!is.na(crime$longitude), ]
    
    #Crime classifications are hierarchical and many of the specific codes are not useful (e.g., 0522 - BURG NO FORCE - RES/DAY)
    #Create a 2-digit classification at a higher level of aggregation (e.g., 05 - Burglary)
    crime$cl <- substr(crime$incident_type,1,2)
    
    #Subset the data based on the type of crime selected in the Crime dropdown
    crime <- subset(crime, cl %in% dataIn)
    
    #Merge the labels for the 2-digit crime classifications
    crime <- join(crime,labeldf, by='cl',type='left',match='all')
    crime$maplabels <- as.factor(crime$maplabels)
    
    #Convert latitude and longitude to numeric variables, the preferred for mat for Leaflet
    crime$longitude <- as.numeric(crime$longitude)
    crime$latitude <- as.numeric(crime$latitude)
    
    #Convert start_date to variable format recognizable by R
    crime$start_date <- as.POSIXct(crime$start_date, "%Y-%m-%dT%H:%M:%S", tz="EST")
    
    #Create variables for use in charts
    crime$dayofweek <- format(crime$start_date, "%a")    #By Weekday
    crime$hourofday <- format(crime$start_date, "%H")    #By Hour
    
    #Create global variable showing the number of rows (i.e., recorded crimes) in the dataset for reactive text
    numrow <<- nrow(crime)
    
    #Return downloaded and cleaned dataset to variable crime() for use in other functions
    return(crime)
  })
  
  output$map <- renderLeaflet({
      
     #Create Legend for Map
      legdf <- data.frame(unique(crime()[, "cl"]), unique(crime()[, "maplabels"]))
      legdf <- arrange(legdf,unique.crime......cl...)
      
      #Create popup that appears when a crime location is clicked on
      popup <-  with(crime(),paste(sep = "",
                                        "<b><h4>",narrative,"</h4></b>",
                                        "<b>Address: </b> ",location,"<br/>",
                                        "<b>Date/Time: </b> ",start_date,"<br/>",
                                        "<b>Place: </b> ",place,"<br/>"))
      
      #Set up color scheme
      col <- colorFactor(brewer.pal(nrow(legdf), "Paired"), domain= legdf$unique.crime......cl...)
      
      #There are two map options, one for displaying all crimes and one for displaying individual crimes
      #The main difference is the addition of the legend in the all crimes map
      if(input$class =="All") {
      leaflet(crime()) %>%
        addProviderTiles("CartoDB.Positron") %>%
          addCircles(lng = crime()$longitude, lat = crime()$latitude, popup= popup, 
                   weight = 8, radius=8, color= col(crime()$cl), stroke = TRUE, fillOpacity = 1) %>%
          addLegend("bottomright", colors= brewer.pal(nrow(legdf), "Paired"), 
                  labels= legdf$unique.crime......maplabels...,opacity = 0.5)
      }else {
        leaflet(crime()) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addCircles(lng = crime()$longitude, lat = crime()$latitude, popup= popup, 
                     weight = 8, radius=8, color= col(crime()$cl), stroke = TRUE, fillOpacity = 1)
      }
      
    })

  #Create plot for crimes by hour of the day
  output$hourofday <- renderPlot ({
    ggplot(data=as.data.frame(prop.table(table(crime()[, "hourofday"])))) +
      geom_bar(stat="identity",aes(x=Var1, y=Freq), fill="lightyellow3", color='black', size=.3) +
      geom_hline(yintercept = .0416, linetype=3) +
      scale_x_discrete(limits=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')) +
      scale_y_continuous(expand = c(0,0), labels=percent) +
      labs(
        title = "Percentage of Crimes by Hour of the Day",
        x = "Hour of the Day",
        y = "Count"
      ) +
      theme(plot.title = element_blank(), #element_text(size = 14, face="bold", color = "black"),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black", size=.75),
            axis.text = element_text(size = 12, color="black"),
            axis.text.x = element_text(angle = 90, vjust=0.5,hjust=0.5),
            axis.title = element_text(size = 14, face="bold", color = "black"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank())
    
  }, bg="transparent")
  
  #Create plot for crimes by day of the week
  output$dayofweek <- renderPlot({
    ggplot(data=as.data.frame(prop.table(table(crime()[, "dayofweek"])))) +
      geom_bar(stat="identity",aes(x=Var1, y=Freq), fill="darkolivegreen3", color='black', size=.3) +
      geom_hline(yintercept = .1428, linetype=3) +
      scale_x_discrete(limits=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')) +
      scale_y_continuous(expand = c(0,0), labels = percent) +
      labs(
        title = "Percentage of Crimes by...",
        x = "Day of Week",
        y = "Percent"
      ) +
      theme(plot.title = element_text(size = 18, face="bold", color = "black"),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black", size=.75),
            axis.text = element_text(size = 12, color="black"),
            axis.title = element_text(size = 14, face="bold", color = "black"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank())
  }, bg="transparent")
  
  #Create plot for crimes by city
  output$city <- renderPlot ({
    ggplot(data=as.data.frame(prop.table(table(crime()[, "city"])))) +
      geom_bar(stat="identity",aes(x=Var1, y=Freq), fill="lavenderblush2", color='black', size=.3) +
      scale_y_continuous(expand = c(0,0), labels=percent) +
      labs(title = "Percentage of Crimes by City") +
      theme(plot.title = element_text(size = 18, face="bold", color = "black", hjust=1),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black", size=.75),
            axis.text = element_text(size = 12, color="black"),
            axis.text.x = element_text(vjust=0.5,hjust=0.5),
            axis.title = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank()) +
      coord_flip()
  }, bg="transparent")
  
  #Change text for "XXXX Crimes Displayed" whenever the date or crime type changes
  observeEvent(input$class, {
    output$txt <- renderText(paste0(numrow[1], " Crimes Displayed"))
  })   
  observeEvent(input$date, {
    output$txt <- renderText(paste0(numrow[1], " Crimes Displayed"))
  })  
} 

  shinyApp(ui = ui, server = server)