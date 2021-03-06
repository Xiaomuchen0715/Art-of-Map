library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet)
library(rgdal)
library(shiny)
library(lubridate)
library(shinydashboard)
library(jsonlite)
ArrestData <- read_csv("ArrestData.csv")
dat<-ArrestData
locdata<-filter(dat,X>-80.1&X< -79.5&Y<41&Y>40.3)#only 15411 samples in our dataset
locdata<-arrange(locdata,INCIDENTNEIGHBORHOOD)
b<-tapply(locdata$INCIDENTNEIGHBORHOOD,locdata$INCIDENTNEIGHBORHOOD,length)
e<-NULL
for(i in b){
  e<-c(e,rep(i,i))
}
locdata$NeighborCount<-as.numeric(e)
----------------------------------------
  #6476  samples left in our dataset
  nrow(locdata)

#add labels to the data
label<-function(frames){
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==2]<-"Division Two"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==1]<-"Division One"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==4]<-"Division Four"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==3]<-"Division Three"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==5]<-"Division Five"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==6]<-"Division Six"
  frames$PUBLIC_WORKS_DIVISION[frames$PUBLIC_WORKS_DIVISION==0]<-"Zero"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="6"]<-"District Six"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="1"]<-"District One"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="3"]<-"District Three"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="9"]<-"District Nine"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="2"]<-"District Two"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="4"]<-"District Four"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="7"]<-"District Seven"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="5"]<-"District Five"
  frames$COUNCIL_DISTRICT[frames$COUNCIL_DISTRICT=="8"]<-"District Eight"
  frames<-frames%>%
    mutate(Weekday=wday(frames$ARRESTTIME,label = TRUE, abbr = FALSE))%>%
    mutate(Hours=hour(frames$ARRESTTIME))%>%
    mutate(Date=as.Date(frames$ARRESTTIME))%>%
    mutate(content = paste("<b>Type of offense:</b>", "<b><font color=darkyellow>",frames$OFFENSES, "</b></font><br>",
                           "<b>Incident Location:</b>","<br>", 
                           "Adress:", frames$INCIDENTLOCATION,"<br>",
                           "Neighborhood:", frames$INCIDENTNEIGHBORHOOD, "<br>",
                           "District Council:", frames$COUNCIL_DISTRICT, "<br>",
                           "<b>Suspect Profile:</b>", "<br",
                           "Race:", frames$RACE, "<br>",
                           "Race:", frames$RACE, "<br>",
                           "Gender:", frames$GENDER, "<br>",
                           "Arrested at", frames$ARRESTLOCATION, "at",frames$ARRESTTIME))
  
  return(frames)
}
locdata<-label(locdata)

Pittsburgh<-readOGR("CityCouncilDistricts.geojson")
Neighborhood<-readOGR("NeighborhoodsSNAP.geojson")

#set choice for different code
numericPalette <- colorNumeric(c("#f9c118", "#aee2d4", "#99c1cc","#7f7e5b",
                                 "#96d1ad","#e2c5b1","#e2d4b1","#c0dce2",
                                 "#ede4da"), domain=Pittsburgh$council)

#add widgets to the map
header <- dashboardHeader(title = p("Crime and Offenses of Pittsburgh"), titleWidth = 400)
dashboard <- column(width =4,
                    # width set to NULL when use column-based layout
                    box(width = NULL , title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
                        solidHeader = T, collapsible = T, status = 'primary',
                        # create a date-range-input widget
                        dateRangeInput('dates', label = "Date Range",width = 300,
                                       start = '2016-01-01', end = '2017-03-31',
                                       min = "2016-01-01", max = "2017-03-31"
                        ),
                        # create a select-input widget for day of week selection
                        selectizeInput('day_of_week','Days of Week', width = 300,
                                       choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                                       selected = c('Tuesday','Wednesday','Thursday'),
                                       multiple = T),
                        # create a slder-input widget for time of day selection
                        sliderInput('time_of_day','Time of Day', min = 0, max = 23,width = 300,
                                    value = c(0,23), step = 1),
                        sliderInput('age', label = "Criminal Age",min=1,max=120,width = 300,
                                    value =c(0,1999) ,step=1),
                        # create a submit-button for user explicitly confirm data input
                        submitButton(text = "Submit",icon =icon('filter'))
                    )
)

map <- column(width =8,
              box(width = NULL, solidHeader = TRUE,
                  leafletOutput('crimeMap',height = 500)))

body <- dashboardBody(
  fluidRow(
    dashboard, map
  )
)

ui<-shinyUI(dashboardPage(header,
                          sidebar = dashboardSidebar(disable = T),
                          body))

server<- shinyServer(function(input, output) {
  # Create a reactive expression to filter data set per user requests
  filteredData <- reactive({
    locdata%>%
      filter(Date > input$dates[1] & Date < input$dates[2]) %>%
      filter(Weekday %in% input$day_of_week) %>%
      filter(Hours >= input$time_of_day[1] & Hours <= input$time_of_day[2])%>%
      filter(AGE >= input$age[1] & AGE <=input$age[2])
  })
  # Use Leaflet to render crime map
  output$crimeMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$CartoDB.DarkMatter,group="Base Map") %>%
      addPolygons(data=Pittsburgh,color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.3,
                  fillColor = ~numericPalette(council),
                  group="Council Districts")%>%
      addPolylines(data=Neighborhood,color = "white", weight = 2, smoothFactor = 0.5,
                   opacity = 0.1, fillOpacity = 1,
                   fillColor = NULL,
                   label = ~Neighborhood_2010_HOOD,
                   highlightOptions = highlightOptions(color = "yellow", weight = 8,
                                                       bringToFront = F),group="Neighborhoods")%>%
      addCircleMarkers(
        ~X,
        ~Y,
        radius = ~NeighborCount/50,
        color = "#42f1f4",
        stroke = T, fillOpacity = 0.1,
        popup = ~content)%>%
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("Council Districts","Neighborhoods"),
        options = layersControlOptions(collapsed = F)
      )
  })
})


shinyApp(ui,server)
