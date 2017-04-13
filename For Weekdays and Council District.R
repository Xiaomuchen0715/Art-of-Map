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

Pittsburgh<-readOGR("map.geojson")
Neighborhood<-readOGR("NeighborhoodsSNAP.geojson")

#set color for different council districts
paltotal <- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$TotalCount)

palSun <- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$SunPercent)

palMon<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$MonPercent)

palTue<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$TuePercent)

palWed<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$WedPercent)

palThur<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$ThurPercent)

palFri<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$FriPercent)

palSat<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$SatPercent)


#Map for total counts
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.9,
                  fillColor = ~paltotal(TotalCount),
                  group="Total")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palSun(SunPercent),
              group="Sun.")%>%
  addPolygons(data=Pittsburgh,color = "gray24", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palMon(MonPercent),
              group="Mon.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palTue(TuePercent),
              group="Tue.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palWed(WedPercent),
              group="Wed.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palThur(ThurPercent),
              group="Thurs.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palFri(FriPercent),
              group="Fri.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palSat(SatPercent),
              group="Sat.")%>%
  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c("Total","Sun.","Mon.","Tue.",
                      "Wed.","Thurs.","Fri.","Sat."),
    options = layersControlOptions(collapsed = T)
  )
  
