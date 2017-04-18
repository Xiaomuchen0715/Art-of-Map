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
Pittsburgh

Pittsburgh<-readOGR("map.geojson")

write.csv(Neighborhood,file="Neighborhood.csv")
dat<-read.csv("Neighborhood.csv")
View(dat)
Neighborhood<-readOGR("NeighborhoodsSNAP.geojson")
Pittsburgh$WeekendPercent<-Pittsburgh$WeekendCount/sum(Pittsburgh$WeekendCount)*100
Pittsburgh$WorkdayPercent<-Pittsburgh$WorkdayCount/sum(Pittsburgh$WorkdayCount)*100
Pittsburgh$MidnightPercent<-(Pittsburgh$X01Count+Pittsburgh$X02Count)/2
Pittsburgh$DaytimePercent<-(Pittsburgh$X16Count+Pittsburgh$X17Count)/2
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

palWeekend<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$WeekendPercent)

palWorkday<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$WorkdayPercent)

palDaytime<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$DaytimePercent)

palMidnight<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$MidnightPercent)

(Pittsburgh$X01Count+Pittsburgh$X02Count)/2
#Map for total counts
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
  addPolylines(data=Neighborhood,color = "gray", weight = 2, smoothFactor = 1,
               opacity = 1, fillOpacity = 1,
               label = ~Neighborhood_2010_HOOD,
               fillColor = NULL,group="Neighborhood")%>%
  addPolylines(data=Pittsburgh, color="black",weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = NULL,
              group="Council")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.9,
                  fillColor = ~paltotal(TotalCount),
                  group="Total")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~palSun(SunPercent),
              group="Sun.")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
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
    overlayGroups = c("Base Map","Total","Sun.","Mon.","Tue.",
                      "Wed.","Thurs.","Fri.","Sat.","Neighborhood","Council"),
    options = layersControlOptions(collapsed = T)
  )
  
#set color for different council districts

pal23<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X23Count)

pal0<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X00Count)

pal01<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X01Count)


pal02<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X02Count)

pal03<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X03Count)

pal04<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X04Count)

pal05<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X05Count)

pal06<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X06Count)

pal07<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X07Count)

pal08<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X08Count)

pal09<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X09Count)

pal10<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X10Count)

pal11<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X11Count)

pal12<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X12Count)

pal13<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X13Count)

pal14<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X14Count)

pal15<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X15Count)

pal16<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X16Count)

pal17<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X17Count)

pal18<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X18Count)

pal19<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X19Count)

pal20<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X20Count)

pal21<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X21Count)

pal22<- colorNumeric(
  palette = "Blues",
  domain = Pittsburgh$X22Count)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal23(X23Count),
              group="23")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal0(X00Count),
              group="00")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal01(X01Count),
              group="01")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal02(X02Count),
              group="02")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal03(X03Count),
              group="03")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal04(X04Count),
              group="04")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal05(X05Count),
              group="05")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal06(X06Count),
              group="06")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal07(X07Count),
              group="07")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal08(X08Count),
              group="08")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal09(X09Count),
              group="09")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal10(X10Count),
              group="10")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal11(X11Count),
              group="11")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal12(X12Count),
              group="12")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal13(X13Count),
              group="13")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal14(X14Count),
              group="14")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal15(X15Count),
              group="15")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal16(X16Count),
              group="16")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal17(X17Count),
              group="17")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal18(X18Count),
              group="18")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal19(X19Count),
              group="19")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal20(X20Count),
              group="20")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal21(X21Count),
              group="21")%>%
  addPolygons(data=Pittsburgh,color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 0.9,
              fillColor = ~pal22(X22Count),
              group="22")%>%
  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c("23","00","01","02","03","04","05",
                      "06","07","08","09","10","11",
                      "12","13","14","15","16","17","18",
                      "19","20","21","22"),
    options = layersControlOptions(collapsed = T)
  )


leaflet(Pittsburgh) %>%
  addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
  addPolygons(color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 1,
              fillColor = ~palWeekend(WeekendPercent),
              group="Weekend")%>%
  addPolygons(color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 1,
              fillColor = ~palWorkday(WorkdayPercent),
              group="Workday")%>%
  addPolygons(color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 1,
              fillColor = ~palDaytime(DaytimePercent),
              group="Daytime")%>%
  addPolygons(color = "gray", weight = 2, smoothFactor = 1,
              opacity = 1, fillOpacity = 1,
              fillColor = ~palMidnight(MidnightPercent),
              group="Midnight")%>%
  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c("Weekend","Workday","Daytime","Midnight"),
    options = layersControlOptions(collapsed = T))%>%
  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c("","Workday","Daytime","Midnight"),
    options = layersControlOptions(collapsed = T))

Pittsburgh$DaytimePercent
Pittsburgh$MidnightPercent
Pittsburgh$council


leaflet(Neighborhood) %>%
  addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
  addPolylines(color = "black", weight = 2, smoothFactor = 1,
              opacity = 1)
