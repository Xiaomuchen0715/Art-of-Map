#Basic Graph 
minidat$ShortCode[minidat$ShortCode=="13(a)"]<-"Controlled Substance"
minidat$ShortCode[minidat$ShortCode=="27"]<-"Assult"
minidat$ShortCode[minidat$ShortCode=="9"]<-"Inchoate"
minidat$ShortCode[minidat$ShortCode=="39"]<-"Theft"
minidat$ShortCode[minidat$ShortCode=="55"]<-"Riot"

table(minidat$AGE)
minidat<-filter(minidat,AGE<999)

library(ggplot2)
library(RColorBrewer)
display.brewer.all()

g1<-ggplot(minidat, aes(x=reorder(ShortCode, -table(ShortCode)[ShortCode]),fill=GENDER))+
  geom_bar(width=.5)+
  ggtitle("Offenses with Highest Frequencies")+xlab("Offense Types")+
  scale_fill_brewer(palette="Set3")
g1
#More males involved than females.

g2<-ggplot(minidat, aes(x=reorder(ShortCode, -table(ShortCode)[ShortCode]),fill=RACE))+
  geom_bar(width=.5)+
  ggtitle("Offenses with Highest Frequencies")+xlab("Offense Types")+
  scale_fill_brewer(palette="RdGy")
g2
#More Afican Americans involved than Whites
#exceplt for " "Controlled Substance, Drug, Device and Cosmetic Act"

#map
install.packages("leaflet")
library(leaflet)
library(sp)
library(rgdal)
DF <- data.frame(lat = minidat$Y, lon = minidat$X, type = minidat$ShortCode)

#set the basic map
map<-leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron,group="Defult")%>%
  addCircleMarkers(lat=subset(DF,type=='Assult')$lat,
                   lng=subset(DF,type=='Assult')$lon,
    radius = 6,
    color = "orange",
    stroke = FALSE, fillOpacity = 0.5,
    group = "Assult")%>%
  addCircleMarkers(lat=subset(DF,type=='Controlled Substance')$lat,
                   lng=subset(DF,type=='Controlled Substance')$lon,
                   radius = 6,
                   color = "green",
                   stroke = FALSE, fillOpacity = 0.5,group="Controlled Substance")%>%
  addCircleMarkers(lat=subset(DF,type=='Inchoate')$lat,
                   lng=subset(DF,type=='Inchoate')$lon,
                   radius = 6,
                   color = "red",
                   stroke = FALSE, fillOpacity = 0.2,group="Inchoate")%>%
  addCircleMarkers(lat=subset(DF,type=='Theft')$lat,
                   lng=subset(DF,type=='Theft')$lon,
                   radius = 6,
                   color = "blue",
                   stroke = FALSE, fillOpacity = 0.2,group="Theft")%>%
  addCircleMarkers(lat=subset(DF,type=='Riot')$lat,
                   lng=subset(DF,type=='Riot')$lon,
                   radius = 6,
                   color = "gray",
                   stroke = FALSE, fillOpacity = 0.5,group="Riot")%>%
  addLayersControl(
    baseGroups = c("Defult"),
    overlayGroups = c("Controlled Substance","Theft","Assult","Riot","Inchoate"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
map

