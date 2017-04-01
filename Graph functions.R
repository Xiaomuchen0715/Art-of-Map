#What do they have right now 
# https://public.tableau.com/profile/alleghenycountydhsdare#!/vizhome/UCRCityofPittsburgh2005-August2015/UCRDB
# https://pittsburghpa.shinyapps.io/BurghsEyeView/?_inputs_&dept_select=null&filter_select=%22%22&hier=null&map_bounds=%7B%22north%22%3A40.5696322335956%2C%22east%22%3A-79.5197296142578%2C%22south%22%3A40.2800495082348%2C%22west%22%3A-80.3986358642578%7D&map_center=%7B%22lng%22%3A-79.9594934304115%2C%22lat%22%3A40.4250801873534%7D&map_zoom=11&offense_select=null&origin_select=null&report_select=%22311%20Requests%22&req.type=null&result_select=null&search=%22%22&toggle311=true&toggleArrests=true&toggleAssets=true&toggleBlotter=true&toggleCitations=true&toggleViolations=true&usage_select=null&violation_select=null
library(ggplot2)
library(RColorBrewer)
#Basic Graph 
minidat$ShortCode[minidat$ShortCode=="13(a)"]<-"Controlled Substance"
minidat$ShortCode[minidat$ShortCode=="27"]<-"Assult"
minidat$ShortCode[minidat$ShortCode=="9"]<-"Inchoate"
minidat$ShortCode[minidat$ShortCode=="39"]<-"Theft"
minidat$ShortCode[minidat$ShortCode=="55"]<-"Riot"
minidat$ShortCode[minidat$ShortCode=="51"]<-"Obstructing Officials"
minidat$ShortCode[minidat$ShortCode=="35"]<-"Burglary"
minidat$ShortCode[minidat$ShortCode=="37"]<-"Robbery"
minidat$ShortCode[minidat$ShortCode=="49"]<-"Falsification and Intimidation"
minidat$ShortCode[minidat$ShortCode=="43"]<-"Offenses Against the Family"
minidat$ShortCode[minidat$ShortCode=="61"]<-"Firearms"
minidat$ShortCode[minidat$ShortCode=="59"]<-"Public Indecency"


display.brewer.all()

g3<-ggplot(minidat, aes(x=reorder(ShortCode, -table(ShortCode)[ShortCode]),fill=GENDER))+
  geom_bar(width=.5)+
  ggtitle("Offenses with Highest Frequencies")+xlab("Offense Types")+
  scale_fill_brewer(palette="Set3")
g3
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
    popup = paste("<b>Type of offense:</b>", "<b><font color=darkyellow>",minidat$ShortCode, "</b></font><br>",
                  "<b>Incident Location:</b>","<br>", 
                  "Adress:", minidat$INCIDENTLOCATION,"<br>",
                  "Neighborhood:", minidat$INCIDENTNEIGHBORHOOD, "<br>",
                  "<b>Suspect Profile:</b>", "<br",
                  "Race:", minidat$RACE, "<br>",
                  "Race:", minidat$RACE, "<br>",
                  "Gender:", minidat$GENDER, "<br>",
                  "Arrested at", minidat$ARRESTLOCATION, "at",minidat$ARRESTTIME),
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
  addCircleMarkers(lat=subset(DF,type=='Obstructing Officials')$lat,
                   lng=subset(DF,type=='Obstructing Officials')$lon,
                   radius = 6,
                   color = "aquamarine",
                   stroke = FALSE, fillOpacity = 0.5,group="Obstructing Officials")%>%
  addCircleMarkers(lat=subset(DF,type=='Burglary')$lat,
                   lng=subset(DF,type=='Burglary')$lon,
                   radius = 6,
                   color = "pink",
                   stroke = FALSE, fillOpacity = 0.5,group="Burglary")%>%
  addCircleMarkers(lat=subset(DF,type=='Robbery')$lat,
                   lng=subset(DF,type=='Robbery')$lon,
                   radius = 6,
                   color = "purple",
                   stroke = FALSE, fillOpacity = 0.5,group="Robbery")%>%
  addCircleMarkers(lat=subset(DF,type=='Falsification and Intimidation')$lat,
                   lng=subset(DF,type=='Falsification and Intimidation')$lon,
                   radius = 6,
                   color = "brown",
                   stroke = FALSE, fillOpacity = 0.5,group="Falsification and Intimidation")%>%
  addCircleMarkers(lat=subset(DF,type=='Offenses Against the Family')$lat,
                   lng=subset(DF,type=='Offenses Against the Family')$lon,
                   radius = 6,
                   color = "cyan",
                   stroke = FALSE, fillOpacity = 0.5,group="Offenses Against the Family")%>%
  addCircleMarkers(lat=subset(DF,type=='Firearms')$lat,
                   lng=subset(DF,type=='Firearms')$lon,
                   radius = 6,
                   color = "magenta",
                   stroke = FALSE, fillOpacity = 0.5,group="Firearms")%>%
  addCircleMarkers(lat=subset(DF,type=='Public Indecency')$lat,
                   lng=subset(DF,type=='Public Indecency')$lon,
                   radius = 6,
                   color = "turquoise",
                   stroke = FALSE, fillOpacity = 0.5,group="Public Indecency")%>%
  addLayersControl(
    baseGroups = c("Defult"),
    overlayGroups = c("Controlled Substance","Theft","Assult","Riot","Inchoate","Obstructing Officials",
                      "Burglary","Robbery","Falsification and Intimidation","Offenses Against the Family",
                      "Firearms","Public Indecency"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
map





