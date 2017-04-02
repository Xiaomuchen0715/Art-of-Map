#What do they have right now 
# https://public.tableau.com/profile/alleghenycountydhsdare#!/vizhome/UCRCityofPittsburgh2005-August2015/UCRDB
# https://pittsburghpa.shinyapps.io/BurghsEyeView/?_inputs_&dept_select=null&filter_select=%22%22&hier=null&map_bounds=%7B%22north%22%3A40.5696322335956%2C%22east%22%3A-79.5197296142578%2C%22south%22%3A40.2800495082348%2C%22west%22%3A-80.3986358642578%7D&map_center=%7B%22lng%22%3A-79.9594934304115%2C%22lat%22%3A40.4250801873534%7D&map_zoom=11&offense_select=null&origin_select=null&report_select=%22311%20Requests%22&req.type=null&result_select=null&search=%22%22&toggle311=true&toggleArrests=true&toggleAssets=true&toggleBlotter=true&toggleCitations=true&toggleViolations=true&usage_select=null&violation_select=null
install.packages("leaflet")
install.packages("shinydashboard")
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(shiny)
library(lubridate)
library(shinydashboard)
library(jsonlite)
#funtion for label
label<-function(frames){
  frames$ShortCode[frames$ShortCode=="27"]<-"Assult"
  frames$ShortCode[frames$ShortCode=="13(a)"]<-"Possessing Controlled Substance"
  frames$ShortCode[frames$ShortCode=="9"]<-"Inchoate Crimes"
  frames$ShortCode[frames$ShortCode=="39"]<-"Theft"
  frames$ShortCode[frames$ShortCode=="55"]<-"Riot and Related Offenses"
  frames$ShortCode[frames$ShortCode=="33"]<-"Arson, Criminal Mischef And Other Property Destructions"
  frames$ShortCode[frames$ShortCode=="35"]<-"Burglary"
  frames$ShortCode[frames$ShortCode=="49"]<-"Falsification and Intimidation"
  frames$ShortCode[frames$ShortCode=="37"]<-"Robbery"
  frames$ShortCode[frames$ShortCode=="61"]<-"Firearms and Other Dangerous Articles"
  frames$ShortCode[frames$ShortCode=="43"]<-"Offenses Against the Family"
  frames$ShortCode[frames$ShortCode=="31"]<-"Sexual Offenses"
  frames<-frames%>%
    mutate(Weekday=wday(frames$ARRESTTIME,label = TRUE, abbr = FALSE))%>%
    mutate(Hours=hour(frames$ARRESTTIME))%>%
    mutate(Date=as.Date(frames$ARRESTTIME))%>%
    mutate(incidentType=as.factor(frames$ShortCode))%>%
    mutate(content = paste("<b>Type of offense:</b>", "<b><font color=darkyellow>",frames$ShortCode, "</b></font><br>",
                           "<b>Incident Location:</b>","<br>", 
                           "Adress:", frames$INCIDENTLOCATION,"<br>",
                           "Neighborhood:", frames$INCIDENTNEIGHBORHOOD, "<br>",
                           "<b>Suspect Profile:</b>", "<br",
                           "Race:", frames$RACE, "<br>",
                           "Race:", frames$RACE, "<br>",
                           "Gender:", frames$GENDER, "<br>",
                           "Arrested at", frames$ARRESTLOCATION, "at",frames$ARRESTTIME))
  
  return(frames)
}

minidata<-label(minidata)


#map
#The color system we use 
#Types
#Assult: Red
#13(a): Blue
#9: Orange
#39: Purple

#Race
#White:"White"
#Black:"Black"
#Other:"Gray"

#Age
#lasttime check the logitude and latitude
plot(minidata$X,minidata$Y)
table(minidata$X[minidata$X==0])
table(minidata$X[minidata$Y==0])
#clearly somethingwrong about the locations with 0 latitude and 0 logitude
#we find the reason is the address is without housenumber 
#But actually we could approximately identity the address
locationClean<-function(emtylocations,x,y){
  emtylocations<-filter(emtylocations,X==0)
  locations<-emtylocations$INCIDENTLOCATION
  x<-NULL
  y<-NULL
  locations<-gsub(" ","+",locations)
  locations<-gsub(",","",locations)
  raw.data<-NULL
  geodat<-NULL
  emtylocations$INCIDENTLOCATION
  for (address in locations){
    # Connecting to the Google Maps to return coordinates given a location name
    root <- "https://maps.googleapis.com/maps/api/geocode/json?"
    url <- paste0(root,"address=",address,"+CA&key=AIzaSyAvui-obPU4xD8SslouCsDFoFtM-86ScCw")
    raw.data <- readLines(url)
    geodat <- fromJSON(raw.data)
    print(geodat)
    ifelse(typeof(geodat$results$geometry$location$lat[1])=="NULL",y<-c(y,0),
           y<-c(y,geodat$results$geometry$location$lat[1]))
    ifelse(typeof(geodat$results$geometry$location$lng[1])=="NULL",x<-c(x,0),
           x<-c(x,geodat$results$geometry$location$lng[1]))
    xy<-data.frame(x,y)
    }
  return(xy)
}

xy<-locationClean(minidata,minidata$X[minidata$X==0],
                        minidata$Y[minidata$Y==0])

nrow(minidata)
#4982

#fit the latitude and longitude in empty part
together<-function(frames,xy){
  frames<-filter(frames,!X==0) 
  set<-filter(minidata,X==0)
  set$X<-xy$x
  set$Y<-xy$y
  frames<-rbind(frames,set)
  frames<-filter(frames,!X==0)
  return(frames)
}
minidata<-together(minidata,xy)


arrestlocation<-function(locations){
  locations<-locations$ARRESTLOCATION
  locations<-gsub(" ","+",locations)
  locations<-gsub(",","",locations)
  latitude<-NULL
  longitude<-NULL
  raw.data<-NULL
  geodat<-NULL
  for (address in locations){
    # Connecting to the Google Maps to return coordinates given a location name
    root <- "https://maps.googleapis.com/maps/api/geocode/json?"
    url <- paste0(root,"address=",address,"+CA&key=AIzaSyAvui-obPU4xD8SslouCsDFoFtM-86ScCw")
    raw.data <- readLines(url)
    geodat <- fromJSON(raw.data)
    latitude<-c(latitude,geodat$results$geometry$location$lat[1])
    longitude<-c(longitude,geodat$results$geometry$location$lng[1])
    ArrstXY<-data.frame(latitude,longitude)
  }
  return(ArrstXY)
}
#But R is too slow for this 
ArrestLocation<-arrestlocation(minidata)
  
