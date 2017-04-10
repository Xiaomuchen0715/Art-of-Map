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
#check the logitude and latitude
plot(minidata$X,minidata$Y)
table(minidata$X[minidata$X==0])

table(minidata$X[minidata$Y==0])
#clearly somethingwrong about the locations with 0 latitude and 0 logitude
#we find the reason is the address is without housenumber 
#We try to  identity latitude and logitide through  address

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

#However without street number the result from this API function is not accurate
#we decide to filter location based on the longitude and latitude of pittsburgh
dat<-filter(dat,X>-80.1&X< -79.5&Y<41&Y>40.3)
plot(dat$X,dat$Y)
#Thereore the database is slightly different from what
#we use for hit map
