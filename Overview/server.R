library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape)
library(leaflet)
library(tidyr)
library(rgdal)
library(scales)
library(ggmap)
library(shiny)
library(RColorBrewer)
dat<-read_csv("ArrestData.csv")
heatdat<-filter(dat,weekdays(ARRESTTIME)>0&COUNCIL_DISTRICT>0)%>%
  mutate(Weekday=weekdays(ARRESTTIME),Hour=hour(ARRESTTIME))
order4<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
heatdat$Weekday<-factor(heatdat$Weekday,
                        levels=order4,ordered=TRUE)
order5<-1:9
heatdat$COUNCIL_DISTRICT<-factor(heatdat$COUNCIL_DISTRICT,
                                 levels=order5,ordered=TRUE)
levels(heatdat$Weekday)
levels(heatdat$COUNCIL_DISTRICT)


heatdat$Teenager<-as.character(ifelse(heatdat$AGE<21,"Teenager","Adult"))
table(addNA(heatdat$Teenager))

heatdat<-na.omit(heatdat)

#count the arrest percentage by weekdays, council_districts, each hour 

#server
server<-shinyServer(function(input, output) {
  # setting the reactive environment 
  dataInput <- reactive({
    subset(heatdat,
           Weekday==input$myWeekdays&
             COUNCIL_DISTRICT==input$myDistricts
    )
  })
  #function for histogram
  output$myPlot <- renderPlot({
    ggplot(dataInput(),aes(Hour,color=factor(Teenager)))+ 
      geom_line(stat ="bin",size = 2,aes(y =(..count..)/sum(..count..)))+
      scale_color_manual(values = c("#EC6875", "#86BECB"))+
      scale_y_continuous(labels=percent_format())+
      xlab("Hour of Day")+
      ylab("Joint Probability")+
      theme(legend.position = "right",
            axis.text.y = element_text(size=18,face = "bold.italic"),
            axis.text.x = element_text(size=18, hjust = 0))
  })
})