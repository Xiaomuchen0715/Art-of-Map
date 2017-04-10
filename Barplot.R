library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)
library(reshape)
library(reshape)
library(RColorBrewer)
dat<-ArrestData
heatdat<-filter(dat,weekdays(ARRESTTIME)>0&COUNCIL_DISTRICT>0)%>%
  mutate(Weekday=wday(ARRESTTIME,label = TRUE, abbr = FALSE),Hour=hour(ARRESTTIME))
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==1]<-"District One"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==2]<-"District Two"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==3]<-"District Three"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==4]<-"District Four"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==5]<-"District Five"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==6]<-"District Six"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==7]<-"District Seven"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==8]<-"District Eight"
heatdat$COUNCIL_DISTRICT[heatdat$COUNCIL_DISTRICT==9]<-"District Nine"
table(heatdat$COUNCIL_DISTRICT)

#District Eight  District Five  District Four  District Nine 
#171            326            555            975 
#District One District Seven   District Six District Three 
#1152            400           1214            997 
#District Two 
#609
#sort the council district
order1<-names(sort(table(heatdat$COUNCIL_DISTRICT),
                   decreasing = T))
heatdat$COUNCIL_DISTRICT<-factor(heatdat$COUNCIL_DISTRICT,
                                           levels=order1,ordered=TRUE)
levels(heatdat$COUNCIL_DISTRICT)

order2<-names(sort(table(heatdat$Weekday),
                   decreasing = T))
heatdat$Weekday<-factor(heatdat$Weekday,
                                 levels=order2,ordered=TRUE)
levels(heatdat$COUNCIL_DISTRICT)


heatdat$CountDistrict<-tapply(heatdat$COUNCIL_DISTRICT,heatdat$COUNCIL_DISTRICT,length)/sum(tapply(heatdat$COUNCIL_DISTRICT,heatdat$COUNCIL_DISTRICT,length))
tapply(heatdat$COUNCIL_DISTRICT,heatdat$COUNCIL_DISTRICT,length)/sum(tapply(heatdat$COUNCIL_DISTRICT,heatdat$COUNCIL_DISTRICT,length))
levels(heatdat$Weekday)

p<-ggplot(heatdat, aes(x=reorder(COUNCIL_DISTRICT, -table(COUNCIL_DISTRICT)[COUNCIL_DISTRICT])),
          label= tapply(heatdat$COUNCIL_DISTRICT,heatdat$COUNCIL_DISTRICT,length)
)+
  geom_bar(width =0.5,fill="deepskyblue3")+
  xlab("Council Districts")+
  theme(text = element_text(size=13,face="bold"))
p

heatdat
p5<-ggplot(heatdat, aes(x=reorder(COUNCIL_DISTRICT, -table(COUNCIL_DISTRICT)[COUNCIL_DISTRICT]),fill=Weekday))+
  xlab("Council District")+
  geom_bar(width=0.5,position="dodge")

p5+scale_fill_manual(values = colorbar3)+
  geom_line(aes(x = COUNCIL_DISTRICT, y = CountDistrict))
  
olorbar3<-(c("indianred2","deepskyblue1","darkorange1",
              "plum4","aquamarine3","azure3","bisque3"))
display.brewer.all()



