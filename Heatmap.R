library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(viridis)

#Reashape the data for weekdays and council district
dat<-ArrestData
heatdat<-filter(dat,weekdays(ARRESTTIME)>0&COUNCIL_DISTRICT>0)%>%
  mutate(Weekday=weekdays(ARRESTTIME),Hour=hour(ARRESTTIME))

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
#Create heatmap
Freqs <- table(heatdat$Weekday, heatdat$COUNCIL_DISTRICT)
WeekCount<- heatdat %>% 
  group_by(Weekday, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(heatdat$Hour, heatdat$COUNCIL_DISTRICT)
HourCount<- heatdat %>% 
  group_by(Hour, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

countPercent<-function(x){
  Percentage<-NULL
  for(i in 1:length(x)){
    Percentage<-c(Percentage,x[i]/sum(x))
    one_00<-Percentage*100
    one_00<-round(one_00,2)
    one_00[one_00>0]<-paste0(one_00,"%")
    one_00[one_00<=0]<-0
  }
  return(one_00)
}
WeekCount$Percentage<-countPercent(WeekCount$Freq)
HourCount$Percentage<-countPercent(HourCount$Freq)

#order weekdays and districts by frequencies
order1<-names(sort(table(heatdat$COUNCIL_DISTRICT),
                   decreasing = T))
WeekCount$COUNCIL_DISTRICT<-factor(WeekCount$COUNCIL_DISTRICT,
                        levels=order1,ordered=TRUE)
HourCount$COUNCIL_DISTRICT<-factor(HourCount$COUNCIL_DISTRICT,
                                   levels=order1,ordered=TRUE)

levels(WeekCount$COUNCIL_DISTRICT)
levels(HourCount$COUNCIL_DISTRICT)
order2<-names(sort(table(heatdat$Weekday),
                   decreasing = T))
WeekCount$Weekday<-factor(WeekCount$Weekday,
                        levels=order2,ordered=TRUE)
levels(WeekCount$Weekday)

order3<-names(sort(table(heatdat$Hour),
                   decreasing = T))

HourCount$Hour<-factor(HourCount$Hour,
                          levels=order3,ordered=TRUE)
levels(HourCount$Hour)


heat_it<-function(frames,x,y,z,q,opts){
  heat<-ggplot(frames, aes(x, y)) + 
    geom_tile(aes(fill = z), colour = "white") + 
    scale_fill_gradient(low = "white", high = "deepskyblue3")+
    scale_x_discrete("", expand = c(0, 0)) + 
    scale_y_discrete("", expand = c(0, 0)) + 
    theme_grey(base_size = 9) + 
    theme(legend.position = "right",
          plot.title = element_text(size = 16,colour="gray40",face = "bold"),
          axis.ticks = element_blank(), 
          axis.text.y = element_text(size=14, hjust = 0,face = "bold.italic"),
          axis.text.x = element_text(size=14, angle = 330,
                                     hjust = 0,face = "bold.italic"))
  if(opts==1){
    heat+geom_text(aes(label = z),
                   size=5,fontface=2,color="gray28")
  }else{
    if(opts==2){heat+geom_text(aes(label = q),
                               size=7,fontface=2,color="gray28")
    }
    else{heat}
  }
}


heat_it(WeekCount,
        WeekCount$COUNCIL_DISTRICT,
        WeekCount$Weekday,
        WeekCount$Freq,WeekCount$Percentage,opts=2)+guides(fill=guide_legend("Joint %"))

heat_it(HourCount,
        HourCount$COUNCIL_DISTRICT,
        HourCount$Hour,
        HourCount$Freq,HourCount$Percentage,opts=2)+guides(fill=guide_legend("Joint %"))

#code for conditional probability
countProp<-function(frames,x,y){
  percentage<-NULL
  for(i in 1:length(x)){
    percentage<-c(percentage,x[i]/sum(x))
  }
  frames$percentage<-percentage
  c<-unlist(tapply(frames$percentage,addNA(y),sum,na.rm=F))
  return(c)
}

c<-countProp(WeekCount,WeekCount$Freq,
             WeekCount$COUNCIL_DISTRICT)
c<-countProp(WeekCount,WeekCount$Freq,
             WeekCount$Weekday)

c<-countProp(HourCount,HourCount$Freq,
             HourCount$Hour)
c

b<-arrange(WeekCount,COUNCIL_DISTRICT,Weekday)
table(addNA(WeekCount$COUNCIL_DISTRICT))
b<-arrange(WeekCount,Weekday,COUNCIL_DISTRICT)
table(addNA(WeekCount$Weekday))
b<-arrange(HourCount,Hour,COUNCIL_DISTRICT)
table(addNA(HourCount$Hour))

c<-c(rep(c[1:9],each=7))
c<-c(rep(c[1:7],each=9))
c<-c(rep(c[1:20],each=9),rep(c[21],8),rep(c[22],9),
     rep(c[23:24],each=8))


minicountProp<-function(frames,x,y){
  percentage<-NULL
  for(i in 1:length(x)){
    percentage<-c(percentage,x[i]/sum(x))
  }
  return(percentage)
}

a<-minicountProp(b,b$Freq)

b$a<-a
b$c<-c
b$z<-a/c
b$h<-paste0(round(b$z*100),"%")


heat<-ggplot(b, aes(b$COUNCIL_DISTRICT,b$Hour )) + 
  geom_tile(aes(fill = b$z), colour = "white") + 
  scale_fill_gradient(low = "white", high = "deepskyblue3")+
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        plot.title = element_text(size = 16,colour="gray40",face = "bold"),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size=14, hjust = 0,face = "bold.italic"),
        axis.text.x = element_text(size=14,angle = 330, hjust = 0))

heat+geom_text(aes(label = b$h),
               size=5,fontface=2,color="gray28")+
  guides(fill=guide_legend("Conditional%"))

heat+guides(fill=guide_legend("Conditional%"))
