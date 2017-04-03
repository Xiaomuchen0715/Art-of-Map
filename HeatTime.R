library(ggplot2)
install.packages("viridis")
install.packages("gplots")
library(viridis)
library(gplots)
install.packages("reshape")
library(reshape)
library(dplyr)
library(dplyr)
install.packages("tidyr")
library(tidyr)



Freqs <- table(minidata$Weekday, minidata$COUNCIL_DISTRICT)
WeekCount<- minidata %>% 
  group_by(Weekday, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(minidata$Weekday, minidata$COUNCIL_DISTRICT)
Week2Count<- minidata %>% 
  group_by(Weekday, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(minidata$Hours, minidata$COUNCIL_DISTRICT)
HourCount<- minidata %>% 
  group_by(Hours, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(minidata$Hours, minidata$COUNCIL_DISTRICT)
Hour2Count<- minidata %>% 
  group_by(Hours, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

countPercent<-function(x){
  Percentage<-NULL
  for(i in 1:length(x)){
    Percentage<-c(Percentage,x[i]/sum(x))
    one_00<-round(Percentage*100,digits = 2)
    print(sum(one_00))
    one_00[one_00<=0]<-0
    one_00[one_00>0]<-paste0(one_00,"%")
  }
  return(one_00)
}

WeekCount$Percentage<-countPercent(WeekCount$Freq)
Week2Count$Percentage<-countPercent(Week2Count$Freq)
HourCount$Percentage<-countPercent(HourCount$Freq)
Hour2Count$Percentage<-countPercent(Hour2Count$Freq)

order3<-names(sort(table(minidata$COUNCIL_DISTRICT),
                   decreasing = T))
order8<-names(sort(table(minidata$Weekday),
                   decreasing = T))
order9<-names(sort(table(minidata$Hours),
                   decreasing = T))

order10<-sort(unique(minidata$Hours),
                   decreasing = T)


WeekCount$Weekday<-factor(WeekCount$Weekday,
                          levels=order8,ordered=TRUE)
WeekCount$COUNCIL_DISTRICT<-factor(WeekCount$COUNCIL_DISTRICT,
                          levels=order3,ordered=TRUE)
Week2Count$COUNCIL_DISTRICT<-factor(Week2Count$COUNCIL_DISTRICT,
                                   levels=order3,ordered=TRUE)


HourCount$Hours<-factor(HourCount$Hours,
                          levels=order9,ordered=TRUE)
HourCount$COUNCIL_DISTRICT<-factor(HourCount$COUNCIL_DISTRICT,
                                   levels=order3,ordered=TRUE)
Hour2Count$COUNCIL_DISTRICT<-factor(Hour2Count$COUNCIL_DISTRICT,
                                   levels=order3,ordered=TRUE)

Hour2Count$Hours<-factor(Hour2Count$Hours,
                                    levels=order10,ordered=TRUE)


levels(HourCount$Hours)
levels(WeekCount$Weekday)
levels(Week2Count$Weekday)

heat_it<-function(frames,x,y,z,q,opts){
  heat<-ggplot(frames, aes(x, y)) + 
    geom_tile(aes(fill = z), colour = "darkturquoise") + 
    scale_fill_viridis(option ="D")+
    scale_x_discrete("", expand = c(0, 0)) + 
    scale_y_discrete("", expand = c(0, 0)) + 
    theme_grey(base_size = 9) + 
    theme(legend.position = "right",
          plot.title = element_text(size = 16,colour="gray40",face = "bold"),
          axis.ticks = element_blank(), 
          axis.text.y = element_text(size=5, hjust = 0,face = "bold.italic"),
          axis.text.x = element_text(size=10, angle = 330,
                                     hjust = 0,face = "bold.italic"))
  if(opts==1){
    heat+geom_text(aes(label = z),
                   size=5,fontface=2,color="orangered1")
  }else{
    if(opts==2){heat+geom_text(aes(label = q),
                               size=4,fontface=2,color="orangered1")
    }
    else{heat}
  }
}

heat_it(WeekCount,
        WeekCount$COUNCIL_DISTRICT,
        WeekCount$Weekday,
        WeekCount$Freq,WeekCount$Percentage,opts=0)

heat_it(Week2Count,
        Week2Count$Weekday,
        Week2Count$COUNCIL_DISTRICT,
        WeekCount$Freq,WeekCount$Percentage,opts=0)

heat_it(HourCount,
        HourCount$COUNCIL_DISTRICT,
        HourCount$Hours,
        HourCount$Freq,HourCount$Percentage,opts=0)

heat_it(Hour2Count,
        Hour2Count$COUNCIL_DISTRICT,
        Hour2Count$Hours,
        Hour2Count$Freq,Hour2Count$Percentage,opts=0)
