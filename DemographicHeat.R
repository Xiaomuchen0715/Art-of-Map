library(ggplot2)
install.packages("viridis")
install.packages("gplots")
library(viridis)
library(gplots)
install.packages("reshape")
library(reshape)
library(dplyr)
#function to count

Freqs <- table(minidata$GENDER, minidata$COUNCIL_DISTRICT)
GenderCount<- minidata %>% 
  group_by(GENDER, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(minidata$RACE, minidata$COUNCIL_DISTRICT)
RaceCount<- minidata %>% 
  group_by(RACE, COUNCIL_DISTRICT) %>%
  summarise(Freq = n())

Freqs <- table(minidata$AGE, minidata$COUNCIL_DISTRICT)
AgeCount<- minidata %>% 
  group_by(AGE, COUNCIL_DISTRICT) %>%
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

RaceCount$Percentage<-countPercent(RaceCount$Freq)
GenderCount$Percentage<-countPercent(GenderCount$Freq)
AgeCount$Percentage<-countPercent(AgeCount$Freq)

#sort data 
order5<-names(sort(table(minidata$GENDER),
                   decreasing = T))
order6<-names(sort(table(minidata$RACE),
                   decreasing = T))
order7<-names(sort(table(minidata$AGE),
                   decreasing = T))

RaceCount$COUNCIL_DISTRICT<-factor(RaceCount$COUNCIL_DISTRICT,levels=order3,ordered=TRUE)
GenderCount$COUNCIL_DISTRICT<-factor(GenderCount$COUNCIL_DISTRICT,levels=order3,ordered=TRUE)
AgeCount$COUNCIL_DISTRICT<-factor(AgeCount$COUNCIL_DISTRICT,levels=order3,ordered=TRUE)

levels(AgeCount$COUNCIL_DISTRICT)
levels(GenderCount$COUNCIL_DISTRICT)

RaceCount$RACE<-factor(RaceCount$RACE,levels=order6,ordered=TRUE)
GenderCount$GENDER<-factor(GenderCount$GENDER,levels=order5,ordered=TRUE)
AgeCount$AGE<-factor(AgeCount$AGE,levels=order7,ordered=TRUE)
levels(AgeCount$AGE)
#"999" is so wried

#heatmap
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
          axis.text.y = element_text(size=8, hjust = 0,face = "bold.italic"),
          axis.text.x = element_text(size=8,angle = 330, hjust = 0))
  if(opts==1){
    heat+geom_text(aes(label = z),
                   size=5,fontface=2,color="orangered1")
  }else{
    if(opts==2){heat+geom_text(aes(label = q),
                               size=0,fontface=0,color="orangered1")
    }
    else{heat}
  }
}

heat_it(GenderCount,
        GenderCount$COUNCIL_DISTRICT,
        GenderCount$GENDER,
        GenderCount$Freq,GenderCount$Percentage,opts=1)

heat_it(RaceCount,
        RaceCount$COUNCIL_DISTRICT,
        RaceCount$RACE,
        RaceCount$Freq,RaceCount$Percentage,opts=2)

heat_it(AgeCount,
        AgeCount$COUNCIL_DISTRICT,
        AgeCount$AGE,
        AgeCount$Freq,AgeCount$Percentage,opts=0)



