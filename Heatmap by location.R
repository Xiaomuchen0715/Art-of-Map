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
 
#function to count
Freqs <- table(minidata$ShortCode, minidata$INCIDENTNEIGHBORHOOD)
  NeighborCount<- minidata %>% 
  group_by(ShortCode, INCIDENTNEIGHBORHOOD) %>%
  summarise(Freq = n())
  
Freqs <- table(minidata$ShortCode, minidata$COUNCIL_DISTRICT)
  DistricCount<- minidata %>% 
    group_by(ShortCode, COUNCIL_DISTRICT) %>%
    summarise(Freq = n())
  
Freqs <- table(minidata$ShortCode, minidata$PUBLIC_WORKS_DIVISION)
PublicCount<- minidata %>% 
    group_by(ShortCode, PUBLIC_WORKS_DIVISION) %>%
    summarise(Freq = n())





b$mprop<-c
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

PublicCount$Percentage<-countPercent(PublicCount$Freq)
DistricCount$Percentage<-countPercent(DistricCount$Freq)
NeighborCount$Percentage<-countPercent(NeighborCount$Freq)

#sortdata function
order1<-names(sort(table(minidata$INCIDENTNEIGHBORHOOD),
           decreasing = T))
order2<-names(sort(table(minidata$ShortCode),
                   decreasing = T))

order3<-names(sort(table(minidata$COUNCIL_DISTRICT),
             decreasing = T))

order4<-names(sort(table(minidata$PUBLIC_WORKS_DIVISION),
                   decreasing = T))

NeighborCount$ShortCode<-factor(NeighborCount$ShortCode,levels=order2,ordered=TRUE)
DistricCount$ShortCode<-factor(DistricCount$ShortCode,levels=order2,ordered=TRUE)
PublicCount$ShortCode<-factor(PublicCount$ShortCode,levels=order2,ordered=TRUE)
  levels(PublicCount$ShortCode)
  levels(DistricCount$ShortCode)
  levels(NeighborCount$ShortCode)
  

NeighborCount$INCIDENTNEIGHBORHOOD<-factor(NeighborCount$INCIDENTNEIGHBORHOOD,levels=order1,ordered=TRUE)
DistricCount$COUNCIL_DISTRICT<-factor(DistricCount$COUNCIL_DISTRICT,
                                           levels=order3,ordered=TRUE)
PublicCount$PUBLIC_WORKS_DIVISION<-factor(PublicCount$PUBLIC_WORKS_DIVISION,
                                      levels=order4,ordered=TRUE)

levels(PublicCount$PUBLIC_WORKS_DIVISION)
levels(DistricCount$COUNCIL_DISTRICT)
levels(NeighborCount$INCIDENTNEIGHBORHOOD)


#heatmap function
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
                     size=4,fontface=2,color="orangered1")
    }
    else{heat}
  }
}
 
heat_it(NeighborCount,
        NeighborCount$INCIDENTNEIGHBORHOOD,
        NeighborCount$ShortCode,
        NeighborCount$Freq,NeighborCount$Percentage,opts=1)

heat_it(DistricCount,
        DistricCount$COUNCIL_DISTRICT,
        DistricCount$ShortCode,
        DistricCount$Freq,DistricCount$Percentage,opts=0)

heat_it(PublicCount,
        PublicCount$ShortCode,
        PublicCount$PUBLIC_WORKS_DIVISION,
        PublicCount$Freq,PublicCount$Percentage,opts=2)

