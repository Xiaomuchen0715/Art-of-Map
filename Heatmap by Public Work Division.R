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
detach("package:plyr", unload=TRUE) 
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

##ADD NAMES FOR 
PublicCount$PUBLIC_WORKS_DIVISION<-as.character(PublicCount$PUBLIC_WORKS_DIVISION)
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="2"]<-"Division Two"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="1"]<-"Division One"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="4"]<-"Division Four"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="3"]<-"Division Three"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="5"]<-"Division Five"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="6"]<-"Division Six"
PublicCount$PUBLIC_WORKS_DIVISION[PublicCount$PUBLIC_WORKS_DIVISION=="0"]<-"Zero"
levels(PublicCount$PUBLIC_WORKS_DIVISION)

DistricCount$COUNCIL_DISTRICT<-as.character(DistricCount$COUNCIL_DISTRICT)
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="6"]<-"District Six"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="1"]<-"District One"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="3"]<-"District Three"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="9"]<-"District Nine"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="2"]<-"District Two"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="4"]<-"District Four"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="7"]<-"District Seven"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="5"]<-"District Five"
DistricCount$COUNCIL_DISTRICT[DistricCount$COUNCIL_DISTRICT=="8"]<-"District Eight"




#sortdata function
order1<-names(sort(table(minidata$INCIDENTNEIGHBORHOOD),
           decreasing = T))
order2<-names(sort(table(minidata$ShortCode),
                   decreasing = T))
names(sort(table(minidata$COUNCIL_DISTRICT),
                   decreasing = T))

order3<-c("District Six","District One",
          "District Three","District Nine",
          "District Two","District Four",
          "District Seven","District Five",
          "District Eight")

names(sort(table(minidata$PUBLIC_WORKS_DIVISION),
                   decreasing = T))

order4<-c("Division Two",
"Division One",
"Division Four",
"Division Three",
"Division Five",
"Division Six",
"Zero")

NeighborCount$ShortCode<-factor(NeighborCount$ShortCode,levels=order2,ordered=TRUE)
DistricCount$ShortCode<-factor(DistricCount$ShortCode,levels=order2,ordered=TRUE)
PublicCount$ShortCode<-factor(PublicCount$ShortCode,levels=order2,ordered=TRUE)
  levels(PublicCount$ShortCode)
  levels(DistricCount$ShortCode)
  levels(NeighborCount$ShortCod)

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
                     size=5,fontface=2,color="orangered1")
    }
    else{heat}
  }
}
 
heat_it(NeighborCount,
        NeighborCount$INCIDENTNEIGHBORHOOD,
        NeighborCount$ShortCode,
        NeighborCount$Freq,NeighborCount$Percentage,opts=1)

heat_it(DistricCount,
        DistricCount$ShortCode,
        DistricCount$COUNCIL_DISTRICT,
        DistricCount$Freq,DistricCount$Percentage,opts=2)

heat_it(PublicCount,
        PublicCount$ShortCode,
        PublicCount$PUBLIC_WORKS_DIVISION,
        PublicCount$Freq,PublicCount$Percentage,opts=2)

