library(ggplot2)
library(dplyr)
library(viridis)
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
c
c<-countProp(GenderCount,GenderCount$Freq,
             GenderCount$COUNCIL_DISTRICT)

c<-countProp(GenderCount,GenderCount$Freq,
             GenderCount$COUNCIL_DISTRICT)
c
b<-arrange(WeekCount,COUNCIL_DISTRICT,Weekday)
b<-arrange(GenderCount,COUNCIL_DISTRICT,GENDER)
b<-arrange(RaceCount,COUNCIL_DISTRICT,RACE)
b<-arrange(DistricCount,COUNCIL_DISTRICT,ShortCode)

table(addNA(WeekCount$COUNCIL_DISTRICT))
table(addNA(GenderCount$COUNCIL_DISTRICT))
table(addNA(RaceCount$COUNCIL_DISTRICT))
table(addNA(DistricCount$COUNCIL_DISTRICT))

c<-c(rep(c[1:2],each=2),rep(c[3],3),rep(c[4:7],each=2),
     rep(c[8],3),rep(c[(9:10)],each=2))

c<-c(rep(c[1:6],each=2),rep(c[7],3),rep(c[4:7],each=2),
     rep(c[8],3),rep(c[(9:10)],each=2))

c<-c(rep(c[1:2],each=6),rep(c[3:4],each=5),rep(c[5:7],each=6),
     rep(c[8],5),rep(c[(9:10)],each=4))

c<-c(rep(c[1:10],each=4))

c<-c(rep(c[1:10],each=7))

c<-c(rep(c[1],4),rep(c[2],6),rep(c[3],7),
     rep(c[4],1),rep(c[5],8),rep(c[6:8],each=9),rep(c[9:30],each=10),
     rep(c[31],9),rep(c[32:33],each=10),rep(c[34],9),
     rep(c[35:36],each=10),rep(c[37],9),rep(c[38:40],10),rep(c[41],9),
     rep(c[42:45],each=10),rep(c[46],8),rep(c[47],10),rep(c[48],9),
     rep(c[49],8),rep(c[50],9),rep(c[51],8),rep(c[52],10),
     rep(c[53:55],each=7),rep(c[56],9),rep(c[57],5),rep(c[58],2),
     rep(c[59],3),rep(c[60:61],each=4),rep(c[62],6),rep(c[63],5),
     rep(c[64],3),rep(c[65],2),rep(c[66:69],each=1),rep(c[70],9))
   

minicountProp<-function(frames,x,y){
  percentage<-NULL
  for(i in 1:length(x)){
    percentage<-c(percentage,x[i]/sum(x))
  }
  return(percentage)
}

a<-minicountProp(b,b$Freq)

a[1:10]
b$a<-a
b$c<-c
b$z<-a/c
b$h<-paste0(round(b$z*100),"%")


    heat<-ggplot(b, aes(b$COUNCIL_DISTRICT,b$Weekday )) + 
    geom_tile(aes(fill = b$z), colour = "darkturquoise") + 
    scale_fill_viridis(option ="D")+
    scale_x_discrete("", expand = c(0, 0)) + 
    scale_y_discrete("", expand = c(0, 0)) + 
    theme_grey(base_size = 9) + 
    theme(legend.position = "right",
          plot.title = element_text(size = 16,colour="gray40",face = "bold"),
          axis.ticks = element_blank(), 
          axis.text.y = element_text(size=10, hjust = 0,face = "bold.italic"),
          axis.text.x = element_text(size=10,angle = 330, hjust = 0))

    heat+geom_text(aes(label = b$h),
                   size=7,fontface=2,color="firebrick1")+
      guides(fill=guide_legend("Conditional%"))
    
    heat+guides(fill=guide_legend("Conditional%"))

    b$a<-a
    b$c<-c
    b$z<-a/c
    b$h<-paste0(round(b$z*100),"%")
    
    
    heat<-ggplot(b, aes(b$COUNCIL_DISTRICT,b$Weekday )) + 
      geom_tile(aes(fill = b$z), colour = "darkturquoise") + 
      scale_fill_viridis(option ="D")+
      scale_x_discrete("", expand = c(0, 0)) + 
      scale_y_discrete("", expand = c(0, 0)) + 
      theme_grey(base_size = 9) + 
      theme(legend.position = "right",
            plot.title = element_text(size = 16,colour="gray40",face = "bold"),
            axis.ticks = element_blank(), 
            axis.text.y = element_text(size=10, hjust = 0,face = "bold.italic"),
            axis.text.x = element_text(size=10,angle = 330, hjust = 0))
    
    heat+geom_text(aes(label = b$h),
                   size=7,fontface=2,color="firebrick1")+
      guides(fill=guide_legend("Conditional%"))
    
    heat+guides(fill=guide_legend("Conditional%"))


