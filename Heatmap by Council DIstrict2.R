library(ggplot2)
install.packages("viridis")
install.packages("gplots")
library(viridis)
library(gplots)
install.packages("reshape")
library(reshape)
library(dplyr)
#function to count


Countfun<-function(rr,x,y){
  x<-sort(x)
  y<-sort(y)
  count<-NULL
  for(i in 1:length(unique(x))){
    for(j in 1:length(unique(y))){
      print(unique(x)[i])
      print(unique(y)[j])
      count<-c(count,as.numeric(length(rr[x==unique(x)[i]&y==unique(y)[j]])))
      print(count)
      }
  }
  return(count)
}


Neighbor<-Countfun(minidata$PK,minidata$ShortCode,minidata$INCIDENTNEIGHBORHOOD)
DistrictData<-filter(minidata,!is.na(COUNCIL_DISTRICT))
PublicData<-filter(minidata,PUBLIC_WORKS_DIVISION>0)
District<-Countfun(DistrictData$PK,DistrictData$ShortCode,DistrictData$COUNCIL_DISTRICT)
Public<-Countfun(PublicData$PK,PublicData$ShortCode,PublicData$PUBLIC_WORKS_DIVISION)

table(Neighbor)
table(District)


#Create matrix For Neighborhood 
NeighborCount<-matrix(Neighbor,nrow=97)
rnames<-c(sort(unique(minidata$INCIDENTNEIGHBORHOOD)))
cnames<-c(sort(unique(minidata$ShortCode)))
rownames(NeighborCount)<-rnames
colnames(NeighborCount)<-cnames

#Create matrix For Coucil Distric
DistricCount<-matrix(District,nrow=9)
rnames<-c(sort(unique(minidata$COUNCIL_DISTRICT)))
cnames<-c(sort(unique(minidata$ShortCode)))
rownames(DistricCount)<-rnames
colnames(DistricCount)<-cnames

#Create matrix For Public Works Division
PublicCount<-matrix(Public,nrow=6)
rnames<-1:6
cnames<-c(sort(unique(minidata$ShortCode)))
rownames(PublicCount)<-rnames
colnames(PublicCount)<-cnames

#sortdata, according to crimes frequencies
numorder<-names(sort(table(minidata$INCIDENTNEIGHBORHOOD),decreasing=T))
num2order<-names(sort(table(minidata$ShortCode),decreasing=T))
row.order<- numorder
col.order<-num2order
NeighborCount<-NeighborCount[row.order,]
NeighborCount<-NeighborCount[,col.order]

#sortdata, according to crimes frequencies
numorder<-names(sort(table(minidata$COUNCIL_DISTRICT),decreasing=T))
num2order<-names(sort(table(minidata$ShortCode),decreasing=T))
row.order<- numorder
col.order<-num2order
DistricCount<-DistricCount[row.order,]
DistricCount<-DistricCount[,col.order]

rownames(DistricCount)<-c("District 6","District 1","District 3","District 9",
"District 2","District 4","District 7","District 5",
"District 8")

#sortdata, according to crime frequencies
numorder<-names(sort(table(minidata$PUBLIC_WORKS_DIVISION),decreasing=T))[1:6]
num2order<-names(sort(table(minidata$ShortCode),decreasing=T))
row.order<- numorder
col.order<-num2order
PublicCount<-PublicCount[row.order,]
PublicCount<-PublicCount[,col.order]

rownames(PublicCount)<-c("District 2","District 1","District 4",
                          "District 3","District 5","District 6")

#creat new dataframe for neighborhood data
rnames<- rownames(NeighborCount)
NeighborCount<-cbind(rnames,data.frame(NeighborCount))
NeighborCount<-melt(NeighborCount)
names(NeighborCount)[3]<-"Frequency"
NeighborCount$rnames<-factor(NeighborCount$rnames, levels=unique(as.character(NeighborCount$rnames)) )
NeighborCount$variable<-factor(NeighborCount$variable, levels=unique(as.character(NeighborCount$variable)) )

#creat new dataframe for district data
rnames<- rownames(DistricCount)
DistricCount<-cbind(rnames,data.frame(DistricCount))
DistricCount<-melt(DistricCount)
names(DistricCount)[3]<-"Frequency"
DistricCount$rnames<-factor(DistricCount$rnames, levels=unique(as.character(DistricCount$rnames)) )
DistricCount$variable<-factor(DistricCount$variable, levels=unique(as.character(DistricCount$variable)) )

#creat new dataframe for public data
rnames<- rownames(PublicCount)
PublicCount<-cbind(rnames,data.frame(PublicCount))
PublicCount<-melt(PublicCount)
names(PublicCount)[3]<-"Frequency"
PublicCount$rnames<-factor(PublicCount$rnames, levels=unique(as.character(PublicCount$rnames)) )
PublicCount$variable<-factor(PublicCount$variable, levels=unique(as.character(PublicCount$variable)) )

Percentage<-NULL
for(i in 1:length(PublicCount$Frequency)){
  print(i)
  Percentage<-c(Percentage,PublicCount$Frequency[i]/sum(PublicCount$Frequency))
  }
PublicCount<-cbind(PublicCount,Percentage)

PublicCount$Percentage<-round(PublicCount$Percentage*100, digits = 0)
PublicCount$Percentage<-ifelse(PublicCount$Percentage>0,paste0(as.character(PublicCount$Percentage),"%"),0)

#graphy
 heat1<-ggplot(NeighborCount, aes(variable, rnames)) + 
    geom_tile(aes(fill = Frequency), colour = "white") + 
  scale_fill_viridis()
 
heat1+scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        plot.title = element_text(size = 16,colour="gray40",face = "bold"),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size=3, hjust = 0),
        axis.text.x = element_text(size=5,angle = 350, hjust = 0))+
    ggtitle("Heatmap of Offense by Neighborhood")

  heat1

heat2<-ggplot(DistricCount, aes(variable, rnames)) + 
  geom_tile(aes(fill = Frequency),colour="white")

heat2<-heat2+scale_fill_viridis()+
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        axis.ticks = element_blank(), 
        plot.title = element_text(size =20,colour="gray40",face = "bold"),
        axis.text.y = element_text(size=15, hjust = 0,face = "bold.italic"),
        axis.text.x = element_text(size=8,angle = 335, hjust = 0))+
  ggtitle("Heatmap of Offense by Council District")
heat2+geom_text(aes(label = Frequency),
           size=6,fontface=2,color="darkorange3")

heat3<-ggplot(PublicCount, aes(variable, rnames)) + 
  geom_tile(aes(fill = Frequency), colour = "white") + 
  scale_fill_viridis()

heat3<-heat3+scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        plot.title = element_text(size = 20,colour="gray40",face = "bold"),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size=13, hjust = 0,face = "bold.italic"),
        axis.text.x = element_text(size=8,angle = 335, hjust = 0))+
  ggtitle("Heatmap of Offense by Public Works Division")

heat3+geom_text(aes(label = Percentage),
                size=6,fontface=2,color="darkorange3")

heat3+geom_text(aes(label = Frequency),
                size=6,fontface=2,color="darkorange3")
