library(ggplot2)
install.packages("viridis")
install.packages("gplots")
library(viridis)
library(gplots)
#function to count


Countfun<-function(rr,x,y){
  x<-sort(x)
  y<-sort(y)
  count<-NULL
  for(i in 1:length(unique(x))){
    for(j in 1:length(unique(y))){
      print(unique(x)[i])
      count<-c(count,as.numeric(length(rr[x==unique(x)[i]&y==unique(y)[j]])))
    }
  }
  return(count)
}
Neighbor<-Countfun(minidata$PK,minidata$ShortCode,minidata$INCIDENTNEIGHBORHOOD)

table(Neighbor)
#check the answer
length(unique(minidata$INCIDENTNEIGHBORHOOD))*
  length(unique(minidata$ShortCode))
table(Neighbor)

NeighborCount<-matrix(Neighbor,nrow=97)
rnames<-c(unique(minidata$INCIDENTNEIGHBORHOOD))
cnames<-c(unique(minidata$ShortCode))
rownames(NeighborCount)<-rnames
colnames(NeighborCount)<-cnames
#sortdata, according to crimes frequencies
numorder<-names(sort(table(minidata$INCIDENTNEIGHBORHOOD),decreasing=F))
row.order<- numorder
NeighborCount<-NeighborCount[row.order,]

heatmap(NeighborCount)
