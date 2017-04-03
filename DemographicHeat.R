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

GenderData<-filter(minidata,PUBLIC_WORKS_DIVISION>0)
Gender<-Countfun(GenderData$PK,GenderData$GENDER,GenderData$PUBLIC_WORKS_DIVISION)
GenderCount<-matrix(Gender,nrow=6)
rnames<-1:6
cnames<-c(sort(unique(minidata$GENDER)))
rownames(GenderCount)<-rnames
colnames(GenderCount)<-cnames

numorder<-names(sort(table(GenderData$PUBLIC_WORKS_DIVISION),decreasing=T))
num2order<-names(sort(table(minidata$GENDER),decreasing=T))
row.order<- numorder
col.order<-num2order
GenderCount<-GenderCount[row.order,]
GenderCount<-GenderCount[,col.order]
rownames(GenderCount)<-c("District 2","District 1","District 4",
                         "District 3","District 5","District 6")

rnames<- rownames(GenderCount)
GenderCount<-cbind(rnames,data.frame(GenderCount))
GenderCount<-melt(GenderCount)
GenderCount$rnames<-factor(GenderCount$rnames, levels=unique(as.character(GenderCount$rnames)) )
GenderCount$variable<-factor(GenderCount$variable, levels=unique(as.character(GenderCount$variable)) )
names(GenderCount)[3]<-"Frequency"

Percentage<-NULL
for(i in 1:length(GenderCount$Frequency)){
  print(i)
  Percentage<-c(Percentage,GenderCount$Frequency[i]/sum(GenderCount$Frequency))
}
GenderCount<-cbind(GenderCount,Percentage)

GenderCount$Percentage<-round(GenderCount$Percentage*100, digits = 0)
GenderCount$Percentage<-ifelse(GenderCount$Percentage>0,paste0(as.character(GenderCount$Percentage),"%"),0)



heat4<-ggplot(GenderCount, aes(variable, rnames)) + 
  geom_tile(aes(fill = Frequency), colour = "darkturquoise") + 
  scale_fill_viridis()

heat4<-heat4+scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        plot.title = element_text(size = 20,colour="gray40",face = "bold"),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size=13, hjust = 0,face = "bold.italic"),
        axis.text.x = element_text(size=13, hjust = 0,face = "bold.italic"))+
  ggtitle("Heatmap of Offense by Gender&Public Works Divison")

heat4
heat4+geom_text(aes(label = Percentage),
                size=8,fontface=2,color="darkorange3")

heat4+geom_text(aes(label = Frequency),
                size=8,fontface=2,color="darkorange3")

#for Race
Race<-Countfun(PublicData$PK,PublicData$RACE,PublicData$PUBLIC_WORKS_DIVISION)
RaceCount<-matrix(Race,nrow=6)
rnames<-1:6
cnames<-c(sort(unique(minidata$RACE)))
rownames(RaceCount)<-rnames
colnames(RaceCount)<-cnames

numorder<-names(sort(table(PublicData$PUBLIC_WORKS_DIVISION),decreasing=T))
num2order<-names(sort(table(PublicData$RACE),decreasing=T))
row.order<- numorder
col.order<-num2order
RaceCount<-RaceCount[row.order,]
RaceCount<-RaceCount[,col.order]
rownames(RaceCount)<-c("District 2","District 1","District 4",
                         "District 3","District 5","District 6")

rnames<- rownames(RaceCount)
RaceCount<-cbind(rnames,data.frame(RaceCount))
RaceCount<-melt(RaceCount)
RaceCount$rnames<-factor(RaceCount$rnames, levels=unique(as.character(RaceCount$rnames)) )
RaceCount$variable<-factor(RaceCount$variable, levels=unique(as.character(RaceCount$variable)) )
names(RaceCount)[3]<-"Frequency"

Percentage<-NULL
for(i in 1:length(RaceCount$Frequency)){
  print(i)
  Percentage<-c(Percentage,RaceCount$Frequency[i]/sum(RaceCount$Frequency))
}
RaceCount<-cbind(RaceCount,Percentage)

RaceCount$Percentage<-round(RaceCount$Percentage*100, digits = 0)
RaceCount$Percentage<-ifelse(RaceCount$Percentage>0,paste0(as.character(RaceCount$Percentage),"%"),0)



heat5<-ggplot(RaceCount, aes(variable, rnames)) + 
  geom_tile(aes(fill = Frequency), colour = "darkturquoise") + 
  scale_fill_viridis()

heat5<-heat5+scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 9) + 
  theme(legend.position = "right",
        plot.title = element_text(size = 20,colour="gray40",face = "bold"),
        axis.ticks = element_blank(), 
        axis.text.y = element_text(size=13, hjust = 0,face = "bold.italic"),
        axis.text.x = element_text(size=15, hjust = 0,face = "bold.italic"))+
  ggtitle("Heatmap of Offense by Race&Public Works Divison")

heat5
heat5+geom_text(aes(label = Percentage),
                size=8,fontface=2,color="darkorange3")

heat5+geom_text(aes(label = Frequency),
                size=8,fontface=2,color="darkorange3")
