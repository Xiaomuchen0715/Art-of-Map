#import data as dat
dat<-ArrestData
#
library(dplyr)
library(ggplot2)

#filt the dataframe and get the first type of offense
easierdat<-function(x,frame){
  offense<-strsplit(as.character(x),"/")
  mainoffense<-sapply(offense,"[[",1)
  First_Offenses<-mainoffense
  matches<-gregexpr("[0-9]+",x)
  FullCode<-regmatches(x,matches)
  FullCode<-sapply(FullCode,"[[",1)
  frame<-cbind(frame,FullCode,stringsAsFactors=F)
  FullCode[which(nchar(FullCode) == 2)]<-"13(a)"
  FullCode[which(nchar(FullCode) == 4)]<-substr( FullCode[which(nchar(FullCode) == 4)],1,2)
  FullCode[which(nchar( FullCode) == 3)]<-substr( FullCode[which(nchar( FullCode) == 3)],1,1)
  ShortCode<-as.character(FullCode)
  frame<-cbind(frame,ShortCode,stringsAsFactors = FALSE)
  frame<-na.omit(frame)
  return(frame)
}

#Noticed it get rid of 382 rows with NAs
dat<-easierdat(dat$OFFENSES,dat)
sort(table(dat$ShortCode),decreasing = T)

#Focusing on the top-ten offenses based on both short and long codes.
sort(table(dat$ShortCode),decreasing = T)[1:10]
#27 13(a)     9    39    95    90    15    55    51    35 
#1171   588   412   396   246   220   186   163   147   144 
sort(table(dat$FullCode),decreasing = T)[1:10]


#To decode we look the information on website http://www.legis.state.pa.us/wu01/li/li/ct/htm/18/18.htm
#, which give us the information about Title 18 in Consolidated Statutes. 
#27 represents chapter 27 of Title 18 with the title "Assult"
#13(a) represents "Controlled Substance, Drug, Device and Cosmetic Act"
#9 represents chapter 9 of Title 18 with the tile "Inchoate Crimes"
#39 represents chapter 39 of Title 18 with the title "Theft and Related Offenses"
#"55" represents chapter 55 of Title 18 with the title "Riot, Disorderly Conduct and Related Offenses"
#"51" represents chapter 51 of Title 18 with the title "Obstructing Governmental Operations"
#"35" represents chapter 35 of Title 18 with the title "Burglary and Other Criminal Intrusion"
#The rest of the top ten are not listed in Title 18.

table(dat$FullCode[dat$ShortCode=="95"])
#9501 
#246
#All the 246 "95" here represents "Bench Warrant"


table(dat$FullCode[dat$ShortCode=="90"])
#Offenses coded as "90" are actually "9015" and "9093"
#9015 9093 
#162   58 
#They are "9015 Failure To Appear/Arrest on Attachment Order" 
#and "Indirect Criminal Contempt"

table(dat$FullCode[dat$ShortCode=="15"])
#1501 1511 1543 1574 
#29    1  155    1 
#1543" is form  Title 75 "Driving While Operating Privilege Is Suspended"
#"1543" is from  Title 75 "Drivers Required to be Licensed"
#"1501" is from Title 75 "Carrying and exhibiting driver's license on demand"
#"1574" is from Title 75 "Permitting unauthorized person to drive"
#All of these are titled as "Licensing of Drivers" from "Title 75" 

#We will only focus on incidents coded based on Title 18 as "27","13(a)","9","39","55","51","35"
minidat<-filter(dat,ShortCode=="27"|ShortCode=="13(a)"|ShortCode=="9"|ShortCode=="39"
       |ShortCode=="55")



