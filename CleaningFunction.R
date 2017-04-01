library(dplyr)
library(lubridate)
library(ggplot2)
#import data as dat
dat<-ArrestMar11th
#check if anything wired about this dataset 
table(year(dat$ARRESTTIME))
#2014 2015 2016 2017 
#3    3 4141 2726 
#The frequencies of 2014 and 2015 is too small 

table(dat$AGE)
#0  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31 
#7  18  23  34  57  58  89 127 117 172 182 227 223 292 242 283 288 266 231 213 221 278 209 
#32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54 
#171 175 181 172 169 138 111 100 102 108  88  92  75  82  86  79  77  75  68  80 108  90  77 
#55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77 
#49  45  78  39  33  35  29  26  26   7   3   6   7  10  15   9   4   1   1   2   2   2   1 
#79  80  82  88 117 999 
#1   1   2   1   1   1 
#7 criminals are under 1 years old, 1 criminal is 99 years old.
#Therefore, we also decide to get rid of these  datas whith these characters. 

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
  FullCode[which(nchar( FullCode) == 3)]<-substr( FullCode[which(nchar(FullCode) == 3)],1,1)
  ShortCode<-as.character(FullCode)
  frame<-cbind(frame,ShortCode,stringsAsFactors = FALSE)
  frame<-na.omit(frame)
  frame<-frame[year(frame$ARRESTTIME)>2015&frame$AGE>0&frame$AGE<999,]
  return(frame)
}

dat<-easierdat(dat$OFFENSES,dat)

nrow(ArrestMar11th)-nrow(dat)
#Noticed it get rid of 568 rows with NAs
#check the result
table(year(dat$ARRESTTIME))
table(dat$AGE)
#The new data is whithout wried years and wired ages

#The distribution of different types of offenses
g1<-ggplot(dat, aes(x=reorder(ShortCode, -table(ShortCode)[ShortCode]),fill=GENDER))+
  ggtitle("Offenses by Different Codes")+xlab("Offense Types")+
  geom_bar(width=.5)
g1
#The distribution is extremely skewed with evident outliers.
#We need to get rid of outliers

dat<-dat[order(-table(dat$ShortCode)[dat$ShortCode]),]
dat$Count<-as.numeric(reorder(dat$ShortCode, -ave(as.numeric(dat$ShortCode), dat$ShortCode, FUN = length)))
table(boxplot.stats(dat$Count)$out)
dat$ShortCode[dat$Count==19]
#31
length(boxplot.stats(dat$Count)$out)
#165 
#we need to get rid of the tail of the  165 rows and offenses coded as "31"
length(dat$`_id`)-165#we only neew 1 tp 6140 row
dat<-dat[1:6140,]
dat<-filter(dat,!ShortCode=="31")
#check the result
g2<-ggplot(dat, aes(x=reorder(ShortCode, -table(ShortCode)[ShortCode]),fill=GENDER))+
  ggtitle("Offenses by Different Codes")+xlab("Offense Types")+
  geom_bar(width=.5)
g2
g1
#Now the distribution is more balanced than we saw in graph one.
#With 18 different types of short codes.



#To decode we look the information on website http://www.legis.state.pa.us/wu01/li/li/ct/htm/18/18.htm
#, which give us the information about Title 18 in Consolidated Statutes. 
#27 represents chapter 27 of Title 18 with the title "Assult"
#13(a) represents "Controlled Substance, Drug, Device and Cosmetic Act"
#9 represents chapter 9 of Title 18 with the tile "Inchoate Crimes"
#39 represents chapter 39 of Title 18 with the title "Theft and Related Offenses"
#"55" represents chapter 55 of Title 18 with the title "Riot, Disorderly Conduct and Related Offenses"
#"51" represents chapter 51 of Title 18 with the title "Obstructing Governmental Operations"
#"35" represents chapter 35 of Title 18 with the title "Burglary and Other Criminal Intrusion"
#"37" represents chapter 37 of Title 18 with the title "Robbery"(but with exceptions from other chapters)
#"49" represents chapter 49 of Title 18 with the title "Falsification and Intimidation"
#"38" represents chapter 38 of Title 18 with the title "Falsification and Intimidation"
#"61" represents chapter 61 of Title 18 with the title "Firearms and Other Dangerous Articles"
#"43" represents chapter 43 of Title 18 with the title "Offenses Against the Family"
#"59" represents chapter 59 of Title 18 with the title Public Indecency"
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

table(dat$FullCode[dat$ShortCode=="13"])
#1501 1511 1543 1574 
#29    1  155    1 
#1543" is form  Title 75 "Driving While Operating Privilege Is Suspended"
#"1543" is from  Title 75 "Drivers Required to be Licensed"
#"1501" is from Title 75 "Carrying and exhibiting driver's license on demand"
#"1574" is from Title 75 "Permitting unauthorized person to drive"
#All of these are titled as "Licensing of Drivers" from "Title 75" 
table(dat$FullCode[dat$ShortCode=="37"])
#3701 3702 3714 3718 3733 3735 3736 3742 3743 3745 
#80    4   26    1    2    1   19    1   23    6
26+1+2+1+19+1+23+6
#there are 79 observations not mentioned in Title 18.
#3714 is "Carless driving"
#3718 is "Minor Prohib from Operating w"
#3733 is "Fleeing or Attempting to Elude Police Officer"
#3735 is "Aggravated Assault While DUI"
#3736 is "Reckless Driving"
#3742 is  "Accidents Involving Death or Personal Injury"
#3743 is "Accidents Involving Damage to Attended Veh or Property"
#3745 is " Accidents Involving Damage to Unattended Veh or Property"
#All offenses except 3701 and 3702 are written in Title 75 instead of Title 18
dat<-filter(dat,!FullCode=="3714"&!FullCode=="3718"&!FullCode=="3733"&!FullCode=="3735"&
              !FullCode=="3736"&!FullCode=="3742"&!FullCode=="3743"&!FullCode=="3745")
#we cancel out these observations belonged to title 75

table(dat$FullCode[dat$ShortCode=="38"])
#3802 
#100 
#"3802" represents 3802 Driving under influence of alcohol or controlled substance 
#under Title 75.

table(dat$FullCode[dat$ShortCode=="38"])
#3802 
#100 
#"3802" represents 3802 Driving under influence of alcohol or controlled substance 
#under Title 75.

table(dat$FullCode[dat$ShortCode=="13"])
#1301 1311 1314 1332 1371 1372 1373 1375 
#50    5    1    4   11    5    3    1 
#all of them are under chapter 13 Registration of Vehicles of Title 75



#We will only focus on incidents coded based on Title 18 as "27","13(a)","9","39","55",
#"51","35","37","49","38","61","43","59"
minidat<-filter(dat,ShortCode=="27"|ShortCode=="13(a)"|ShortCode=="9"|ShortCode=="39"
       |ShortCode=="55"|ShortCode=="51"|ShortCode=="35"|ShortCode=="37"|
         ShortCode=="49"|ShortCode=="38"|ShortCode=="61"|ShortCode=="43"|ShortCode=="59")


