library(dplyr)
library(lubridate)
library(ggplot2)
#import data as dat
dat<-ArrestData
#check if anything wired about this dataset 
table(year(dat$ARRESTTIME))
#2014 2015 2016 2017 
#3    3 4141 2726 
#The frequencies of 2014 and 2015 is too small 

table(dat$AGE)
#0  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32 
#7  18  23  34  57  58  89 127 117 172 183 227 223 292 243 283 288 267 231 213 222 278 210 171 
#33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56 
#176 181 172 169 138 111 100 102 108  88  92  75  82  86  79  77  75  68  81 108  90  77  49  45 
#57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  79  80  82 
#78  39  33  35  29  26  26   7   3   6   7  10  15   9   4   1   1   2   2   2   1   1   1   2 
#88 117 999 
#1   1   1 
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
  frame<-frame[year(frame$ARRESTTIME)>2015,]
  return(frame)
}

easydat<-easierdat(dat$OFFENSES,dat)
nrow(ArrestData)-nrow(easydat)
#Noticed it only get rid of 6 rows 
#The dat is now with NAs and with wired ages.
#Becasue we need to count the frequency of different types of crimes


#We checked the result with other groups to make sure we use the same data
#from function
sort(table(easydat$ShortCode),decreasing = T)[1:10]

#function two
#More detail of this function is below
easierierdat<-function(frames){
  frames<-frames[order(-table(frames$ShortCode)[frames$ShortCode]),]
  frames$Count<-as.numeric(reorder(frames$ShortCode, -ave(as.numeric(frames$ShortCode), frames$ShortCode, FUN = length)))
  lgth<-length(frames$Count)-length(boxplot.stats(frames$Count)$out)
  frames<-frames[1:lgth,]
  frames<-filter(frames,!ShortCode=="unique(frames$ShortCode[frames$Count==table(boxplot.stats(frames$Count)$out)[1]])")
  frames<-filter(frames,!FullCode=="3714"&!FullCode=="3718"&!FullCode=="3733"&!FullCode=="3735"&
                   !FullCode=="3736"&!FullCode=="3742"&!FullCode=="3743"&!FullCode=="3745")
  minidat<-filter(frames,ShortCode=="27"|ShortCode=="13(a)"|ShortCode=="9"|ShortCode=="39"|
                    ShortCode=="55"|ShortCode=="33"|ShortCode=="35"|ShortCode=="49"|
                    ShortCode=="37"|ShortCode=="61"|ShortCode=="43"|ShortCode=="31")
  minidat<-filter(minidat,!is.na(X)|!is.na(Y))
  return(minidat)
}

minidata<-easierierdat(easydat)

#--------------------------------detail of second data clean function 
#The distribution of different types of offenses

#get the outliers when treating different types as numbers
sdat$Count<-as.numeric(reorder(sdat$ShortCode, -ave(as.numeric(sdat$ShortCode), sdat$ShortCode, FUN = length)))
table(boxplot.stats(sdat$Count)$out)
sdat$ShortCode[sdat$Count==19]
#59
length(boxplot.stats(sdat$Count)$out)
#213

#we need to get rid of the tail of the  213 rows and offenses coded as "59"
length(sdat$`_id`)-213#we only neew 1 tp 6661 row
sdat<-sdat[1:6661,]
sdat<-filter(sdat,!ShortCode=="59")
#check the result
table(sdat$Count)
sort(table(sdat$ShortCode),decreasing = T)

#Now the distribution is more balanced than we saw in graph one.
#With 17 different types of short codes.
#27 13(a)     9    39    95    90    15    55    33    35    49    37 
#1746   904   646   639   378   317   281   278   220   211   189   181 
#38    61    13    43    31 
#110    97    91    77    73 



#To decode we look the information on website http://www.legis.state.pa.us/wu01/li/li/ct/htm/18/18.htm
#, which give us the information about Title 18 in Consolidated Statutes. 
#27 represents chapter 27 of Title 18 with the title "Assult"
#13(a) represents "Controlled Substance, Drug, Device and Cosmetic Act"
#9 represents chapter 9 of Title 18 with the tile "Inchoate Crimes"
#39 represents chapter 39 of Title 18 with the title "Theft and Related Offenses"
#"55" represents chapter 55 of Title 18 with the title "Riot, Disorderly Conduct and Related Offenses"
#"33" represents chapter 33 of Title 18 with the title "Arson, Criminal Mischef And Other
#Property Destructions"
#"35" represents chapter 35 of Title 18 with the title "Burglary and Other Criminal Intrusion"
#"49" represents chapter 49 of Title 18 with the title "Falsification and Intimidation"
#"37" represents chapter 37 of Title 18 with the title "Robbery"(but with exceptions from other chapters)
#"61" represents chapter 61 of Title 18 with the title "Firearms and Other Dangerous Articles"
#"43" represents chapter 43 of Title 18 with the title "Offenses Against the Family"
#"31" represents chapter 31 of Title 18 with the title "Sexual Offenses"
#The major arrest reason of criminals  are not listed in Title 18.

table(sdat$FullCode[sdat$ShortCode=="95"])
#9501 
#378
#All the 246 "95" here represents "Bench Warrant"


table(sdat$FullCode[sdat$ShortCode=="90"])
#Offenses coded as "90" are actually "9015" and "9093"
#9015 9093 
#247   70 
#They are "9015 Failure To Appear/Arrest on Attachment Order" 
#and "Indirect Criminal Contempt"



table(sdat$FullCode[sdat$ShortCode=="15"])
#1501 1504 1511 1543 1574 
#45    1     3    231  1 
#1543" is form  Title 75 "Driving While Operating Privilege Is Suspended"
#"1543" is from  Title 75 "Drivers Required to be Licensed"
#"1501" is from Title 75 "Carrying and exhibiting driver's license on demand"
#"1574" is from Title 75 "Permitting unauthorized person to drive"
#All of these are titled as "Licensing of Drivers" from "Title 75" 

table(sdat$FullCode[sdat$ShortCode=="37"])
#3701 3702 3714 3718 3733 3735 3736 3742 3743 3745 
#  84    4   30    1    2    1   23    3   27    6 
30+1+2+1+23+3+27+6 
#there are 93 observations not mentioned in Title 18.
#3714 is "Carless driving"
#3718 is "Minor Prohib from Operating w"
#3733 is "Fleeing or Attempting to Elude Police Officer"
#3735 is "Aggravated Assault While DUI"
#3736 is "Reckless Driving"
#3742 is  "Accidents Involving Death or Personal Injury"
#3743 is "Accidents Involving Damage to Attended Veh or Property"
#3745 is " Accidents Involving Damage to Unattended Veh or Property"
#All offenses except 3701 and 3702 are written in Title 75 instead of Title 18
sdat<-filter(sdat,!FullCode=="3714"&!FullCode=="3718"&!FullCode=="3733"&!FullCode=="3735"&
              !FullCode=="3736"&!FullCode=="3742"&!FullCode=="3743"&!FullCode=="3745")
#we cancel out these observations belonged to title 75


table(sdat$FullCode[sdat$ShortCode=="38"])
#3802 
#110 
#"3802" represents 3802 Driving under influence of alcohol or controlled substance 
#under Title 75.

table(sdat$FullCode[sdat$ShortCode=="13"])
#1301 1311 1314 1332 1371 1372 1373 1375 
#58    5    1    1   5    11    4    1 
#all of them are under chapter 13 Registration of Vehicles of Title 75


#We will only focus on incidents coded based on Title 18 as 
#"27","13(a)","9","39","55",
#"51","35","37","49","38","61","43","59"
minidat<-filter(sdat,ShortCode=="27"|ShortCode=="13(a)"|ShortCode=="9"|ShortCode=="39"|
                  ShortCode=="55"|ShortCode=="33"|ShortCode=="35"|ShortCode=="49"|
                  ShortCode=="37"|ShortCode=="61"|ShortCode=="43"|ShortCode=="31")
nrow(minidat)
#5168

#to make sure every incident have an location
minidat<-filter(minidat,!is.na(X)|!is.na(Y))
nrow(minidat)
#4982
#There are 4982 incidents for the finaldata 

#we write an easyier for these steps at first
#and get minidata
#to check the result
all(minidat$`_id`==minidata$`_id`)

