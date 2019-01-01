#Poors properties.R
# 
# Copyright © 2018:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())
starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)
library(xlsx)
library(haven)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))

  #Load Data
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"NewFinalPoor.rda"))
  UNewFinalPoor<-NewFinalPoor[Region=="Urban"]
  RNewFinalPoor<-NewFinalPoor[Region=="Rural"]
  
  #Add house properties
  T95P2 <- read_dta("D:/R/T95P2.dta")
  R95P2 <- read_dta("D:/R/R95P2.dta")
  U95P2 <- read_dta("D:/R/U95P2.dta")
  
  CBN95<-merge(NewFinalPoor,T95P2,by =c("HHID"),all.x=TRUE)
  CBN_Urban<-merge(UNewFinalPoor,U95P2,by =c("HHID"),all.x=TRUE)
  CBN_Rural<-merge(RNewFinalPoor,R95P2,by =c("HHID"),all.x=TRUE)
  
  CBN_Poor95<-CBN95[FinalPoor==1]
  CBN_Poor_Urban<-CBN_Urban[FinalPoor==1]
  CBN_Poor_Rural<-CBN_Rural[FinalPoor==1]
  
  
  CBN_NonPoor_Urban<-CBN_Urban[FinalPoor==0]
  CBN_NonPoor_Rural<-CBN_Rural[FinalPoor==0]
  CBN_NonPoor95<-rbind(CBN_NonPoor_Urban,CBN_NonPoor_Rural)


##### Age #####
#Calculation
Age<-CBN_NonPoor95[,Age:=weighted.mean(HAge,Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Age,NewArea,Weight)]
Age1<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Age1[,Weight:=NULL]

Age<-CBN_Poor95[,Poor_Age:=weighted.mean(HAge,Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age,NewArea,Weight)]
Age2<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_NonPoor95[,Poor_Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_40,NewArea,Weight)]
Age3<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_NonPoor95[,Poor_Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_4050,NewArea,Weight)]
Age4<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_NonPoor95[,Poor_Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_50,NewArea,Weight)]
Age5<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_Poor95[,Poor_Age:=weighted.mean(HAge,Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age,NewArea,Weight)]
Age2<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_Poor95[,Poor_Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_40,NewArea,Weight)]
Age6<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_Poor95[,Poor_Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_4050,NewArea,Weight)]
Age7<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Age<-CBN_Poor95[,Poor_Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=NewArea][order(NewArea)]
Age<-Age[,.(Poor_Age_50,NewArea,Weight)]
Age8<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

AgeT<-merge(Age1,Age2,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age3,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age4,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age5,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age6,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age7,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age8,by =c("NewArea"),all.x=TRUE)
AgeT[,Weight:=NULL]
write.xlsx(AgeT, "D:/R/Poors.xlsx",sheetName = "Age1")


Age<-CBN95[,Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=FinalPoor][order(FinalPoor)]
Age<-Age[,.(Age_40,FinalPoor,Weight)]
Age6<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Age6[,Weight:=NULL]

Age<-CBN95[,Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=FinalPoor][order(FinalPoor)]
Age<-Age[,.(Age_4050,FinalPoor,Weight)]
Age7<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]

Age<-CBN95[,Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=FinalPoor][order(FinalPoor)]
Age<-Age[,.(Age_50,FinalPoor,Weight)]
Age8<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]

AgeT<-merge(Age6,Age7,by =c("FinalPoor"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age8,by =c("FinalPoor"),all.x=TRUE)
AgeT[,Weight:=NULL]
write.xlsx(AgeT, "D:/R/Poors.xlsx",sheetName = "Age2",append = TRUE)



##### Sex #####
Female<-CBN_NonPoor95[,Female_Poors:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=ProvinceCode][order(ProvinceCode)]
Female<-Female[,.(Female_Poors,NewArea,Weight)]
Female1<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Female1[,Weight:=NULL]

Female<-CBN_Poor95[,Female:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=ProvinceCode][order(ProvinceCode)]
Female<-Female[,.(Female,NewArea,Weight)]
Female2<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Female1<-merge(Female1,Female2,by =c("NewArea"),all.x=TRUE)
Female1[,Weight:=NULL]
write.xlsx(Female1, "D:/R/Poors.xlsx",sheetName = "Female1",append = TRUE)

Female<-CBN95[,Female:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Female<-Female[,.(Female,FinalPoor,Weight)]
Female3<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Female3[,Weight:=NULL]
write.xlsx(Female3, "D:/R/Poors.xlsx",sheetName = "Female2",append = TRUE)

##### Size #####
Size<-CBN_NonPoor95[,Size:=weighted.mean(Size,Weight),by=NewArea][order(NewArea)]
Size<-Size[,.(Size,NewArea,Weight)]
Size1<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Size1[,Weight:=NULL]

Size<-CBN_Poor95[,Poor_Size:=weighted.mean(Size,Weight),by=NewArea][order(NewArea)]
Size<-Size[,.(Poor_Size,NewArea,Weight)]
Size2<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

SizeT<-merge(Size1,Size2,by =c("NewArea"),all.x=TRUE)
SizeT[,Weight:=NULL]
write.xlsx(SizeT, "D:/R/Poors.xlsx",sheetName = "Size1",append = TRUE)

Size<-CBN95[,Size:=weighted.mean(Size,Weight),by=FinalPoor][order(FinalPoor)]
Size<-Size[,.(Size,FinalPoor,Weight)]
Size3<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Size3[,Weight:=NULL]
write.xlsx(Size3, "D:/R/Poors.xlsx",sheetName = "Size2",append = TRUE)




##### Literature #####
Literate<-CBN_NonPoor95[,Literate_Poors:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=NewArea][order(NewArea)]
Literate<-Literate[,.(Literate_Poors,NewArea,Weight)]
Literate1<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Literate1[,Weight:=NULL]

Literate<-CBN_Poor95[,Literate:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=NewArea][order(NewArea)]
Literate<-Literate[,.(Literate,NewArea,Weight)]
Literate2<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Literate1<-merge(Literate1,Literate2,by =c("NewArea"),all.x=TRUE)
Literate1[,Weight:=NULL]
write.xlsx(Literate1, "D:/R/Poors.xlsx",sheetName = "Literate1",append = TRUE)

Literate<-CBN95[,Literate:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Literate<-Literate[,.(Literate,FinalPoor,Weight)]
Literate3<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Literate3[,Weight:=NULL]
write.xlsx(Literate3, "D:/R/Poors.xlsx",sheetName = "Literate2",append = TRUE)


##### Education #####

#Education<-CBN_NonPoor95[,Education_Poors1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,
#  ifelse(HEduLevel0 %in% "Elementary",2,
#ifelse(HEduLevel0 %in% "Middle",3,
#ifelse(HEduLevel0 %in% "High",4,
#ifelse(HEduLevel0 %in% "Pre",5,
#ifelse(HEduLevel0 %in% "University",6,0)))))),Weight),by=NewArea][order(NewArea)]

Education<-CBN_NonPoor95[,Education_Poors1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors1,NewArea,Weight)]
Education1<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Education1[,Weight:=NULL]

Education<-CBN_NonPoor95[,Education_Poors2:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors2,NewArea,Weight)]
Education2<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_NonPoor95[,Education_Poors3:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors3,NewArea,Weight)]
Education3<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_NonPoor95[,Education_Poors4:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors4,NewArea,Weight)]
Education4<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_NonPoor95[,Education_Poors5:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors5,NewArea,Weight)]
Education5<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_NonPoor95[,Education_Poors6:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors6,NewArea,Weight)]
Education6<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_Poor95[,Education_Poors7:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors7,NewArea,Weight)]
Education7<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Education7[,Weight:=NULL]

Education<-CBN_Poor95[,Education_Poors8:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors8,NewArea,Weight)]
Education8<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_Poor95[,Education_Poors9:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors9,NewArea,Weight)]
Education9<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_Poor95[,Education_Poors10:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors10,NewArea,Weight)]
Education10<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_Poor95[,Education_Poors11:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors11,NewArea,Weight)]
Education11<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education<-CBN_Poor95[,Education_Poors12:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=NewArea][order(NewArea)]
Education<-Education[,.(Education_Poors12,NewArea,Weight)]
Education12<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Education1<-merge(Education1,Education2,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education3,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education4,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education5,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education6,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education7,by =c("NewArea"),all.x=TRUE)
Education1<-merge(Education1,Education8,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education9,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education10,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education11,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education12,by =c("NewArea"),all.x=TRUE)
Education1[,Weight:=NULL]
write.xlsx(Education1, "D:/R/Poors.xlsx",sheetName = "Education1",append = TRUE)


Education13<-CBN95[,Education1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education13<-Education13[,.(Education1,FinalPoor,Weight)]
Education13<-Education13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education13[,Weight:=NULL]

Education14<-CBN95[,Education2:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education14<-Education14[,.(Education2,FinalPoor,Weight)]
Education14<-Education14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education14[,Weight:=NULL]

Education15<-CBN95[,Education3:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education15<-Education15[,.(Education3,FinalPoor,Weight)]
Education15<-Education15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education15[,Weight:=NULL]

Education16<-CBN95[,Education4:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education16<-Education16[,.(Education4,FinalPoor,Weight)]
Education16<-Education16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education16[,Weight:=NULL]

Education17<-CBN95[,Education5:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education17<-Education17[,.(Education5,FinalPoor,Weight)]
Education17<-Education17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education17[,Weight:=NULL]

Education18<-CBN95[,Education6:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Education18<-Education18[,.(Education6,FinalPoor,Weight)]
Education18<-Education18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Education18[,Weight:=NULL]

Education2<-merge(Education13,Education14,by =c("FinalPoor"),all.x=TRUE)
Education2<-merge(Education2,Education15,by =c("FinalPoor"),all.x=TRUE)
Education2<-merge(Education2,Education16,by =c("FinalPoor"),all.x=TRUE)
Education2<-merge(Education2,Education17,by =c("FinalPoor"),all.x=TRUE)
Education2<-merge(Education2,Education18,by =c("FinalPoor"),all.x=TRUE)
write.xlsx(Education2, "D:/R/Poors.xlsx",sheetName = "Education2",append = TRUE)



##### Activity #####
Activity<-CBN_NonPoor95[,Activity_Poors1:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors1,NewArea,Weight)]
Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Activity1[,Weight:=NULL]

Activity<-CBN_NonPoor95[,Activity_Poors2:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors2,NewArea,Weight)]
Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_NonPoor95[,Activity_Poors3:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors3,NewArea,Weight)]
Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_NonPoor95[,Activity_Poors4:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors4,NewArea,Weight)]
Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_NonPoor95[,Activity_Poors5:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors5,NewArea,Weight)]
Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_NonPoor95[,Activity_Poors6:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors6,NewArea,Weight)]
Activity6<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_Poor95[,Activity_Poors7:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors7,NewArea,Weight)]
Activity7<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Activity7[,Weight:=NULL]

Activity<-CBN_Poor95[,Activity_Poors8:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors8,NewArea,Weight)]
Activity8<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_Poor95[,Activity_Poors9:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors9,NewArea,Weight)]
Activity9<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_Poor95[,Activity_Poors10:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors10,NewArea,Weight)]
Activity10<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_Poor95[,Activity_Poors11:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors11,NewArea,Weight)]
Activity11<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity<-CBN_Poor95[,Activity_Poors12:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=NewArea][order(NewArea)]
Activity<-Activity[,.(Activity_Poors12,NewArea,Weight)]
Activity12<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Activity1<-merge(Activity1,Activity2,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity3,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity4,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity5,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity6,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity7,by =c("NewArea"),all.x=TRUE)
Activity1<-merge(Activity1,Activity8,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity9,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity10,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity11,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity12,by =c("NewArea"),all.x=TRUE)
Activity1[,Weight:=NULL]
write.xlsx(Activity1, "D:/R/Poors.xlsx",sheetName = "Activity1",append = TRUE)


Activity13<-CBN95[,Activity1:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity13<-Activity13[,.(Activity1,FinalPoor,Weight)]
Activity13<-Activity13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity13[,Weight:=NULL]

Activity14<-CBN95[,Activity2:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity14<-Activity14[,.(Activity2,FinalPoor,Weight)]
Activity14<-Activity14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity14[,Weight:=NULL]

Activity15<-CBN95[,Activity3:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity15<-Activity15[,.(Activity3,FinalPoor,Weight)]
Activity15<-Activity15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity15[,Weight:=NULL]

Activity16<-CBN95[,Activity4:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity16<-Activity16[,.(Activity4,FinalPoor,Weight)]
Activity16<-Activity16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity16[,Weight:=NULL]

Activity17<-CBN95[,Activity5:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity17<-Activity17[,.(Activity5,FinalPoor,Weight)]
Activity17<-Activity17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity17[,Weight:=NULL]

Activity18<-CBN95[,Activity6:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=FinalPoor][order(FinalPoor)]
Activity18<-Activity18[,.(Activity6,FinalPoor,Weight)]
Activity18<-Activity18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Activity18[,Weight:=NULL]

Activity2<-merge(Activity13,Activity14,by =c("FinalPoor"),all.x=TRUE)
Activity2<-merge(Activity2,Activity15,by =c("FinalPoor"),all.x=TRUE)
Activity2<-merge(Activity2,Activity16,by =c("FinalPoor"),all.x=TRUE)
Activity2<-merge(Activity2,Activity17,by =c("FinalPoor"),all.x=TRUE)
Activity2<-merge(Activity2,Activity18,by =c("FinalPoor"),all.x=TRUE)
write.xlsx(Activity2, "D:/R/Poors.xlsx",sheetName = "Activity2",append = TRUE)

}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
