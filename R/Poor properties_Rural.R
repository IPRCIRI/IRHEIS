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

#Load Data
load(file=paste0(Settings$HEISProcessedPath,"CBN_Urban","95.rda"))
load(file=paste0(Settings$HEISProcessedPath,"CBN_Rural","95.rda"))
CBN95<-rbind(CBN_Urban,CBN_Rural)

#Add house properties
T95P2 <- read_dta("D:/R/T95P2.dta")
R95P2 <- read_dta("D:/R/R95P2.dta")
U95P2 <- read_dta("D:/R/U95P2.dta")

CBN95<-merge(CBN95,T95P2,by =c("HHID"),all.x=TRUE)
CBN_Urban<-merge(CBN_Urban,U95P2,by =c("HHID"),all.x=TRUE)
CBN_Rural<-merge(CBN_Rural,R95P2,by =c("HHID"),all.x=TRUE)

load(file=paste0(Settings$HEISProcessedPath,"CBNPoor_Urban","95.rda"))
load(file=paste0(Settings$HEISProcessedPath,"CBNPoor_Rural","95.rda"))
CBN_Poor95<-rbind(CBNPoor_Urban,CBNPoor_Rural)

CBN_Poor95<-merge(CBN_Poor95,T95P2,by =c("HHID"),all.x=TRUE)
CBNPoor_Urban<-merge(CBNPoor_Urban,U95P2,by =c("HHID"),all.x=TRUE)
CBNPoor_Rural<-merge(CBNPoor_Rural,R95P2,by =c("HHID"),all.x=TRUE)


CBN_NonPoor_Urban<-CBN_Urban[Poor11==0]
CBN_NonPoor_Rural<-CBN_Rural[Poor11==0]
CBN_NonPoor95<-rbind(CBN_NonPoor_Urban,CBN_NonPoor_Rural)

CBN95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Poor95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBNPoor_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBNPoor_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]

##### Age #####
#Calculation
Age<-CBN_NonPoor_Rural[,Age:=weighted.mean(HAge,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Age,ProvinceCode2,Weight)]
Age1<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Age1[,Weight:=NULL]

Age<-CBNPoor_Rural[,Poor_Age:=weighted.mean(HAge,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age,ProvinceCode2,Weight)]
Age2<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBN_NonPoor_Rural[,Poor_Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_40,ProvinceCode2,Weight)]
Age3<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBN_NonPoor_Rural[,Poor_Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_4050,ProvinceCode2,Weight)]
Age4<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBN_NonPoor_Rural[,Poor_Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_50,ProvinceCode2,Weight)]
Age5<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBNPoor_Rural[,Poor_Age:=weighted.mean(HAge,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age,ProvinceCode2,Weight)]
Age2<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBNPoor_Rural[,Poor_Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_40,ProvinceCode2,Weight)]
Age6<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBNPoor_Rural[,Poor_Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_4050,ProvinceCode2,Weight)]
Age7<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Age<-CBNPoor_Rural[,Poor_Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Age<-Age[,.(Poor_Age_50,ProvinceCode2,Weight)]
Age8<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

AgeT<-merge(Age1,Age2,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age3,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age4,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age5,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age6,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age7,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age8,by =c("ProvinceCode2"),all.x=TRUE)
AgeT[,Weight:=NULL]
write.xlsx(AgeT, "D:/R/Poors_Rural.xlsx",sheetName = "Age1")


Age<-CBN_Rural[,Age_40:=weighted.mean(ifelse(HAge<=40,1,0),Weight),by=Poor11][order(Poor11)]
Age<-Age[,.(Age_40,Poor11,Weight)]
Age6<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Age6[,Weight:=NULL]

Age<-CBN_Rural[,Age_4050:=weighted.mean(ifelse(HAge>40 & HAge<50,1,0),Weight),by=Poor11][order(Poor11)]
Age<-Age[,.(Age_4050,Poor11,Weight)]
Age7<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]

Age<-CBN_Rural[,Age_50:=weighted.mean(ifelse(HAge>=50,1,0),Weight),by=Poor11][order(Poor11)]
Age<-Age[,.(Age_50,Poor11,Weight)]
Age8<-Age[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]

AgeT<-merge(Age6,Age7,by =c("Poor11"),all.x=TRUE)
AgeT[,Weight:=NULL]
AgeT<-merge(AgeT,Age8,by =c("Poor11"),all.x=TRUE)
AgeT[,Weight:=NULL]
write.xlsx(AgeT, "D:/R/Poors_Rural.xlsx",sheetName = "Age2",append = TRUE)

#Graphs
#sm.density.compare(CBN_Rural$HAge, CBN_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$HAge, CBNPoor_Rural$cluster)

#sm.density.compare(CBN_Rural$HAge, CBN_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$HAge, CBNPoor_Rural$cluster)

#sm.density.compare(CBN_NonPoor_Rural$HAge, CBN_NonPoor_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$HAge, CBNPoor_Rural$cluster)


#hist(CBNPoor_Rural$HAge,breaks=12, col="red")
#plot(density(CBNPoor_Rural$HAge))
#plot(density(CBNPoor_Rural$HAge,weights=CBNPoor_Rural$Weight),type='l',bty='n')
#polygon(density(CBNPoor_Rural$HAge), col="red", border="blue")
#sm.density.compare(CBNPoor_Rural$HAge, CBNPoor_Rural$cluster,weights=CBNPoor_Rural$Weight)
#boxplot(CBNPoor_Rural$HAge, horizontal = TRUE,col="red")


##### Sex #####
Female<-CBN_NonPoor_Rural[,Female_Poors:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=ProvinceCode][order(ProvinceCode)]
Female<-Female[,.(Female_Poors,ProvinceCode2,Weight)]
Female1<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Female1[,Weight:=NULL]

Female<-CBNPoor_Rural[,Female:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=ProvinceCode][order(ProvinceCode)]
Female<-Female[,.(Female,ProvinceCode2,Weight)]
Female2<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Female1<-merge(Female1,Female2,by =c("ProvinceCode2"),all.x=TRUE)
Female1[,Weight:=NULL]
write.xlsx(Female1, "D:/R/Poors_Rural.xlsx",sheetName = "Female1",append = TRUE)

Female<-CBN_Rural[,Female:=weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=Poor11][order(Poor11)]
Female<-Female[,.(Female,Poor11,Weight)]
Female3<-Female[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Female3[,Weight:=NULL]
write.xlsx(Female3, "D:/R/Poors_Rural.xlsx",sheetName = "Female2",append = TRUE)

##### Size #####
Size<-CBN_NonPoor_Rural[,Size:=weighted.mean(Size,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Size<-Size[,.(Size,ProvinceCode2,Weight)]
Size1<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Size1[,Weight:=NULL]

Size<-CBNPoor_Rural[,Poor_Size:=weighted.mean(Size,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Size<-Size[,.(Poor_Size,ProvinceCode2,Weight)]
Size2<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

SizeT<-merge(Size1,Size2,by =c("ProvinceCode2"),all.x=TRUE)
SizeT[,Weight:=NULL]
write.xlsx(SizeT, "D:/R/Poors_Rural.xlsx",sheetName = "Size1",append = TRUE)

Size<-CBN_Rural[,Size:=weighted.mean(Size,Weight),by=Poor11][order(Poor11)]
Size<-Size[,.(Size,Poor11,Weight)]
Size3<-Size[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Size3[,Weight:=NULL]
write.xlsx(Size3, "D:/R/Poors_Rural.xlsx",sheetName = "Size2",append = TRUE)


#sm.density.compare(CBN_Rural$Size, CBN_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$Size, CBNPoor_Rural$cluster)

#sm.density.compare(CBN_Rural$Size, CBN_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$Size, CBNPoor_Rural$cluster)

#sm.density.compare(CBN_NonPoor_Rural$Size, CBN_NonPoor_Rural$Poor11)
#sm.density.compare(CBNPoor_Rural$Size, CBNPoor_Rural$cluster)


##### Literature #####
Literate<-CBN_NonPoor_Rural[,Literate_Poors:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Literate<-Literate[,.(Literate_Poors,ProvinceCode2,Weight)]
Literate1<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Literate1[,Weight:=NULL]

Literate<-CBNPoor_Rural[,Literate:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Literate<-Literate[,.(Literate,ProvinceCode2,Weight)]
Literate2<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Literate1<-merge(Literate1,Literate2,by =c("ProvinceCode2"),all.x=TRUE)
Literate1[,Weight:=NULL]
write.xlsx(Literate1, "D:/R/Poors_Rural.xlsx",sheetName = "Literate1",append = TRUE)

Literate<-CBN_Rural[,Literate:=weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=Poor11][order(Poor11)]
Literate<-Literate[,.(Literate,Poor11,Weight)]
Literate3<-Literate[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Literate3[,Weight:=NULL]
write.xlsx(Literate3, "D:/R/Poors_Rural.xlsx",sheetName = "Literate2",append = TRUE)


##### Education #####

#Education<-CBN_NonPoor_Rural[,Education_Poors1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,
#  ifelse(HEduLevel0 %in% "Elementary",2,
#ifelse(HEduLevel0 %in% "Middle",3,
#ifelse(HEduLevel0 %in% "High",4,
#ifelse(HEduLevel0 %in% "Pre",5,
#ifelse(HEduLevel0 %in% "University",6,0)))))),Weight),by=ProvinceCode2][order(ProvinceCode2)]

Education<-CBN_NonPoor_Rural[,Education_Poors1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors1,ProvinceCode2,Weight)]
Education1<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Education1[,Weight:=NULL]

Education<-CBN_NonPoor_Rural[,Education_Poors2:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors2,ProvinceCode2,Weight)]
Education2<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBN_NonPoor_Rural[,Education_Poors3:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors3,ProvinceCode2,Weight)]
Education3<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBN_NonPoor_Rural[,Education_Poors4:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors4,ProvinceCode2,Weight)]
Education4<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBN_NonPoor_Rural[,Education_Poors5:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors5,ProvinceCode2,Weight)]
Education5<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBN_NonPoor_Rural[,Education_Poors6:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors6,ProvinceCode2,Weight)]
Education6<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBNPoor_Rural[,Education_Poors7:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors7,ProvinceCode2,Weight)]
Education7<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Education7[,Weight:=NULL]

Education<-CBNPoor_Rural[,Education_Poors8:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors8,ProvinceCode2,Weight)]
Education8<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBNPoor_Rural[,Education_Poors9:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors9,ProvinceCode2,Weight)]
Education9<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBNPoor_Rural[,Education_Poors10:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors10,ProvinceCode2,Weight)]
Education10<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBNPoor_Rural[,Education_Poors11:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors11,ProvinceCode2,Weight)]
Education11<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education<-CBNPoor_Rural[,Education_Poors12:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Education<-Education[,.(Education_Poors12,ProvinceCode2,Weight)]
Education12<-Education[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Education1<-merge(Education1,Education2,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education3,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education4,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education5,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education6,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education7,by =c("ProvinceCode2"),all.x=TRUE)
Education1<-merge(Education1,Education8,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education9,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education10,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education11,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
Education1<-merge(Education1,Education12,by =c("ProvinceCode2"),all.x=TRUE)
Education1[,Weight:=NULL]
write.xlsx(Education1, "D:/R/Poors_Rural.xlsx",sheetName = "Education1",append = TRUE)


Education13<-CBN_Rural[,Education1:=weighted.mean(ifelse(HEduLevel0 %in% "Illiterate",1,0),Weight),by=Poor11][order(Poor11)]
Education13<-Education13[,.(Education1,Poor11,Weight)]
Education13<-Education13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education13[,Weight:=NULL]

Education14<-CBN_Rural[,Education2:=weighted.mean(ifelse(HEduLevel0 %in% "Elementary",1,0),Weight),by=Poor11][order(Poor11)]
Education14<-Education14[,.(Education2,Poor11,Weight)]
Education14<-Education14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education14[,Weight:=NULL]

Education15<-CBN_Rural[,Education3:=weighted.mean(ifelse(HEduLevel0 %in% "Middle",1,0),Weight),by=Poor11][order(Poor11)]
Education15<-Education15[,.(Education3,Poor11,Weight)]
Education15<-Education15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education15[,Weight:=NULL]

Education16<-CBN_Rural[,Education4:=weighted.mean(ifelse(HEduLevel0 %in% "High",1,0),Weight),by=Poor11][order(Poor11)]
Education16<-Education16[,.(Education4,Poor11,Weight)]
Education16<-Education16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education16[,Weight:=NULL]

Education17<-CBN_Rural[,Education5:=weighted.mean(ifelse(HEduLevel0 %in% "Pre",1,0),Weight),by=Poor11][order(Poor11)]
Education17<-Education17[,.(Education5,Poor11,Weight)]
Education17<-Education17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education17[,Weight:=NULL]

Education18<-CBN_Rural[,Education6:=weighted.mean(ifelse(HEduLevel0 %in% "University",1,0),Weight),by=Poor11][order(Poor11)]
Education18<-Education18[,.(Education6,Poor11,Weight)]
Education18<-Education18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Education18[,Weight:=NULL]

Education2<-merge(Education13,Education14,by =c("Poor11"),all.x=TRUE)
Education2<-merge(Education2,Education15,by =c("Poor11"),all.x=TRUE)
Education2<-merge(Education2,Education16,by =c("Poor11"),all.x=TRUE)
Education2<-merge(Education2,Education17,by =c("Poor11"),all.x=TRUE)
Education2<-merge(Education2,Education18,by =c("Poor11"),all.x=TRUE)
write.xlsx(Education2, "D:/R/Poors_Rural.xlsx",sheetName = "Education2",append = TRUE)



##### Activity #####
Activity<-CBN_NonPoor_Rural[,Activity_Poors1:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors1,ProvinceCode2,Weight)]
Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Activity1[,Weight:=NULL]

Activity<-CBN_NonPoor_Rural[,Activity_Poors2:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors2,ProvinceCode2,Weight)]
Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBN_NonPoor_Rural[,Activity_Poors3:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors3,ProvinceCode2,Weight)]
Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBN_NonPoor_Rural[,Activity_Poors4:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors4,ProvinceCode2,Weight)]
Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBN_NonPoor_Rural[,Activity_Poors5:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors5,ProvinceCode2,Weight)]
Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBN_NonPoor_Rural[,Activity_Poors6:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors6,ProvinceCode2,Weight)]
Activity6<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBNPoor_Rural[,Activity_Poors7:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors7,ProvinceCode2,Weight)]
Activity7<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Activity7[,Weight:=NULL]

Activity<-CBNPoor_Rural[,Activity_Poors8:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors8,ProvinceCode2,Weight)]
Activity8<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBNPoor_Rural[,Activity_Poors9:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors9,ProvinceCode2,Weight)]
Activity9<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBNPoor_Rural[,Activity_Poors10:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors10,ProvinceCode2,Weight)]
Activity10<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBNPoor_Rural[,Activity_Poors11:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors11,ProvinceCode2,Weight)]
Activity11<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity<-CBNPoor_Rural[,Activity_Poors12:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
Activity<-Activity[,.(Activity_Poors12,ProvinceCode2,Weight)]
Activity12<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Activity1<-merge(Activity1,Activity2,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity3,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity4,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity5,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity6,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity7,by =c("ProvinceCode2"),all.x=TRUE)
Activity1<-merge(Activity1,Activity8,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity9,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity10,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity11,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
Activity1<-merge(Activity1,Activity12,by =c("ProvinceCode2"),all.x=TRUE)
Activity1[,Weight:=NULL]
write.xlsx(Activity1, "D:/R/Poors_Rural.xlsx",sheetName = "Activity1",append = TRUE)


Activity13<-CBN_Rural[,Activity1:=weighted.mean(ifelse(HActivityState %in% "Employed",1,0),Weight),by=Poor11][order(Poor11)]
Activity13<-Activity13[,.(Activity1,Poor11,Weight)]
Activity13<-Activity13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity13[,Weight:=NULL]

Activity14<-CBN_Rural[,Activity2:=weighted.mean(ifelse(HActivityState %in% "Unemployed",1,0),Weight),by=Poor11][order(Poor11)]
Activity14<-Activity14[,.(Activity2,Poor11,Weight)]
Activity14<-Activity14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity14[,Weight:=NULL]

Activity15<-CBN_Rural[,Activity3:=weighted.mean(ifelse(HActivityState %in% "Income without Work",1,0),Weight),by=Poor11][order(Poor11)]
Activity15<-Activity15[,.(Activity3,Poor11,Weight)]
Activity15<-Activity15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity15[,Weight:=NULL]

Activity16<-CBN_Rural[,Activity4:=weighted.mean(ifelse(HActivityState %in% "Student",1,0),Weight),by=Poor11][order(Poor11)]
Activity16<-Activity16[,.(Activity4,Poor11,Weight)]
Activity16<-Activity16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity16[,Weight:=NULL]

Activity17<-CBN_Rural[,Activity5:=weighted.mean(ifelse(HActivityState %in% "Housekeeper",1,0),Weight),by=Poor11][order(Poor11)]
Activity17<-Activity17[,.(Activity5,Poor11,Weight)]
Activity17<-Activity17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity17[,Weight:=NULL]

Activity18<-CBN_Rural[,Activity6:=weighted.mean(ifelse(HActivityState %in% "Other",1,0),Weight),by=Poor11][order(Poor11)]
Activity18<-Activity18[,.(Activity6,Poor11,Weight)]
Activity18<-Activity18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Activity18[,Weight:=NULL]

Activity2<-merge(Activity13,Activity14,by =c("Poor11"),all.x=TRUE)
Activity2<-merge(Activity2,Activity15,by =c("Poor11"),all.x=TRUE)
Activity2<-merge(Activity2,Activity16,by =c("Poor11"),all.x=TRUE)
Activity2<-merge(Activity2,Activity17,by =c("Poor11"),all.x=TRUE)
Activity2<-merge(Activity2,Activity18,by =c("Poor11"),all.x=TRUE)
write.xlsx(Activity2, "D:/R/Poors_Rural.xlsx",sheetName = "Activity2",append = TRUE)




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
