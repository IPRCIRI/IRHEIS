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

##### HouseOwn #####

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors1,ProvinceCode2,Weight)]
HouseOwn1<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
HouseOwn1[,Weight:=NULL]

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors2,ProvinceCode2,Weight)]
HouseOwn2<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors3,ProvinceCode2,Weight)]
HouseOwn3<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors4,ProvinceCode2,Weight)]
HouseOwn4<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors5,ProvinceCode2,Weight)]
HouseOwn5<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_NonPoor_Rural[,HouseOwn_Poors6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors6,ProvinceCode2,Weight)]
HouseOwn6<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors7:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors7,ProvinceCode2,Weight)]
HouseOwn7<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
HouseOwn7[,Weight:=NULL]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors8:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors8,ProvinceCode2,Weight)]
HouseOwn8<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors9:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors9,ProvinceCode2,Weight)]
HouseOwn9<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors10:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors10,ProvinceCode2,Weight)]
HouseOwn10<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors11:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors11,ProvinceCode2,Weight)]
HouseOwn11<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBNPoor_Rural[,HouseOwn_Poors12:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors12,ProvinceCode2,Weight)]
HouseOwn12<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn1<-merge(HouseOwn1,HouseOwn2,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn3,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn4,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn5,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn6,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn7,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1<-merge(HouseOwn1,HouseOwn8,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn9,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn10,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn11,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn12,by =c("ProvinceCode2"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
write.xlsx(HouseOwn1, "D:/R/Poors_House2_Rural.xlsx",sheetName = "HouseOwn1")


HouseOwn13<-CBN_Rural[,HouseOwn1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn13<-HouseOwn13[,.(HouseOwn1,Poor11,Weight)]
HouseOwn13<-HouseOwn13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn13[,Weight:=NULL]

HouseOwn14<-CBN_Rural[,HouseOwn2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn14<-HouseOwn14[,.(HouseOwn2,Poor11,Weight)]
HouseOwn14<-HouseOwn14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn14[,Weight:=NULL]

HouseOwn15<-CBN_Rural[,HouseOwn3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn15<-HouseOwn15[,.(HouseOwn3,Poor11,Weight)]
HouseOwn15<-HouseOwn15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn15[,Weight:=NULL]

HouseOwn16<-CBN_Rural[,HouseOwn4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn16<-HouseOwn16[,.(HouseOwn4,Poor11,Weight)]
HouseOwn16<-HouseOwn16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn16[,Weight:=NULL]

HouseOwn17<-CBN_Rural[,HouseOwn5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn17<-HouseOwn17[,.(HouseOwn5,Poor11,Weight)]
HouseOwn17<-HouseOwn17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn17[,Weight:=NULL]

HouseOwn18<-CBN_Rural[,HouseOwn6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn18<-HouseOwn18[,.(HouseOwn6,Poor11,Weight)]
HouseOwn18<-HouseOwn18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn18[,Weight:=NULL]

HouseOwn2<-merge(HouseOwn13,HouseOwn14,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn15,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn16,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn17,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn18,by =c("Poor11"),all.x=TRUE)
write.xlsx(HouseOwn2, "D:/R/Poors_House2_Rural.xlsx",sheetName = "HouseOwn2",append = TRUE)

##### skeleton #####

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors1:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors1,ProvinceCode2,Weight)]
skeleton1<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
skeleton1[,Weight:=NULL]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors2:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors2,ProvinceCode2,Weight)]
skeleton2<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors3:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors3,ProvinceCode2,Weight)]
skeleton3<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors4:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors4,ProvinceCode2,Weight)]
skeleton4<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors5:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors5,ProvinceCode2,Weight)]
skeleton5<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors6:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors6,ProvinceCode2,Weight)]
skeleton6<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors13:=weighted.mean(ifelse(skeleton==7,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors13,ProvinceCode2,Weight)]
skeleton13<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors14:=weighted.mean(ifelse(skeleton==8,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors14,ProvinceCode2,Weight)]
skeleton14<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors15:=weighted.mean(ifelse(skeleton==20,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors15,ProvinceCode2,Weight)]
skeleton15<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBN_NonPoor_Rural[,skeleton_Poors16:=weighted.mean(ifelse(skeleton==10,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors16,ProvinceCode2,Weight)]
skeleton16<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]


skeleton<-CBNPoor_Rural[,skeleton_Poors7:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors7,ProvinceCode2,Weight)]
skeleton7<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
skeleton7[,Weight:=NULL]

skeleton<-CBNPoor_Rural[,skeleton_Poors8:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors8,ProvinceCode2,Weight)]
skeleton8<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors9:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors9,ProvinceCode2,Weight)]
skeleton9<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors10:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors10,ProvinceCode2,Weight)]
skeleton10<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors11:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors11,ProvinceCode2,Weight)]
skeleton11<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors12:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors12,ProvinceCode2,Weight)]
skeleton12<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors17:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors17,ProvinceCode2,Weight)]
skeleton17<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors18:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors18,ProvinceCode2,Weight)]
skeleton18<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors19:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors19,ProvinceCode2,Weight)]
skeleton19<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton<-CBNPoor_Rural[,skeleton_Poors20:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
skeleton<-skeleton[,.(skeleton_Poors20,ProvinceCode2,Weight)]
skeleton20<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

skeleton1<-merge(skeleton1,skeleton2,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton3,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton4,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton5,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton6,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton13,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton14,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton15,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton16,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton7,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1<-merge(skeleton1,skeleton8,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton9,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton10,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton11,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton12,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton17,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton18,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton19,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton20,by =c("ProvinceCode2"),all.x=TRUE)
skeleton1[,Weight:=NULL]
write.xlsx(skeleton1, "D:/R/Poors_House2_Rural.xlsx",sheetName = "skeleton1",append = TRUE)


skeleton13<-CBN_Rural[,skeleton1:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=Poor11][order(Poor11)]
skeleton13<-skeleton13[,.(skeleton1,Poor11,Weight)]
skeleton13<-skeleton13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton13[,Weight:=NULL]

skeleton14<-CBN_Rural[,skeleton2:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=Poor11][order(Poor11)]
skeleton14<-skeleton14[,.(skeleton2,Poor11,Weight)]
skeleton14<-skeleton14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton14[,Weight:=NULL]

skeleton15<-CBN_Rural[,skeleton3:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=Poor11][order(Poor11)]
skeleton15<-skeleton15[,.(skeleton3,Poor11,Weight)]
skeleton15<-skeleton15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton15[,Weight:=NULL]

skeleton16<-CBN_Rural[,skeleton4:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=Poor11][order(Poor11)]
skeleton16<-skeleton16[,.(skeleton4,Poor11,Weight)]
skeleton16<-skeleton16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton16[,Weight:=NULL]

skeleton17<-CBN_Rural[,skeleton5:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=Poor11][order(Poor11)]
skeleton17<-skeleton17[,.(skeleton5,Poor11,Weight)]
skeleton17<-skeleton17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton17[,Weight:=NULL]

skeleton18<-CBN_Rural[,skeleton6:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=Poor11][order(Poor11)]
skeleton18<-skeleton18[,.(skeleton6,Poor11,Weight)]
skeleton18<-skeleton18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton18[,Weight:=NULL]

skeleton19<-CBN_Rural[,skeleton7:=weighted.mean(ifelse(skeleton==7,1,0),Weight),by=Poor11][order(Poor11)]
skeleton19<-skeleton19[,.(skeleton7,Poor11,Weight)]
skeleton19<-skeleton19[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton19[,Weight:=NULL]

skeleton20<-CBN_Rural[,skeleton8:=weighted.mean(ifelse(skeleton==8,1,0),Weight),by=Poor11][order(Poor11)]
skeleton20<-skeleton20[,.(skeleton8,Poor11,Weight)]
skeleton20<-skeleton20[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton20[,Weight:=NULL]

skeleton21<-CBN_Rural[,skeleton10:=weighted.mean(ifelse(skeleton==10,1,0),Weight),by=Poor11][order(Poor11)]
skeleton21<-skeleton21[,.(skeleton10,Poor11,Weight)]
skeleton21<-skeleton21[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton21[,Weight:=NULL]

skeleton22<-CBN_Rural[,skeleton20:=weighted.mean(ifelse(skeleton==20,1,0),Weight),by=Poor11][order(Poor11)]
skeleton22<-skeleton22[,.(skeleton20,Poor11,Weight)]
skeleton22<-skeleton22[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
skeleton22[,Weight:=NULL]

skeleton2<-merge(skeleton13,skeleton14,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton15,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton16,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton17,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton18,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton19,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton20,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton21,by =c("Poor11"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton22,by =c("Poor11"),all.x=TRUE)
write.xlsx(skeleton2, "D:/R/Poors_House2_Rural.xlsx",sheetName = "skeleton2",append = TRUE)


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)