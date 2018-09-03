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

HouseOwn<-CBN95[,HouseOwn_Poors1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors1,ProvinceCode2,Weight)]
HouseOwn1<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
HouseOwn1[,Weight:=NULL]

HouseOwn<-CBN95[,HouseOwn_Poors2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors2,ProvinceCode2,Weight)]
HouseOwn2<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN95[,HouseOwn_Poors3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors3,ProvinceCode2,Weight)]
HouseOwn3<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN95[,HouseOwn_Poors4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors4,ProvinceCode2,Weight)]
HouseOwn4<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN95[,HouseOwn_Poors5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors5,ProvinceCode2,Weight)]
HouseOwn5<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN95[,HouseOwn_Poors6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors6,ProvinceCode2,Weight)]
HouseOwn6<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors7:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors7,ProvinceCode2,Weight)]
HouseOwn7<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
HouseOwn7[,Weight:=NULL]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors8:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors8,ProvinceCode2,Weight)]
HouseOwn8<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors9:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors9,ProvinceCode2,Weight)]
HouseOwn9<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors10:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors10,ProvinceCode2,Weight)]
HouseOwn10<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors11:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors11,ProvinceCode2,Weight)]
HouseOwn11<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors12:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=ProvinceCode2][order(ProvinceCode2)]
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
write.xlsx(HouseOwn1, "D:/R/Poors_House2.xlsx",sheetName = "HouseOwn1")


HouseOwn13<-CBN95[,HouseOwn1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn13<-HouseOwn13[,.(HouseOwn1,Poor11,Weight)]
HouseOwn13<-HouseOwn13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn13[,Weight:=NULL]

HouseOwn14<-CBN95[,HouseOwn2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn14<-HouseOwn14[,.(HouseOwn2,Poor11,Weight)]
HouseOwn14<-HouseOwn14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn14[,Weight:=NULL]

HouseOwn15<-CBN95[,HouseOwn3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn15<-HouseOwn15[,.(HouseOwn3,Poor11,Weight)]
HouseOwn15<-HouseOwn15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn15[,Weight:=NULL]

HouseOwn16<-CBN95[,HouseOwn4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn16<-HouseOwn16[,.(HouseOwn4,Poor11,Weight)]
HouseOwn16<-HouseOwn16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn16[,Weight:=NULL]

HouseOwn17<-CBN95[,HouseOwn5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn17<-HouseOwn17[,.(HouseOwn5,Poor11,Weight)]
HouseOwn17<-HouseOwn17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn17[,Weight:=NULL]

HouseOwn18<-CBN95[,HouseOwn6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=Poor11][order(Poor11)]
HouseOwn18<-HouseOwn18[,.(HouseOwn6,Poor11,Weight)]
HouseOwn18<-HouseOwn18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
HouseOwn18[,Weight:=NULL]

HouseOwn2<-merge(HouseOwn13,HouseOwn14,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn15,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn16,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn17,by =c("Poor11"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn18,by =c("Poor11"),all.x=TRUE)
write.xlsx(HouseOwn2, "D:/R/Poors_House2.xlsx",sheetName = "HouseOwn2",append = TRUE)

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)