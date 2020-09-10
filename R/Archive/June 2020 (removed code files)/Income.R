# Income

# households working in public sector.
#
# Copyright © 2017-2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Income =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Pub1.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Pub2.rda"))
 names(Pub2)<-c("HHID","IndPub")
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Prv1.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Prv2.rda"))
 names(Prv2)<-c("HHID","IndPrv")
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Buss1.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Buss2.rda"))
 names(Buss2)<-c("HHID","IndBuss")
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Agri1.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Agri2.rda"))
 names(Agri2)<-c("HHID","IndAgri")
 
 Pub2<-Pub2[IndPub==2]
 Prv2<-Prv2[IndPrv==2]
 Buss2<-Buss2[IndBuss==2]
 Agri2<-Agri2[IndAgri==2]
 
 WomanEmployed<-merge(Pub2,Prv2,all = TRUE)
 WomanEmployed<-merge(WomanEmployed,Buss2,all = TRUE)
 WomanEmployed<-merge(WomanEmployed,Agri2,all = TRUE)
 WomanEmployed[is.na(WomanEmployed)] <- 0
 WomanEmployed[,indivsum:=1]
 WomanEmployed<-WomanEmployed[,.(HHID,indivsum)]
 save(WomanEmployed, file = paste0(Settings$HEISProcessedPath,"Y",year,"WomanEmployed.rda"))
 
 Pub1<-Pub1[PubEarners<3]
 Prv1<-Prv1[PrvEarners<3]
 Buss1<-Buss1[BussEarners<3]
 Agri1<-Agri1[AgriEarners<3]
 
 NumberEmployed<-merge(Pub1,Prv1,all = TRUE)
 NumberEmployed<-merge(NumberEmployed,Buss1,all = TRUE)
 NumberEmployed<-merge(NumberEmployed,Agri1,all = TRUE)
 NumberEmployed[is.na(NumberEmployed)] <- 0
 NumberEmployed[,numbersum:=PubEarners+PrvEarners+BussEarners+AgriEarners]
 NumberEmployed<-NumberEmployed[,.(HHID,numbersum)]
 NumberEmployed<-NumberEmployed[numbersum<3]
 save(NumberEmployed, file = paste0(Settings$HEISProcessedPath,"Y",year,"NumberEmployed.rda"))
 
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
