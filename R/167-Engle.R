# 167-Revising Engle
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Revising Engle =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

FinalCountryResults <- data.table(Year=NA_integer_,PovertyLine=NA_real_,
                                  Engle=NA_real_,Bundle_Value=NA_real_,
                                  Total_Exp_Month_Per=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

FinalRegionResults <- data.table(Year=NA_integer_,Region=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                 PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

FinalClusterResults <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                                  House_Share=NA_real_,FoodKCaloriesHH_Per=NA_real_,
                                  SampleSize=NA_integer_,
                                  Engle=NA_integer_,FPLine=NA_integer_,
                                  PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]



for(year in (90:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  #MD<-MD[cluster3==7]
  MD<-MD[,Clusterdiff:=ifelse(cluster3==7,1,0)]
  MD<-MD[,EngleH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                   TOriginalFoodExpenditure_Per<1.2*FPLine,
                 .(.N,Engel=weighted.mean(EngleH,Weight),
                   FPLine=mean(FPLine)),by=.(Region,cluster3)]
  

  EngleD<-EngleD[,Year:=year]
  EngleDD <- EngleD
  EngleDD <- subset(EngleDD, select = c(Year,cluster3,Engel,Region))
  
  save(EngleDD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleDD.rda"))
  save(EngleD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleD.rda"))
}

for(year in (Settings$startyear:Settings$endyear)){
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  

  #MD<-MD[,EngleH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  cat(paste0("\nYear:",year,"\t"))
  #nyear=year+1
  #if(Settings$endyear==year){nyear=year}
  #load(file=paste0(Settings$HEISProcessedPath,"Y",nyear,"EngleDD.rda"))
  #EngleND <- EngleDD
  #colnames(EngleND)[colnames(EngleND) == 'Engel'] <- 'Engeln'
  #colnames(EngleND)[colnames(EngleND) == 'Year'] <- 'Yearn'
  #nnyear=year+2
  #if(year >Settings$endyear-2){nnyear=year}
  #load(file=paste0(Settings$HEISProcessedPath,"Y",nnyear,"EngleDD.rda"))
  #EngleNND <- EngleDD
  #colnames(EngleNND)[colnames(EngleNND) == 'Engel'] <- 'Engelnn'
  #colnames(EngleNND)[colnames(EngleNND) == 'Year'] <- 'Yearnn'
  
  pyear=year-2
  load(file=paste0(Settings$HEISProcessedPath,"Y",pyear,"EngleDD.rda"))
  EnglePPD <- EngleDD
  colnames(EnglePPD)[colnames(EnglePPD) == 'Engel'] <- 'Engelpp'
  colnames(EnglePPD)[colnames(EnglePPD) == 'Year'] <- 'Yearpp'
  
  pyear=year-1
  load(file=paste0(Settings$HEISProcessedPath,"Y",pyear,"EngleDD.rda"))
  EnglePD <- EngleDD
  colnames(EnglePD)[colnames(EnglePD) == 'Engel'] <- 'Engelp'
  colnames(EnglePD)[colnames(EnglePD) == 'Year'] <- 'Yearp'
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleDD.rda"))
  EngleMD<-merge(EnglePPD,EnglePD)
  EngleMD<-merge(EngleMD,EngleDD)
  #  EngleMD<-merge(,EngleND)
  #  EngleMD<-merge(EngleMD,EngleNND)
  
  #EngleMD <- EngleMD[,Engelm:=(Engel+Engelp+Engelpp+Engelnn+Engeln)/5]  
  #if(year==97){  EngleMD <- EngleMD[,Engelm:=(Engel+Engelp+Engelpp)/3]  }
  #if(year==96){  EngleMD <- EngleMD[,Engelm:=(Engel+Engelp+Engelpp+Engeln)/4]  }
  
  #  EngleMD <- EngleMD[,Engelm:=Engel]
  
  
  
  EngleMD[,Engel:=     ifelse(Year==84,Engel*0.9937,
                              ifelse(Year==85,Engel*0.9938,
                              ifelse(Year==86,Engel*0.9936,
                              ifelse(Year==87,Engel*1.0012,
                              ifelse(Year==88,Engel*0.9963,
                              ifelse(Year==89,Engel*0.9898,
                              ifelse(Year==90,Engel*0.9664,
                              ifelse(Year==91,Engel*1.0269,
                              ifelse(Year==92,Engel*1.0250,
                              ifelse(Year==93,Engel*1.0080,
                              ifelse(Year==94,Engel*1.0006,
                              ifelse(Year==95,Engel*0.9996,
                              ifelse(Year==96,Engel*1.0000,
                              ifelse(Year==97,Engel*1.0379,0))))))))))))))]
  
  EngleMD[,Engelp:=    ifelse(Yearp==83,Engelp*1.0063,
                              ifelse(Yearp==84,Engelp*1.0125,
                              ifelse(Yearp==85,Engelp*1.0127,
                              ifelse(Yearp==86,Engelp*1.0053,
                              ifelse(Yearp==87,Engelp*1.0025,
                              ifelse(Yearp==88,Engelp*1.0141,
                              ifelse(Yearp==89,Engelp*1.0454,
                              ifelse(Yearp==90,Engelp*1.0076,
                              ifelse(Yearp==91,Engelp*0.95,
                              ifelse(Yearp==92,Engelp*0.9679,
                              ifelse(Yearp==93,Engelp*0.9915,
                              ifelse(Yearp==94,Engelp*0.9999,
                              ifelse(Yearp==95,Engelp*1.0004,
                              ifelse(Yearp==96,Engelp*0.9635,0))))))))))))))]
  

  EngleMD[,Engelpp:=   ifelse(Yearpp==82,Engelpp*1.0063,
                              ifelse(Yearpp==83,Engelpp*1.0125,
                              ifelse(Yearpp==84,Engelpp*1.0191,
                              ifelse(Yearpp==85,Engelpp*1.0115,
                              ifelse(Yearpp==86,Engelpp*1.009,
                              ifelse(Yearpp==87,Engelpp*1.0128,
                              ifelse(Yearpp==88,Engelpp*1.0493,
                              ifelse(Yearpp==89,Engelpp*1.018,
                              ifelse(Yearpp==90,Engelpp*0.983,
                              ifelse(Yearpp==91,Engelpp*0.9425,
                              ifelse(Yearpp==92,Engelpp*0.9673,
                              ifelse(Yearpp==93,Engelpp*0.9919,
                              ifelse(Yearpp==94,Engelpp*0.9999,
                              ifelse(Yearpp==95,Engelpp*0.9639,0))))))))))))))]
  
  EngleMD[,Engelm:=(Engel+Engelp+Engelpp)/3]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleD.rda"))
  EngleD  <- EngleD[,.(Region,cluster3,N,FPLine,Year)]
  
  EngleMD <- EngleMD[,EngleFinal:=Engelm]
  EngleDD <- EngleMD[,.(cluster3,EngleFinal,Engel,Region)]

  
  EngleD<-merge(EngleD,EngleDD,by=c("cluster3","Region"))
  
  #EngleD<-EngleD[,.(cluster3,Engel)]
  
  save(EngleD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS_Engle.rda"))

  
}




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
