#170-Deciling for normal calculations.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Deciling for normal calculations =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  
  ###Nominal- Country
  MD<- MD[order(Total_Exp_Month_Per_nondurable),by=ProvinceCode]  #Deciling in Country(Nominal)
  MD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size),by=ProvinceCode]  # Cumulative Relative Weight
  MD[,Decile:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
  MD[,Percentile:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]
  
  Decile_Nominal<-MD[,.(HHID,Decile)]
  
save(Decile_Nominal,file = paste0(Settings$HEISProcessedPath,"Y",year,"Decile_Nominal.rda"))
  
}

MD[,Pop2:=sum(Weight*Size),by=ProvinceName]

Pooldar<-MD[as.numeric((Decile))>7]
Pooldar[,sum(Weight*Size),by=ProvinceName][order(V1)]
Pooldar[,Pop:=sum(Weight*Size)]
a<-Pooldar[,.(sum(Weight*Size)/Pop),by=ProvinceName]
a<-Pooldar[,.(sum(Weight*Size)/Pop2),by=ProvinceName]
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))

Poor<-MD[as.numeric((Decile))<4]
Poor[,sum(Weight*Size),by=ProvinceName][order(V1)]
Poor[,Pop:=sum(Weight*Size)]
a<-Poor[,.(sum(Weight*Size)/Pop),by=ProvinceName]
a<-Poor[,.(sum(Weight*Size)/Pop2),by=ProvinceName]

Pooldar[CountyCode==2301,weighted.mean(Total_Exp_Month_Per,Weight)]
Pooldar[ProvinceCode==11,weighted.mean(Total_Exp_Month_Per,Weight)]

Poor[CountyCode==2301,weighted.mean(Total_Exp_Month_Per,Weight)]
Poor[ProvinceCode==11,weighted.mean(Total_Exp_Month_Per,Weight)]


Pooldar[CountyCode==2301,weighted.mean(FoodExpenditure_Per,Weight)]
Pooldar[ProvinceCode==11,weighted.mean(FoodExpenditure_Per,Weight)]

Poor[CountyCode==2301,weighted.mean(FoodExpenditure_Per,Weight)]
Poor[ProvinceCode==11,weighted.mean(FoodExpenditure_Per,Weight)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
