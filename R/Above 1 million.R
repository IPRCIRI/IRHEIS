# 168-Step8-PovertyStats.R
# 
# Copyright © 2018-2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)


year<-98
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))

  MD[,Above:=ifelse((Total_Exp_Month-House_Exp)/Size>10000000,1,0)]
  MD[,weighted.mean(Above,Weight)]
  MD[,weighted.mean(Above,Weight),by=Region]
  MD[,weighted.mean(Above,Weight),by=ProvinceCode][order(ProvinceCode)]
  
  MD[Above==0,weighted.mean((Total_Exp_Month-House_Exp)/Size,Weight)]
  MD[Above==0,weighted.mean((Total_Exp_Month-House_Exp)/Size,Weight),by=Region]
  MD[Above==0,weighted.mean((Total_Exp_Month-House_Exp)/Size,Weight),by=ProvinceCode][order(ProvinceCode)]
  



endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")