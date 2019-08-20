# 25-Specification of food original groups.R

#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ FoodGroups =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Foods2.rda"))
  
  BigFData[,OriginalFoodExpenditure:=Expenditure]
  NfoodExp<-BigFData[,.(HHID,OriginalFoodExpenditure)]     
  NfoodExp <- NfoodExp[,lapply(.SD,sum),by=HHID]  
  FoodData<-merge(FoodData,NfoodExp,all.x = TRUE)
  FoodData[is.na(FoodData)] <- 0
  FoodData[,FoodOtherExpenditure:=FoodExpenditure-OriginalFoodExpenditure]     
  save(FoodData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Foods.rda"))
cat(FoodData[,mean(OriginalFoodExpenditure)])
  }

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")