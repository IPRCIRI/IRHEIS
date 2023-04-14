# 153-Specification of food original groups.R

#
# Copyright © 2019: Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(readxl)
#year <- 100
cat("\n\n================ FoodGroups =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFDataTotalNutrition.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  
  BigFData[,OriginalFoodExpenditure:=Expenditure]
  NfoodExp<-BigFData[,.(HHID,OriginalFoodExpenditure)]     
  NfoodExp <- NfoodExp[,lapply(.SD,sum),by=HHID]  
  FoodExpData<-merge(TotalFoodExpData,NfoodExp,all.x = TRUE)
  FoodExpData[is.na(FoodExpData)] <- 0
  FoodExpData[,FoodOtherExpenditure:=FoodExpenditure-OriginalFoodExpenditure]     
  save(FoodExpData, file = paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
}

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")
