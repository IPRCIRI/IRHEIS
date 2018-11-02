# 24-Total_Food_Calories.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Total Calories =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  FData <- BigFData[,.(FoodKCalories=sum(FoodKCalories)),by=HHID]
  FData <- merge(HHBase[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,Dimension)],
                 FData,by = "HHID",all.x = TRUE)
  
  FData <- FData[FoodKCalories<100000] # arbitrary removal of outliers
 
  FDataRural<-FData[(FData$Region=="Rural"),]
  FDataUrban<-FData[(FData$Region=="Urban"),]
  FDataRural[,Region:=NULL]
  FDataUrban[,Region:=NULL]
  save(FData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  save(FDataRural, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Rural.rda"))
  save(FDataUrban, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Urban.rda"))
}
  
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)