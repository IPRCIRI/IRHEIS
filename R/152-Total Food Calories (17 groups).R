# 152-Total_Food_Calories.R
# 
# Copyright © 2017-2020:Arin Shahbazian, Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Total Calories =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  FData <- BigFData[,.(FoodKCaloriesHH=sum(FoodKCalories),
                       FoodProteinHH=sum(FoodProtein)),by=HHID]
  
  FData <- FData[FoodKCaloriesHH<100000] # arbitrary removal of outliers 
  # TODO: remove households that had some event (religious, weddings, ...) instead of this arbitrary removal
  
  save(FData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
}

cat("\n\n============================\nIt took ")

endTime <- proc.time()
cat((endTime-startTime)[3],"seconds.")