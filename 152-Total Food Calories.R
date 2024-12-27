# 152-Total_Food_Calories.R
# 
# Copyright © 2017-2020:Arin Shahbazian, Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md
# 
rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Total Calories =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
#year <- 100
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFDataTotalNutrition.rda"))
  FoodNutritionData <- BigFData[,.(FoodKCaloriesHH=sum(FoodKCalories),
                       FoodProteinHH=sum(FoodProtein),
                       FoodVitaminAHH=sum(FoodVitaminA),
                       FoodRiboflavinHH=sum(FoodRiboflavin),
                       FoodFeHH=sum(FoodFe),
                       FoodCalciumHH=sum(FoodCalcium)),by=HHID]
  
  
  FoodNutritionData <- FoodNutritionData[FoodKCaloriesHH<100000] # arbitrary removal of outliers 
  # TODO: remove households that had some event (religious, weddings, ...) instead of this arbitrary removal
  
  save(FoodNutritionData, file = paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
}
endTime <- proc.time()
cat("\n\n============================\nIt took ",(endTime-startTime)[3],"seconds.")
