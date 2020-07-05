#163- Step 3.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Calculationg Per values =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))
  
 
  #MD[,Relative_Calorie1 :=FoodKCaloriesHH/Calorie_Need1]
  #MD[,Relative_Calorie2 :=FoodKCaloriesHH/Calorie_Need2]
  MD[,OriginalFoodExpenditure_Per :=OriginalFoodExpenditure/EqSizeCalory]
  MD[,FoodKCaloriesHH_Per:=FoodKCaloriesHH/EqSizeCalory]
  MD[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
  
  #Calculate per_Calory from resturants
  MD[,Calory_Price:=(OriginalFoodExpenditure_Per/FoodKCaloriesHH_Per)]
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)]
  MD[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
  for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
  MD[,TFoodKCaloriesHH:=FoodKCaloriesHH+ResturantKCalories]
  MD[,TOriginalFoodExpenditure:=OriginalFoodExpenditure+Resturant_Exp]
  
  MD[,TOriginalFoodExpenditure_Per :=TOriginalFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCaloriesHH_Per:=TFoodKCaloriesHH/EqSizeCalory]
  
  
cat(MD[,weighted.mean(TOriginalFoodExpenditure_Per,Weight)],"\n")
cat(MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE)],"\n")
cat(MD[,weighted.mean(Calory_Price,Weight,na.rm = TRUE)],"\n")
cat(MD[,weighted.mean(EqSizeCalory,Weight,na.rm = TRUE)],"\n")
  ##############################################################
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  

}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)