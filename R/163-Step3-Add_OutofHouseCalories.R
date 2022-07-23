#163- Step 3.R
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))
  
 
  MD[,FoodExpenditure_Per :=FoodExpenditure/EqSizeCalory]
  MD[,OriginalFoodExpenditure_Per :=OriginalFoodExpenditure/EqSizeCalory]
  MD[,FoodKCaloriesHH_Per:=FoodKCaloriesHH/EqSizeCalory]
  MD[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
  
  #Calculate per_Calory from Restaurants
  MD[,Calory_Price:=(OriginalFoodExpenditure_Per/FoodKCaloriesHH_Per)]
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)]
  MD[,RestaurantKCalories:=(Settings$OutFoodKCXShare*Restaurant_Exp)/Calory_Price_Area]
  MD[is.na(RestaurantKCalories),RestaurantKCalories:=0]
  
  MD[,TFoodKCaloriesHH:=FoodKCaloriesHH+RestaurantKCalories]
  MD[,TOriginalFoodExpenditure:=OriginalFoodExpenditure+Restaurant_Exp]
  
  MD[,TOriginalFoodExpenditure_Per :=TOriginalFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCaloriesHH_Per:=TFoodKCaloriesHH/EqSizeCalory]
  
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ",(endtime-starttime)[3],"seconds.")