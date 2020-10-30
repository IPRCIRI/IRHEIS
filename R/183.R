#163- Step 3.R
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()


library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(spatstat)

year<-98
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2Ostan.rda"))
  
  
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
  
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3Ostan.rda"))


endtime <- proc.time()
cat("\n\n============================\nIt took ",(endtime-starttime)[3],"seconds.")