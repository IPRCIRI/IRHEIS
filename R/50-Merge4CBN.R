#50-Merge Data for CBN Method.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
#library(sm)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights[,Year:=NULL]
  
  #load Expenditures
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Energy", "Furnitures","Hotels",
             "House", "Medicals","Behdashts","Transportations","Others",
             "Resturants")){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  }
  
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  FData[,Region:=NULL]
  #for (col in c("FoodKCalories")) FData[is.na(get(col)), (col) := 0]
  FData <- FData[FoodKCalories>0]
  
  #merge groups
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  FData[,Size:=NULL]
  MD<-merge(MD,FData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HHWeights ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,BehdashtData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,DurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "EducExpenditure", "Energy_Exp", 
                "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", "Transportation_Exp",
                "Other_Exp", "ServiceExp", "Medical_Exp", "Durable_Exp", 
                "Resturant_Exp")) 
    MD[is.na(get(col)), (col) := 0]
  
  
  #Calculate Monthly Total Expenditures 
  nw <- c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp",
          "Amusement_Exp", "Communication_Exp", "EducExpenditure",
          "Energy_Exp", "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", 
          "Transportation_Exp", "Other_Exp", "ServiceExp")
  w <- c(nw, "Medical_Exp", "Durable_Exp")
  
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeRevOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeRevOECD]
  
  #MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  MD<-MD[Size!=0 & FoodExpenditure!=0 & !is.na(FoodKCalories)]
  #MD[,Home_Per_Metr:=MetrPrice/EqSizeRevOECD]
  
  #Calculate Per Values
  MD[,EqSizeCalory :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  MD[,FoodExpenditure_Per :=FoodExpenditure/EqSizeCalory]
  MD[,FoodKCalories_Per:=FoodKCalories/EqSizeCalory]
  
  #Calculate per_Calory from resturants
  MD[,Calory_Price:=(FoodExpenditure_Per/FoodKCalories_Per)]
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)][order(Calory_Price)]
  MD[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
  for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
  MD[,TFoodKCalories:=FoodKCalories+ResturantKCalories]
  MD[,TFoodExpenditure:=FoodExpenditure+(Settings$OutFoodKCXShare*Resturant_Exp)]
  
  MD[,TFoodExpenditure_Per :=TFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCalories_Per:=TFoodKCalories/EqSizeCalory]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)