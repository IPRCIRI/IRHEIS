#161-Step1-MergeData.R
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  for(G in c("Cigars","Cloths","Amusements","Communications", 
             "Educations", "Furnitures","Hotels","Energys","House", "Medicals",
             "Hygienes","Transportations","Others", "Restaurants",
             "Durable_4Groups","OwnedDurableItemsDepreciation"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  
  FoodNutritionData <- FoodNutritionData[FoodKCaloriesHH>0]
  
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodNutritionData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodExpData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,RestaurantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HygieneData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Durable_4Groups,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OwnedDurableItemsDepreciation,by =c("HHID"),all=TRUE)
  
  #Calculate Monthly Total Expenditures 
  for (col in Settings$w)
    MD[is.na(get(col)), (col) := 0]
  
  MD[,Total_Exp_Month := Reduce(`+`, .SD), .SDcols=Settings$w]
  MD[,Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=Settings$nw]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)[3],"seconds.")
