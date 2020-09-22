#161-Step1-MergeData.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
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
  #  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  # HHWeights<- as.data.table(HHWeights)
  # HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  # HHWeights[,Year:=NULL]

  for(G in c("Cigars","Cloths","Amusements","Communications",
             "Durables", "Educations", "Furnitures","Hotels",
             "Energys","House", "Medicals","Hygienes","Transportations","Others",
             "Restaurants"
             ,"Durable_Detail","OwnedDurableItemsDepreciation"
  )){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  }
  
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
  MD<-merge(MD,Durable_Detail,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OwnedDurableItemsDepreciation,by =c("HHID"),all=TRUE)
  


  #Calculate Monthly Total Expenditures 
  nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp",
          "Cloth_Exp","Amusement_Exp", "Communication_Exp", 
          "House_Exp","Energy_Exp", "Furniture_Exp", "Hotel_Exp","Restaurant_Exp",
          "Hygiene_Exp", "Transportation_Exp", "Other_Exp",
          "Add_to_NonDurable" ,"OwnedDurableItemsDepreciation" )
  w <- c(nw, "Medical_Exp",
         "Durable_NoDep","Durable_Emergency")
  
  for (col in w)
    MD[is.na(get(col)), (col) := 0]
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  MD[,weighted.mean(Total_Exp_Month,Weight)]
  MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
