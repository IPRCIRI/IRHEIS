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

year<-96
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file="Inflation9098.rda")
  
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
  MD<-merge(MD,Inflation9098,by="ProvinceCode")
  
  MD[,FoodExpenditure:=FoodExpenditure*y9697*y9798]
  MD[,OriginalFoodExpenditure:=OriginalFoodExpenditure*y9697*y9798]
  MD[,FoodOtherExpenditure:=FoodOtherExpenditure*y9697*y9798]
  MD[,Cigar_Exp:=Cigar_Exp*y9697*y9798]
  MD[,Cloth_Exp:=Cloth_Exp*y9697*y9798]
  MD[,Amusement_Exp:=Amusement_Exp*y9697*y9798]
  MD[,Communication_Exp:=Communication_Exp*y9697*y9798]
  MD[,Education_Exp:=Education_Exp*y9697*y9798]
  MD[,Energy_Exp:=Energy_Exp*y9697*y9798]
  MD[,House_Exp:=House_Exp*y9697*y9798]
  MD[,MeterPrice:=MeterPrice*y9697*y9798]
  MD[,Furniture_Exp:=Furniture_Exp*y9697*y9798]
  MD[,Hotel_Exp:=Hotel_Exp*y9697*y9798]
  MD[,Restaurant_Exp:=Restaurant_Exp*y9697*y9798]
  MD[,Hygiene_Exp:=Hygiene_Exp*y9697*y9798]
  MD[,Transportation_Exp:=Transportation_Exp*y9697*y9798]
  MD[,Other_Exp:=Other_Exp*y9697*y9798]
  MD[,Medical_Exp:=Medical_Exp*y9697*y9798]
  MD[,Add_to_NonDurable:=Add_to_NonDurable*y9697*y9798]
  MD[,Durable_Dep:=Durable_Dep*y9697*y9798]
  MD[,Durable_NoDep:=Durable_NoDep*y9697*y9798]
  MD[,Durable_Emergency:=Durable_Emergency*y9697*y9798]
  MD[,OwnedDurableItemsDepreciation:=OwnedDurableItemsDepreciation*y9697*y9798]
  
  #Calculate Monthly Total Expenditures 
  for (col in Settings$w)
    MD[is.na(get(col)), (col) := 0]
  
  MD[,Total_Exp_Month := Reduce(`+`, .SD), .SDcols=Settings$w]
  MD[,Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=Settings$nw]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  MD96<-copy(MD)
  
############################################################
  ########################################################
  ########################################################
  
  year<-97
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file="Inflation9098.rda")
  
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
  MD<-merge(MD,Inflation9098,by="ProvinceCode")
  
  MD[,FoodExpenditure:=FoodExpenditure*y9798]
  MD[,OriginalFoodExpenditure:=OriginalFoodExpenditure*y9798]
  MD[,FoodOtherExpenditure:=FoodOtherExpenditure*y9798]
  MD[,Cigar_Exp:=Cigar_Exp*y9798]
  MD[,Cloth_Exp:=Cloth_Exp*y9798]
  MD[,Amusement_Exp:=Amusement_Exp*y9798]
  MD[,Communication_Exp:=Communication_Exp*y9798]
  MD[,Education_Exp:=Education_Exp*y9798]
  MD[,Energy_Exp:=Energy_Exp*y9798]
  MD[,House_Exp:=House_Exp*y9798]
  MD[,MeterPrice:=MeterPrice*y9798]
  MD[,Furniture_Exp:=Furniture_Exp*y9798]
  MD[,Hotel_Exp:=Hotel_Exp*y9798]
  MD[,Restaurant_Exp:=Restaurant_Exp*y9798]
  MD[,Hygiene_Exp:=Hygiene_Exp*y9798]
  MD[,Transportation_Exp:=Transportation_Exp*y9798]
  MD[,Other_Exp:=Other_Exp*y9798]
  MD[,Medical_Exp:=Medical_Exp*y9798]
  MD[,Add_to_NonDurable:=Add_to_NonDurable*y9798]
  MD[,Durable_Dep:=Durable_Dep*y9798]
  MD[,Durable_NoDep:=Durable_NoDep*y9798]
  MD[,Durable_Emergency:=Durable_Emergency*y9798]
  MD[,OwnedDurableItemsDepreciation:=OwnedDurableItemsDepreciation*y9798]
  
  #Calculate Monthly Total Expenditures 
  for (col in Settings$w)
    MD[is.na(get(col)), (col) := 0]
  
  MD[,Total_Exp_Month := Reduce(`+`, .SD), .SDcols=Settings$w]
  MD[,Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=Settings$nw]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  MD97<-copy(MD)
  
  ############################################################
  ########################################################
  ########################################################
  
  year<-98
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodNutritionData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodExpData.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file="Inflation9098.rda")
  
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
  MD<-merge(MD,Inflation9098,by="ProvinceCode")
  

  #Calculate Monthly Total Expenditures 
  for (col in Settings$w)
    MD[is.na(get(col)), (col) := 0]
  
  MD[,Total_Exp_Month := Reduce(`+`, .SD), .SDcols=Settings$w]
  MD[,Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=Settings$nw]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  MD98<-copy(MD)
  
  #####################################################
  rm(MD)
  
  MD<-rbind(MD96,MD97)
  MD<-rbind(MD,MD98)
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBNOstan.rda"))
  

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)[3],"seconds.")
