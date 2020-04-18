#171-Step 1.R
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
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
  HHBase<-merge(HHBase,lactating,by="HHID")
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  
  
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Furnitures","HotelRestaurants",
             "HouseandEnergys","House", "Medicals","Hygienes","Transportations","Others",
             "Resturants"
  )){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
    load(file = paste0(Settings$HEISProcessedPath,"Y",year,"NonFreeDurableData.rda"))
    
  }
  
  # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Added_Food.rda")) 
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  FData[,Region:=NULL]
  #for (col in c("FoodKCaloriesHH")) FData[is.na(get(col)), (col) := 0]
  FData <- FData[FoodKCaloriesHH>0]
  
  #merge groups
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  FData[,Size:=NULL]
  MD<-merge(MD,FData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HHWeights ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodData,by =c("HHID"),all=TRUE)
  # MD<-merge(MD,Added_Food,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseandEnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelRestaurantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HygieneData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BenzinData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,GazData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BarghData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,NaftSefidData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,NonFreeDurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "Education_Exp", "HouseandEnergy_Exp", 
                "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", "Transportation_Exp",
                "Other_Exp", "Medical_Exp", "NonFreeDurable_Exp",
                "Resturant_Exp","ServiceExp"
  )) 
    MD[is.na(get(col)), (col) := 0]
  #  MD<-MD[,Yaraneh:=416000*Size]
  
  MD<-merge(MD,Calorie_Need)
  
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durablele_Detail.rda"))
  #MD<-merge(MD,Durablele_Detail)
  

   
   cpi97 <- read_excel("C:/Users/pc1/Desktop/cpi97.xlsx")
   cpi97<-as.data.table(cpi97)
   cpi97<-cpi97[Month>0]
   cpi97<-cpi97[,Year:=NULL]
   cpi97<-cpi97[,PeriodYear:=NULL]
   cpi97<-cpi97[,.(Month,FoodDrink_Index,Cigar_Index,Cloth_Index,HouseEnergy_Index,
                   Furniture_Index,Hygiene_Index,Transportation_Index,
                   Communication_Index,Amusement_Index,Hotel_Index,
                   Other_Index,Durable_Index,Total_Index)]
   
   MD<-merge(MD,cpi97,by=c("Month"),all.x=TRUE)
   
   FoodDrink_Index98<-219.7
   Cigar_Index98<-281.1
   Cloth_Index98<-197.8
   HouseEnergy_Index98<-155.1
   Furniture_Index98<-225.1
   Hygiene_Index98<-158.1
   Transportation_Index98<-195.9
   Communication_Index98<-146.7
   Amusement_Index98<-224.5
   Hotel_Index98<-182.0
   Other_Index98<-199.5
   Durable_Index98<-259.2
   Total_Index98<-185.1
   
   #Calculate Monthly Total Expenditures 
   nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp",
           "Amusement_Exp", "Communication_Exp", 
           "HouseandEnergy_Exp", "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", 
           "Transportation_Exp", "Other_Exp" )
   #w <- c(nw, "Medical_Exp", "Remain_Durable")
   w <- c(nw, "Medical_Exp", "NonFreeDurable_Exp")
   #w <- c(nw, "Medical_Exp", "Durable_Pure_Exp")
   # pw <- c(nw, "Added_Food_Exp_Month")
   #Lw <- c(pw,  "Medical_Exp", "Durable_Exp")
   
   MD<-MD[,OriginalFoodExpenditure:=OriginalFoodExpenditure*FoodDrink_Index98/FoodDrink_Index]
   MD<-MD[,Cigar_Exp:=Cigar_Exp*Cigar_Index98/Cigar_Index]
   MD<-MD[,Cloth_Exp:=Cloth_Exp*Cloth_Index98/Cloth_Index]
   MD<-MD[,HouseandEnergy_Exp:=HouseandEnergy_Exp*HouseEnergy_Index98/HouseEnergy_Index]
   MD<-MD[,Furniture_Exp:=Furniture_Exp*Furniture_Index98/Furniture_Index]
   MD<-MD[,Hygiene_Exp:=Hygiene_Exp*Hygiene_Index98/Hygiene_Index]
   MD<-MD[,Medical_Exp:=Medical_Exp*Hygiene_Index98/Hygiene_Index]
   MD<-MD[,Transportation_Exp:=Transportation_Exp*Transportation_Index98/Transportation_Index]
   MD<-MD[,Communication_Exp:=Communication_Exp*Communication_Index98/Communication_Index]
   MD<-MD[,Amusement_Exp:=Amusement_Exp*Amusement_Index98/Amusement_Index]
   MD<-MD[,HotelRestaurant_Exp:=HotelRestaurant_Exp*Hotel_Index98/Hotel_Index]
   MD<-MD[,Other_Exp:=Other_Exp*Other_Index98/Other_Index]
   MD<-MD[,NonFreeDurable_Exp:=NonFreeDurable_Exp*Durable_Index98/Durable_Index]

   MD[is.na(MD)] <- 0
   
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  MD[,weighted.mean(Total_Exp_Month,Weight)]
  MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]

  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN198.rda"))
  
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)