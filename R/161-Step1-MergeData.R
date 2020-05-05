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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  Value<-merge(Value,HHHouseProperties)
  Value<-merge(Value,HHWeights)
  
  Value[,Auto2_rani:=0.1*Auto2_rani]
  Value[,Auto1_Khareji:=0.1*Auto1_Khareji]
  Value[,Auto2_Khareji:=0.1*Auto2_Khareji]
  Value[,Auto1_Irani:=0.1*Auto1_Irani]
  Value[,TV_Rangi_Irani:=0.033*TV_Rangi_Irani]
  Value[,TV_Rangi_Khareji:=0.033*TV_Rangi_Khareji]
  Value[,freezer2:=0.033*freezer2]
  Value[,OjaghGaz:=0.033*OjaghGaz]
  Value[,Mashin_Lebasshooyi:=0.033*Mashin_Lebasshooyi]
  Value[,Mobile:=0.11*Mobile]
  Value[,Cooler_Gaz:=0.05*Cooler_Gaz]
  Value[,PC:=0.06*PC]
  Value[,Lastik_Mashin:=0.5*Lastik_Mashin]
  Value[,Motor_Machin:=0.033*Motor_Machin]
  Value[,Tamirat_Asasi:=0.05*Tamirat_Asasi]
  
  A1<- Value[Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani>0,
        weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+
                                            Auto1_Irani,Weight)]
  A2<-Value[TV_Rangi_Irani+TV_Rangi_Khareji>0,weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]
  A3<-Value[freezer2>0,weighted.mean(freezer2,Weight)]
  A4<-Value[OjaghGaz>0,weighted.mean(OjaghGaz,Weight)]
  A5<-Value[Mashin_Lebasshooyi>0,weighted.mean(Mashin_Lebasshooyi,Weight)]
  A6<-Value[Mobile>0,weighted.mean(Mobile,Weight)]
  A7<-Value[Cooler_Gaz>0,weighted.mean(Cooler_Gaz,Weight)]
  A8<-Value[PC>0,weighted.mean(PC,Weight)]
  A9<-Value[Lastik_Mashin>0,weighted.mean(Lastik_Mashin,Weight)]
  A10<-Value[Motor_Machin>0,weighted.mean(Motor_Machin,Weight)]
  A11<-Value[Tamirat_Asasi>0,weighted.mean(Tamirat_Asasi,Weight)]
  
  Value[car=="True",Added1:=A1]
  Value[tvcr=="True",Added2:=A2]
  Value[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
        Added3:=A3]
  Value[oven=="True",Added4:=A4]
  Value[washer=="True",Added5:=A5]
  Value[cellphone=="True",Added6:=A6]
  Value[cooler_gas=="True",Added7:=A7]
  Value[computer=="True",Added8:=A8]
  Value[car=="True",Added9:=A9]
  Value[car=="True",Added10:=A10]
  Value[car=="True",Added11:=A11]
  
  


  
  Value[,Weight:=NULL]
  dep <- c( "Auto2_rani", "Auto1_Khareji","Auto2_Khareji", "Auto1_Irani",
           "TV_Rangi_Irani", "TV_Rangi_Khareji","freezer2", "OjaghGaz",
           "Mashin_Lebasshooyi", "Mobile","Cooler_Gaz", "PC",
           "Lastik_Mashin", "Motor_Machin","Tamirat_Asasi")
  
  Value[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  Value[is.na(Value)] <- 0
  
  Value[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
          Added7+Added8+Added9+Added10+Added11]
  
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Furnitures","HotelRestaurants",
             "HouseandEnergys","House", "Medicals","Hygienes","Transportations","Others",
             "Resturants"
             ,"Durablele_Detail"
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
  MD<-merge(MD,Durablele_Detail,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Value,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "Education_Exp", "HouseandEnergy_Exp", 
                "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", "Transportation_Exp",
                "Other_Exp", "Medical_Exp", "NonFreeDurable_Exp",
                "Resturant_Exp","ServiceExp"
                ,"Add_to_NonDurable","Durable_Dep",
                "Durable_NoDep","Durable_Emergency","Total_Depreciated_Durable"
  )) 
    MD[is.na(get(col)), (col) := 0]
  #  MD<-MD[,Yaraneh:=416000*Size]
  
  MD<-merge(MD,Calorie_Need)
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durablele_Detail.rda"))
  #MD<-merge(MD,Durablele_Detail)
  
  #Calculate Monthly Total Expenditures 
  nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp",
          "Amusement_Exp", "Communication_Exp", 
          "HouseandEnergy_Exp", "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", 
          "Transportation_Exp", "Other_Exp"
          ,"Add_to_NonDurable"
          #,"Added"
         #, "Total_Depreciated_Durable"
  )
  w <- c(nw, "Medical_Exp",
         "Durable_NoDep","Durable_Emergency")
 # w <- c(nw, "Medical_Exp", "NonFreeDurable_Exp")
  #w <- c(nw, "Medical_Exp", "Durable_Pure_Exp")
  # pw <- c(nw, "Added_Food_Exp_Month")
  #Lw <- c(pw,  "Medical_Exp", "Durable_Exp")
  
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  MD[,weighted.mean(Total_Exp_Month,Weight)]
  MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)