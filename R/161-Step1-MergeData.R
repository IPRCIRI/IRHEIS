#161-Step 1.R
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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Esghat_Value.rda"))
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  Value<-merge(Value,HHHouseProperties)
  Value<-merge(Value,HHWeights)
  Value[,Year:=year]
  Value<-merge(Value,Esghat_Value,by="Year")
  Value[,Year:=NULL]
  
  Value[,`71111`:=0.1*`71111`]
  Value[,`71112`:=0.1*`71112`]
  Value[,`71116`:=0.1*`71116`]
  Value[,`71117`:=0.1*`71117`]
  Value[,`91128`:=0.033*`91128`]
  Value[,`91129`:=0.033*`91129`]
  Value[,`53112`:=0.033*`53112`]
  Value[,`53116`:=0.033*`53116`]
  Value[,`53113`:=0.033*`53113`]
  Value[,`82113`:=0.11*`82113`]
  Value[,`53125`:=0.05*`53125`]
  Value[,`91311`:=0.06*`91311`]
  Value[,`72111`:=0.5*`72111`]
  if (year!=92){
  Value[,`72118`:=0.033*`72118`]
  }
  Value[,`72319`:=0.05*`72319`]
  
  A1<- Value[`71111`+`71112`+`71116`+`71117`>0,
        weighted.mean(`71111`+`71112`+`71116`+
                                            `71117`,Weight)]
  A2<-Value[`91128`+`91129`>0,weighted.mean(`91128`+`91129`,Weight)]
  A3<-Value[`53112`>0,weighted.mean(`53112`,Weight)]
  A4<-Value[`53116`>0,weighted.mean(`53116`,Weight)]
  A5<-Value[`53113`>0,weighted.mean(`53113`,Weight)]
  A6<-Value[`82113`>0,weighted.mean(`82113`,Weight)]
  A7<-Value[`53125`>0,weighted.mean(`53125`,Weight)]
  A8<-Value[`91311`>0,weighted.mean(`91311`,Weight)]
  A9<-Value[`72111`>0,weighted.mean(`72111`,Weight)]
  if (year!=92){
  A10<-Value[`72118`>0,weighted.mean(`72118`,Weight)]
  }
  A11<-Value[`72319`>0,weighted.mean(`72319`,Weight)]
  
  Value[car=="True",Added1:=A1-0.05*Auto_Sale] ### We use 0.05 instead of 0.1
  Value[tvcr=="True",Added2:=A2-0.033*TV_Sale]
  Value[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
        Added3:=A3-0.033*yakhchal_Sale]
  Value[oven=="True",Added4:=A4-0.033*ojaghgaz_Sale]
  Value[washer=="True",Added5:=A5-0.033*lebasshooyi_Sale]
  Value[cellphone=="True",Added6:=A6-0.11*Mobile_Sale]
  Value[cooler_gas=="True",Added7:=A7-0.05*Coolergazi_Sale]
  Value[computer=="True",Added8:=A8-0.06*PC_Sale]
  Value[car=="True",Added9:=A9-0.5*lastik_Sale]
  if (year!=92){
  Value[car=="True",Added10:=A10]
  }
  Value[car=="True",Added11:=A11]
  
  


  
  Value[,Weight:=NULL]
  if (year!=92){
  dep <- c( "71111", "71112","71116", "71117",
           "91128", "91129","53112", "53116",
           "53113", "82113","53125", "91311",
           "72111", "72118","72319")
  }
  if (year==92){
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111","72319")
  }
  
  Value[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  Value[is.na(Value)] <- 0
  
  if (year!=92){
  Value[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
          Added7+Added8+Added9+Added10+Added11]
  }
  if (year==92){
  Value[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
          Added7+Added8+Added9+Added11]
  }
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Furnitures","HotelRestaurants",
             "HouseandEnergys","House", "Medicals","Hygienes","Transportations","Others",
             "Resturants"
             ,"Durablele_Detail"
  )){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))

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
 # MD<-merge(MD,NonFreeDurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Durablele_Detail,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Value,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "Education_Exp", "HouseandEnergy_Exp", 
                "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", "Transportation_Exp",
                "Other_Exp", "Medical_Exp", 
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
  
  cat(MD[,weighted.mean(Added1,Weight,na.rm = TRUE)],"\n")
  cat(MD[,weighted.mean(Hygiene_Exp,Weight,na.rm = TRUE)],"\n")
  cat(MD[,weighted.mean(Medical_Exp,Weight,na.rm = TRUE)],"\n")
  cat(MD[,weighted.mean(Total_Exp_Month_nondurable,Weight,na.rm = TRUE)],"\n")

}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)