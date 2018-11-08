#50-CBN Method.R
# 
# Copyright © 2018:Majid Einian- Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
 cat(paste0("\n------------------------------\nYear:",year,"\n"))
 
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"HHWeights",year,".rda"))
  HHWeights[,Year:=NULL]
  
  #load Expenditures
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Foods.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Cigars.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Cloths.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Amusements.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Communications.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Education.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Energy.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Furnitures.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Hotels.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Medicals.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Behdashts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Transportations.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Others.rda"))
  if(year %in% 78:95){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Investments.rda"))
  }
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Resturants.rda"))
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  
  #merge groups
  CBN<-merge(HHBase,HHWeights ,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FoodData,by =c("HHID"),all=TRUE)
  for (col in c("FoodExpenditure")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,CigarData,by =c("HHID"),all=TRUE)
  for (col in c("Cigar_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,ClothData,by =c("HHID"),all=TRUE)
  for (col in c("Cloth_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,AmusementData,by =c("HHID"),all=TRUE)
  for (col in c("Amusement_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,CommunicationData,by =c("HHID"),all=TRUE)
  for (col in c("Communication_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,EducData,by =c("HHID"),all=TRUE)
  for (col in c("EducExpenditure")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,EnergyData,by =c("HHID"),all=TRUE)
  for (col in c("Energy_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,FurnitureData,by =c("HHID"),all=TRUE)
  for (col in c("Furniture_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,HotelData,by =c("HHID"),all=TRUE)
  for (col in c("Hotel_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,BehdashtData,by =c("HHID"),all=TRUE)
  for (col in c("Behdasht_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,TransportationData,by =c("HHID"),all=TRUE)
  for (col in c("Transportation_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,OtherData,by =c("HHID"),all=TRUE)
  for (col in c("Other_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,HouseData,by =c("HHID"),all=TRUE)
  for (col in c("ServiceExp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,MedicalData,by =c("HHID"),all=TRUE)
  for (col in c("Medical_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,DurableData,by =c("HHID"),all=TRUE)
  for (col in c("Durable_Exp")) CBN[is.na(get(col)), (col) := 0]
 if(year %in% 78:95){
    CBN<-merge(CBN,InvestmentData,by =c("HHID"),all=TRUE)
    for (col in c("Investment_Exp")) CBN[is.na(get(col)), (col) := 0]
  }
  CBN<-merge(CBN,ResturantData,by =c("HHID"),all=TRUE)
  for (col in c("Resturant_Exp")) CBN[is.na(get(col)), (col) := 0]
  
  #Calculate Monthly Total Expenditures 
  if(year %in% 76:77){
    CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(23:35,39:40)][] 
    CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=23:35][] 
  }
  if(year %in% 78:95){
    CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(23:35,39:41)][] 
    CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=23:35][] 
  }
  #CBN[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeRevOECD]
  #CBN[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month/EqSizeRevOECD]

  #CBN<-CBN[Dimension!=0 & FoodExpenditure!=0]


  }