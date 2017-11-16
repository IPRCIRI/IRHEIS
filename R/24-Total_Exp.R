# 28-Total_Exp.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Total =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Transportations.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Others.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Investments.rda"))


  MyData<-merge(HHBase,FoodData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,CigarData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,ClothData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,AmusementData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,CommunicationData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,DurableData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,EducData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,EnergyData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,FurnitureData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,HotelData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,HouseData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,MedicalData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,TransportationData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,OtherData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,InvestmentData,by =c("HHID"),all=TRUE)
  
  MyData<-MyData[Dimension!=0]
  MyData[is.na(MyData)] <- 0

  MyData[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=20:34][] 
  MyData$Total_Exp_Per_Month<-MyData$Total_Exp_Month/MyData$Dimension
  
  MyData$decile<-findInterval(MyData$Total_Exp_Per_Month, quantile(MyData$Total_Exp_Per_Month, probs=1:10/10), left.open=T)
  MyData$decile<- MyData$decile+1
  MyData$percentile<-findInterval(MyData$Total_Exp_Per_Month, quantile(MyData$Total_Exp_Per_Month, probs=1:100/100), left.open=T)
  MyData$percentile<- MyData$percentile+1
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  MyData<-merge(MyData,MyFood,by =c("HHID"),all=TRUE)
  
  #for (decile in 1:10) {
   # MyData$Average_Calories<-mean(MyData[,"Daily_Calories",by=list(decile)])
   # MyData$Average_Calories<MyData[, Average_Calories:=mean(Daily_Calories), by=decile]
 # }
 # tapply(MyData$Daily_Calories, MyData$decile, mean)
 aggregate( Daily_Calories ~ decile, MyData, mean )

  
  #save(MyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Exp.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)