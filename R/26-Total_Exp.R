# 26-Total_Exp.R
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
  MyData$Total_Exp_Month_Per<-MyData$Total_Exp_Month/MyData$Dimension
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  MyData<-merge(MyData,MyFood,by =c("HHID"),all=TRUE)
  MyData$Per_Daily_Calories<-MyData$Daily_Calories/MyData$Dimension
  
  MyDataRural<-MyData[(MyData$Region=="Rural"),]
  MyDataUrban<-MyData[(MyData$Region=="Urban"),]
  
  MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  MyDataRural$decile<- MyDataRural$decile+1
  MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  MyDataUrban$decile<- MyDataUrban$decile+1
  
  MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_decile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(decile)], by="decile")
  MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_decile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(decile)], by="decile")
  #MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")
  
  MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")
  MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")
  
#Calculate Calories for deciles-Rural
  d1<-subset(MyDataRural, decile==1)
  mean(d1$Per_Daily_Calories,na.rm = TRUE)
  
  d2<-subset(MyDataRural, decile==2)
  mean(d2$Per_Daily_Calories,na.rm = TRUE)
  
  d3<-subset(MyDataRural, decile==3)
  mean(d3$Per_Daily_Calories,na.rm = TRUE)
  
  d4<-subset(MyDataRural, decile==4)
  mean(d4$Per_Daily_Calories,na.rm = TRUE)
  
  d5<-subset(MyDataRural, decile==5)
  mean(d5$Per_Daily_Calories,na.rm = TRUE)
  
  d6<-subset(MyDataRural, decile==6)
  mean(d6$Per_Daily_Calories,na.rm = TRUE)
  
  d7<-subset(MyDataRural, decile==7)
  mean(d7$Per_Daily_Calories,na.rm = TRUE)
  
  d8<-subset(MyDataRural, decile==8)
  mean(d8$Per_Daily_Calories,na.rm = TRUE)
  
  d9<-subset(MyDataRural, decile==9)
  mean(d9$Per_Daily_Calories,na.rm = TRUE)
  
  d10<-subset(MyDataRural, decile==10)
  mean(d10$Per_Daily_Calories,na.rm = TRUE)
  
  #Calculate Calories for deciles-Urban
  d1<-subset(MyDataUrban, decile==1)
  mean(d1$Per_Daily_Calories,na.rm = TRUE)
  
  d2<-subset(MyDataUrban, decile==2)
  mean(d2$Per_Daily_Calories,na.rm = TRUE)
  
  d3<-subset(MyDataUrban, decile==3)
  mean(d3$Per_Daily_Calories,na.rm = TRUE)
  
  d4<-subset(MyDataUrban, decile==4)
  mean(d4$Per_Daily_Calories,na.rm = TRUE)
  
  d5<-subset(MyDataUrban, decile==5)
  mean(d5$Per_Daily_Calories,na.rm = TRUE)
  
  d6<-subset(MyDataUrban, decile==6)
  mean(d6$Per_Daily_Calories,na.rm = TRUE)
  
  d7<-subset(MyDataUrban, decile==7)
  mean(d7$Per_Daily_Calories,na.rm = TRUE)
  
  d8<-subset(MyDataUrban, decile==8)
  mean(d8$Per_Daily_Calories,na.rm = TRUE)
  
  d9<-subset(MyDataUrban, decile==9)
  mean(d9$Per_Daily_Calories,na.rm = TRUE)
  
  d10<-subset(MyDataUrban, decile==10)
  mean(d10$Per_Daily_Calories,na.rm = TRUE)
  
  #Calculate Calories for Provinces-Rural

  
  MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Expenditure_decile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(decile)], by="decile")
  MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Expenditure_decile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(decile)], by="decile")
  #MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")
  
  MyDataRural[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]
  MyDataUrban[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]
 # MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2300, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
  
  
  MyDataRural$Excess_Expenditure_decile <-(MyDataRural$Average_Calories_decile-2300)*(MyDataRural$Calory_price_decile)
  MyDataUrban$Excess_Expenditure_decile <-(MyDataUrban$Average_Calories_decile-2300)*(MyDataUrban$Calory_price_decile)
  #MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2300)*(MyData$Calory_price_percentile)
  
  MyDataRural$povertyline_decile <-(MyDataRural$Average_Expenditure_decile-MyDataRural$Excess_Expenditure_decile)
  MyDataUrban$povertyline_decile <-(MyDataUrban$Average_Expenditure_decile-MyDataUrban$Excess_Expenditure_decile)
  #MyData$povertyline_percentile <-(MyData$Average_Expenditure_percentil-MyData$Excess_Expenditure_percentile)
  
  PovertylineRural<-min(MyDataRural[,"povertyline_decile"], na.rm=TRUE)
  PovertylineUrban<-min(MyDataUrban[,"povertyline_decile"], na.rm=TRUE)
  
  PovertylineRural
  
  #Calculate Poor Index
  Rural_Pop<-sum(MyDataRural$Dimension)
  Urban_Pop<-sum(MyDataUrban$Dimension) 
  
  Rural_Poor<-MyDataRural[Per_Daily_Calories<2300]
  Urban_Poor<-MyDataUrban[Per_Daily_Calories<2300]
  
  Rural_Poor_Pop<-sum(Rural_Poor$Dimension)
  Urban_Poor_Pop<-sum(Urban_Poor$Dimension)
  
  Rural_Poor_Index<-Rural_Poor_Pop/Rural_Pop
  Urban_Poor_Index<-Urban_Poor_Pop/Urban_Pop
  ###
  
  aggregate(MyDataRural$Dimension, by=list(MyDataRural$Per_Daily_Calories < 2300), FUN=sum)
  aggregate(MyDataUrban$Dimension, by=list(MyDataUrban$Per_Daily_Calories < 2300), FUN=sum)
  
  #Total
  # MyData$decile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  #MyData$decile<- MyData$decile+1
  #MyData$percentile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:100/100), left.open=T)
  # MyData$percentile<- MyData$percentile+1
  
  # MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  # MyDataRural$decile<- MyDataRural$decile+1
  # MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  # MyDataUrban$decile<- MyDataUrban$decile+1

  # MyData <- merge(MyData, MyData[,.(Average_Calories_decile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(decile)], by="decile")
  # MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")

  # MyData <- merge(MyData, MyData[,.(Average_Expenditure_decile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(decile)], by="decile")
  # MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")

  # MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2300, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
  # MyData[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]

  # MyData$Excess_Expenditure_decile <-(MyData$Average_Calories_decile-2300)*(MyData$Calory_price_decile)
  # MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2300)*(MyData$Calory_price_percentile)
  
  # MyData$povertyline_decile <-(MyData$Average_Expenditure_decile-MyData$Excess_Expenditure_decile)
  # MyData$povertyline_percentile <-(MyData$Average_Expenditure_percentil-MyData$Excess_Expenditure_percentile)
  
  # MyData$Average_Calories<-MyData[,lapply(.SD,mean),by=decile]
  # MyData$Average_Calories<-MyData[,.(Average_Calories=mean(Per_Daily_Calories)),by=decile]
  # MyData[, .(Average_Calories = mean(Per_Daily_Calories) ), by = .(decile)]
  # MyData$Average_Calories<-mean(MyData[,"Per_Daily_Calories",by=.(decile)])
  # MyData$Average_Calories<-MyData[, Average_Calories:=mean(Daily_Calories), by=decile]
  # MyData[,.(Average_Calories=mean(Per_Daily_Calories)), by=decile]
  # tapply(MyData$Per_Daily_Calories, MyData$decile, mean)
  # aggregate( Per_Daily_Calories ~ percentile, MyData, mean )

  
  #save(MyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Exp.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)