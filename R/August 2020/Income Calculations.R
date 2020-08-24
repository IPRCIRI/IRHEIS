# 168- Step 8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  Calculations =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  #load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodCon.rda"))
  #load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"HomemadeWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"InterestWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"IntraWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"OtherIncome.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"RentWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  
  Data<-merge(MD[,.(HHID,Region,ProvinceCode,Size,EqSizeOECD,Weight,Total_Exp_Month_Per,Total_Exp_Month)],Deciles
              ,by="HHID",all.x = TRUE)
  Data<-merge(Data,PubWageData[,.(HHID,PubWageNetIncomeY)],by="HHID",all.x = TRUE)
  Data<-merge(Data,PrvWageData[,.(HHID,PrvWageNetIncomeY)],by="HHID",all.x = TRUE)
  Data<-merge(Data,BussIncomeData[,.(HHID,BussNetIncomeY)],by="HHID",all.x = TRUE)
  Data<-merge(Data,AgriIncomeData[,.(HHID,AgriNetIncomeY)],by="HHID",all.x = TRUE)
  Data<-merge(Data,AidWageData[,.(HHID,aid)],by="HHID",all.x = TRUE)
  Data<-merge(Data,HomemadeWageData[,.(HHID,homemade)],by="HHID",all.x = TRUE)
  Data<-merge(Data,InterestWageData[,.(HHID,interest)],by="HHID",all.x = TRUE)
  Data<-merge(Data,IntraWageData[,.(HHID,intra)],by="HHID",all.x = TRUE)
  Data<-merge(Data,RentWageData[,.(HHID,rent)],by="HHID",all.x = TRUE)
  Data<-merge(Data,RetirementWageData[,.(HHID,retirement)],by="HHID",all.x = TRUE)
  Data<-merge(Data,SubsidyWageData[,.(HHID,Subsidy)],by="HHID",all.x = TRUE)
  
  
  Data[is.na(Data)] <- 0
  Data[,Income_HH:=(PubWageNetIncomeY+PrvWageNetIncomeY+BussNetIncomeY+AgriNetIncomeY+
         aid+homemade+interest+intra+rent+retirement+Subsidy)/12]
  Data[,Income_Per:=Income_HH/EqSizeOECD]
  
 a1<- Data[,weighted.mean(Income_Per,Weight)]
 a1<- as.data.table(a1)
 b1<- Data[,weighted.mean(Income_Per,Weight),by=Region]
 c1<- Data[,weighted.mean(Income_Per,Weight),by=Decile][order(Decile)]
  
 a2<- Data[,weighted.mean(Total_Exp_Month_Per,Weight)]
 a2<- as.data.table(a2)
 b2<- Data[,weighted.mean(Total_Exp_Month_Per,Weight),by=Region]
 c2<- Data[,weighted.mean(Total_Exp_Month_Per,Weight),by=Decile][order(Decile)]
 
 cat(Data[,weighted.mean(Income_HH,Weight)],"\t")
 cat(Data[,weighted.mean(Total_Exp_Month,Weight)])
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")