#Yarane report-Refah
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(tidyverse)

year<-98

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))


MD<-merge(MD,Deciles,all.x = TRUE)
MD[,Decile:=Decile1]
MD<-merge(MD,AidWageData,all.x = TRUE)
for (col in c("aid")) MD[is.na(get(col)), (col) := 0]
MD[,PositiveAid:=ifelse(aid>0,1,0)]
MD[,weighted.mean(PositiveAid,Weight*Size),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(PositiveAid,Weight*Size),by=.(Decile)][order(Decile)]


load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
MD<-merge(MD,SubsidyWageData,all.x = TRUE,by="HHID")
for (col in c("Subsidy")) MD[is.na(get(col)), (col) := 0]
MD[,PositiveSubsidy:=ifelse(Subsidy>0,1,0)]
MD[,weighted.mean(PositiveSubsidy,Weight*Size),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(PositiveSubsidy,Weight*Size),by=.(Decile)][order(Decile)]


#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"UTSubsidyW.rda"))
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"RTSubsidyW.rda"))

#TSubsidy<-rbind(UTSubsidyW,RTSubsidyW)
#TSubsidy<-TSubsidy[,HHID:=Address]
#MD<-merge(MD,TSubsidy[,.(HHID,check1)],all.x = TRUE,by="HHID")
#MD[,Subsidy3:=ifelse(check1>800000,1,0)]
#y<-MD[Subsidy3==1,.(HHID,Decile,Region)]
#MD[,weighted.mean(Subsidy3,Weight*Size,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
#MD[,weighted.mean(Subsidy3,Weight*Size,na.rm = TRUE),by=.(Decile)][order(Decile)]

MD[,TotalAid:=(Subsidy+aid)/12]
MD_Ok<- MD[TotalAid>0]
MD_Ok[,ratio:=TotalAid/Total_Exp_Month]
MD_Ok[,.(.N,weighted.mean(ratio,Weight*Size,na.rm = TRUE)),by=.(Region,Decile)][order(Region,Decile)]
MD_Ok[,.(weighted.mean(ratio,Weight*Size,na.rm = TRUE)),by=.(Decile)][order(Decile)]
MD_Ok[,.(.N,weighted.mean(Total_Exp_Month,Weight*Size,na.rm = TRUE)),by=.(Region,Decile)][order(Region,Decile)]
MD_Ok[,.(weighted.mean(Total_Exp_Month,Weight*Size,na.rm = TRUE)),by=.(Decile)][order(Decile)]
MD_Ok[,.(.N,weighted.mean(Total_Exp_Month/EqSizeOECD,Weight*Size,na.rm = TRUE)),by=.(Region,Decile)][order(Region,Decile)]
MD_Ok[,.(weighted.mean(Total_Exp_Month/EqSizeOECD,Weight*Size,na.rm = TRUE)),by=.(Decile)][order(Decile)]

#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BreadExp.rda"))
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BreadCon.rda"))
#MD<-merge(MD,BreadData,by="HHID")
#MD<-merge(MD,BreadConsumption,by="HHID")

MD[is.na(MD)] <- 0

#MD[,weighted.mean(G01114+G01115,Weight),by=.(Region,Decile)][order(Region,Decile)]
#MD[,weighted.mean(G01114+G01115,Weight),by=.(Decile)][order(Decile)]

#MD[,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]
#MD[,weighted.mean(Size,Weight),by=.(Region,Decile)][order(Region,Decile)]

#MD[,weighted.mean(BreadGrams,Weight),by=.(Region,Decile)][order(Region,Decile)]
#MD[,weighted.mean(BreadGrams,Weight),by=.(Decile)][order(Decile)]

#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"DrugsExp.rda"))
#MD<-merge(MD,DrugsExp,all.x = TRUE)
#MD[is.na(MD)] <- 0

#MD[,weighted.mean(DrugsExp,Weight),by=.(Region,Decile)][order(Region,Decile)]
#MD[,weighted.mean(DrugsExp,Weight),by=.(Decile)][order(Decile)]

load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
TotalFoodExp<-merge(TotalFoodExp,MD[,.(HHID,Decile)])

TotalFoodExp[,weighted.mean(`011211`+`011212`+`011213`+
                              `011214`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011117`+`011118`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011231`+`011232`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011411`+`011412`+`011413`+`011414`+
                              `011421`+`011422`+`011423`+`011424`+
                              `011425`+`011426`+`011427`+`011428`+
                              `011429`+`011431`+`011432`+`011433`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011441`+`011442`+`011443`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011531`+`011532`+`011533`,Weight),by=.(Decile)][order(Decile)]

TotalFoodExp[,weighted.mean(`011812`,Weight),by=.(Decile)][order(Decile)]


load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodCon.rda"))
MD<-merge(MD,TotalFoodCon,by="HHID")
MD<-MD[as.numeric(Decile)>=1 & as.numeric(Decile)<=10]
A<-MD[,weighted.mean((Rice_Khareji1Gram+Rice_Khareji2Gram)/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean(Oil_NabatiGram/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean(PoultryMeat_MGram/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean(CowMeatGram/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean(SheepMeatGram/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean((Egg_MashinGram+Egg_NonMashinGram)/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean((BreadGrams)/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean((GrainGrams)/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean((MilkGrams+MilkproductsGrams)/EqSizeCalory,Weight),by=Decile][order(Decile)]
A<-MD[,weighted.mean((TreeFruitsGrams+JaliziFruitsGrams)/EqSizeCalory,Weight),by=Decile][order(Decile)]

MD[as.numeric(Decile)<4,weighted.mean(Total_Exp_Month_Per,Weight)]
MD[as.numeric(Decile)>7,weighted.mean(Total_Exp_Month_Per,Weight)]


