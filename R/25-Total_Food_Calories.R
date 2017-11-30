# 25-Total_Food_Calories.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Total Calories =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

#for(year in (Settings$startyear:Settings$endyear)){
  #cat(paste0("\n------------------------------\nYear:",year,"\n"))
  

  load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHBase.rda"))
  HHBase[,IndivNo:=NULL]
  HHBase[,Relationship:=NULL]
  HHBase[,Sex:=NULL]
  HHBase[,Age:=NULL]
  HHBase[,Literate:=NULL]
  HHBase[,Student:=NULL]
  HHBase[,EduCode:=NULL]
  HHBase[,EduYears:=NULL]
  HHBase[,EduLevel:=NULL]
  HHBase[,EduLevel0:=NULL]
  HHBase[,ActivityState:=NULL]
  HHBase[,MarritalState:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Ghands.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Hoboobats.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Roghans.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Berenjs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Nans.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Gooshts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Morghs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mahis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Shirs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Masts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Panirs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Tokhmemorghs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mives.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sabzis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Makaroonis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sibzaminis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Shirinis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Biscuits.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Khoshkbars.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Foods.rda"))

  MyFood<-merge(HHBase,GhandData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,HoboobatData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,RoghanData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,BerenjData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,NanData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,GooshtData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,MorghData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,MahiData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,ShirData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,MastData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,PanirData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,TokhmemorghData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,MiveData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,SabziData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,MakarooniData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,SibzaminiData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,ShiriniData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,KhoshkbarData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,BiscuitData,by =c("HHID"),all.x=TRUE)
  MyFood[,Grams:=NULL]
  MyFood[,Kilos:=NULL]
  MyFood<-merge(MyFood,FoodData,by =c("HHID"),all.x=TRUE)
  
 # MyFood<-MyFood[Region== 'Urban' | Region== 'Rural']
  
  MyFood$Ghand_Calory<- MyFood$GhandGram*4
  MyFood$Hoboobat_Calory<- MyFood$HoboobatGram*3
  MyFood$Nan_Calory<- MyFood$NanGram*2.5
  MyFood$Berenj_Calory<- MyFood$BerenjGram*1.2
  MyFood$Roghan_Calory<- MyFood$RoghanGram*8
  MyFood$Goosht_Calory<- MyFood$GooshtGram*2.5
  MyFood$Morgh_Calory<- MyFood$MorghGram*2
  MyFood$Mahi_Calory<- MyFood$MahiGram*1
  MyFood$Shir_Calory<- MyFood$ShirGram*2.5
  MyFood$Mast_Calory<- MyFood$MastGram*1.5
  MyFood$Panir_Calory<- MyFood$PanirGram*2.5
  MyFood$Tokhmemorgh_Calory<- MyFood$TokhmemorghGram*1.4
  MyFood$Mive_Calory<- MyFood$MiveGram*0.5
  MyFood$Sabzi_Calory<- MyFood$SabziGram*0.5
  MyFood$Makarooni_Calory<- MyFood$MakarooniGram*3.6
  MyFood$Sibzamini_Calory<- MyFood$SibzaminiGram*0.9
  MyFood$Shirini_Calory<- MyFood$ShiriniGram*3.5
  MyFood$Biscuit_Calory<- MyFood$BiscuitGram*3
  MyFood$Khoshkbar_Calory<- MyFood$KhoshkbarGram*5


  MyFood[,Year:=NULL]
  MyFood[,Quarter:=NULL]
  MyFood[,Month:=NULL]
  MyFood[,ProvinceCode:=NULL]
  MyFood[,Dimension:=NULL]
  MyFood[,FoodExpenditure:=NULL]
  
  MyFood[is.na(MyFood)] <- 0
  MyFood[, Daily_Calories := Reduce(`+`, .SD), .SDcols=22:40][] 
  
  MyFood <- MyFood[Daily_Calories<100000] # arbitrary removal of outliers
 # MyFoodRural<-MyFood[(MyFood$Region=="Rural"),]
 # MyFoodUrban<-MyFood[(MyFood$Region=="Urban"),]
  
  MyFoodRural<-MyFood[(MyFood$Region=="Rural"),]
  MyFoodUrban<-MyFood[(MyFood$Region=="Urban"),]
  MyFoodRural[,Region:=NULL]
  MyFoodUrban[,Region:=NULL]
 save(MyFoodRural, file = paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Rural.rda"))
 save(MyFoodUrban, file = paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Urban.rda"))
  
  
 # save(MyFood, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
 # save(MyFoodRural, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Rural.rda"))
 # save(MyFoodUrban, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Urban.rda"))
#}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)