# 28-Total_Food_Calories.R
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

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Ghands.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Hoboobats.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Roghans.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Berenjs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Nans.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Gooshts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Morghs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Mahis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Shirs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Masts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Panirs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Mives.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Sabzis.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Foods.rda"))
  
  MyFood<-merge(HHBase,GhandData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,HoboobatData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,RoghanData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,BerenjData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,NanData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,GooshtData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,MorghData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,MahiData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,ShirData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,MastData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,PanirData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,TokhmemorghData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,MiveData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,SabziData,by =c("HHID"),all=TRUE)
  MyFood<-merge(MyFood,FoodData,by =c("HHID"),all=TRUE)
  
  save(MyFood, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Food.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)