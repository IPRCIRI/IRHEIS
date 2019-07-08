# Budget 2100
# Builds the Food Groups data.table for households
#
# Copyright © 2018: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ FoodGroups =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
 
 Base<-merge(BigFData,MD[,.(HHID,Region,NewArea,NewArea2,ProvinceCode,cluster3,Decile,
                            FinalPoor,Weight,Size,EqSizeRevOECD,EqSizeCalory)],by="HHID")

 Base<-Base[,FGrams_Per:=FGrams/EqSizeCalory]
             
 Base[FoodType=="Khoshkbar" ,
      weighted.mean(FGrams_Per,Weight),by=.(FinalPoor,ProvinceCode)]
    }

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")