#Gini Coefficient.R
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(acid)

year<-89
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
 load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  
 m<-c(0:30)
 for(p in m){
   print(MD[ProvinceCode==p,weighted.gini(Total_Exp_Month_Per,Weight)])
}

#}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
