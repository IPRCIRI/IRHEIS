# 29-Total.R
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
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total_Exp.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  
  TotalData<-merge(MyData,MyIncome,by =c("HHID"),all=TRUE)
  
  save(TotalData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)