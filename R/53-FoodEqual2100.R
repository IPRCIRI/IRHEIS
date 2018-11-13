#53-FoodEqual2100.R
# 
# Copyright © 2018:Majid Einian- Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))

  #Food Equal 2100 KCal
  MDPoors<-MD[InitialPoor==1]
  MD<-MD[,FPLine:=weighted.mean(Bundle_Value[InitialPoor==1],Weight[InitialPoor==1],na.rm = TRUE),by=.(cluster,Region)]
  x<-MD[,.(cluster,Region,FPLine,InitialPoor)]
  MD[,InitialPoor:=ifelse(TFoodExpenditure_Per < FPLine,1,0 ),by=.(cluster,Region)]
  
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")