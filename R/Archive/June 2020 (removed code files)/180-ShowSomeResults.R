# 180-ShowSomeResults.R
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
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

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  #MD<-MD[Region=="Urban"]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  # MD[,FinalPoor2:=ifelse(Total_Exp_Month_Per < PovertyLine2,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
      # MD[,weighted.mean(PovertyLine2,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight)]
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)]
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region,cluster3)]
  
  #MD[,crw:=sum(Size*Weight),by=Region]
  #MD[,sum((Size*Weight)/crw),by=.(Region,Decile)][order(Region,Decile)]
  #MD[,weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(Region,Decile)][order(Region,Decile)]
  
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=c("ProvinceCode")][order(ProvinceCode)]
  
  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("ProvinceCode")][order(ProvinceCode)]
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")][order(Region,cluster3)]
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region")]
  MD[,weighted.mean(FinalPoor,Weight*Size)]
  MD3<-MD[,.(HHID,FinalPoor,Weight)]
  save(MD3,file=paste0(Settings$HEISProcessedPath,"Y",year,"PoorsforMerge.rda"))
}
  
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
