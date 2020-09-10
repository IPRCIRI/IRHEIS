# High Engle.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ High Engle =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  #cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
 
  MD<-MD[,High_Engle:=ifelse(HHEngle>0.5,1,0)]
  MD[,weighted.mean(High_Engle,Weight)]
  MD[High_Engle==1,.(sum(HIndivNo),sum(FinalPoor)),by=.(Region,NewArea_Name)][order(V1)]
  MD[,weighted.mean(High_Engle,Weight),by=.(Region,NewArea_Name)][order(V1)]
  MD[High_Engle==1,sum(HIndivNo),by=.(Region,NewArea_Name)][order(V1)]
  MD[,sum(HIndivNo),by=.(Region,NewArea_Name)][order(Region)]
  
  HighEngle<-MD[HHEngle>0.5]
  HighEngle[,weighted.mean(FinalPoor,Weight)]
  HighEngle[,weighted.mean(FinalPoor,Weight),by=.(Region)][order(V1)]
  
  HighEngle[,weighted.mean(FinalPoor,Weight),by=.(Region,NewArea_Name)][order(V1)]
  
  HighEngle[,sum(HIndivNo),by=.(ProvinceCode)][order(V1)]
  HighEngle[,weighted.median(Total_Exp_Month_Per_nondurable,Weight),by=.(ProvinceCode)][order(V1)]
  HighEngle[,weighted.median(TOriginalFoodExpenditure,Weight),by=.(ProvinceCode)][order(V1)]
  
  HighEngle[,weighted.median(Total_Exp_Month_nondurable-TOriginalFoodExpenditure,Weight),by=.(ProvinceCode)][order(V1)]

  HighEngle[Region=="Rural" & NewArea_Name=="Sistan",sum(HIndivNo),by=.(Decile)][order(Decile)]
  HighEngle[Region=="Rural" & NewArea_Name=="Sistan",weighted.mean(TOriginalFoodExpenditure_Per,Weight),by=.(Decile)][order(Decile)]
  HighEngle[Region=="Rural" & NewArea_Name=="Sistan",weighted.mean(Total_Exp_Month_Per,Weight),by=.(Decile)][order(Decile)]
  
  HighEngle[Region=="Urban" & NewArea_Name=="Sistan",sum(HIndivNo),by=.(Decile)][order(Decile)]
  HighEngle[Region=="Urban" & NewArea_Name=="Sistan",weighted.mean(TOriginalFoodExpenditure_Per,Weight),by=.(Decile)][order(Decile)]
  HighEngle[Region=="Urban" & NewArea_Name=="Sistan",weighted.mean(Total_Exp_Month_Per,Weight),by=.(Decile)][order(Decile)]
  
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")