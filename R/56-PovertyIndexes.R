# 56-PovertyIndexes.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
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
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  #HCR
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
}

 #Shedate faghr
 MD[FinalPoor==1,Index:=1-(weighted.mean(Total_Exp_Month_Per_nondurable/PovertyLine,Weight))]
 #cat(MD[FinalPoor==1,weighted.mean(Index,Weight)])
 
 #Foster Index
 MD[FinalPoor==1,Index2:=(1-(weighted.mean(Total_Exp_Month_Per_nondurable/PovertyLine,Weight)))^2]
 MD[FinalPoor==1,Index2:=weighted.mean(Index2,Weight)]
 #cat(MD[FinalPoor==1,weighted.mean(Index,Weight)])
 
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")