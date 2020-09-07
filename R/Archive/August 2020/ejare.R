# 168- Step 8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Ejare =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

DT1 <- data.table(Year=NA_integer_,Exp=NA_real_,Exp_Pure=NA_real_,
                  Region=NA_character_,
                    Malek=NA_integer_,Decile=NA_real_)[0]

DT2 <- data.table(Year=NA_integer_,Exp=NA_real_,Region=NA_character_,
                  Malek=NA_integer_,Decile=NA_real_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodCon.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  #load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
  
  MD[,Durable_Exp:=NULL]
  MD[,Durable_Sale:=NULL]
  
  if (year >= 90){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
    MD[,Decile:=NULL]
    MD[,Percentile:=NULL]
    MD<-merge(MD,Deciles,by="HHID")
  }
  
  if (year < 90){
    MD<-merge(MD,TotalFoodCon,by="HHID",all.x=TRUE)
  }
  
  MD<-merge(MD,TotalFoodExp[,.(HHID,G01111,G01114,G01121,G01123,G0114,G01153,`011731`)],by="HHID",all.x=TRUE)
  MD<-merge(MD,DurableData,by="HHID",all.x=TRUE)
  MD[,Malek:=ifelse(tenure=="OwnLandandBuilding" | tenure=="Apartment" ,1,0)]

  X <- MD[,.(Exp_Pure=weighted.mean(Total_Exp_Month_Per-(ServiceExp/EqSizeOECD),Weight),
             Exp=weighted.mean(Total_Exp_Month_Per,Weight)),by=c("Region","Malek","Decile")]
  X[,Year:=year]
  DT1 <- rbind(DT1,X)
  write.csv(DT1,file="DT1.csv")
  


}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")