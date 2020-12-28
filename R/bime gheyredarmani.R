# Step 7,8,9 and other
# 
# Copyright © 2018: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================bime =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

X1 <- data.table(Year = numeric(0),share = integer(0),Decile = integer(0))
X2 <- data.table(Year = numeric(0),share = integer(0),Decile = integer(0),
                 Region =NA_character_ )

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"job.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))

MD[,Decile:=NULL]
if (year==98){
  Deciles[,Decile:=Decile1]
}
MD<-merge(MD,Deciles[,.(HHID,Decile)],by=c("HHID"))

MD<-merge(MD,job[,.(HHID,PubWageNetIncomeY,PrvWageNetIncomeY)],by=c("HHID"))


load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))


RT<-merge(MD,TotalDurable[,.(HHID,`125113`, `125114`,`125115`,`125116`,
                             `125117`,`125118`,`125119`,`125121`)])
A1<-RT[,.(share=weighted.mean(`125121`>0,Weight)),by="Decile"]
A2<-RT[,.(share=weighted.mean(`125121`>0,Weight)),by=c("Region","Decile")]

A1[,Year:=year]
X1 <- rbind(A1,X1)

A2[,Year:=year]
X2 <- rbind(A2,X2)

}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")