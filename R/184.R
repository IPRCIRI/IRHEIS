#164-Step 4-FindInitialPoor.R
# 
# Copyright © 2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
#library(ggplot2)
#library(compare)

year<-96
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
Decile96<-MD[,.(HHID,Decile,Percentile,Year)]

year<-97
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
Decile97<-MD[,.(HHID,Decile,Percentile,Year)]

year<-98
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
Decile98<-MD[,.(HHID,Decile,Percentile,Year)]

Decile<-rbind(Decile96,Decile97)
Decile<-rbind(Decile,Decile98)

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3Ostan.rda"))
MD<-merge(MD,Decile,by=c("HHID","Year"))

save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorOstan.rda"))


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
