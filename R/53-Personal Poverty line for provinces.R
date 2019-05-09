# 53-Personal Poverty line for provinces.R
# 
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ T-Tests =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(spatstat)

year<-93
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"MD4test.rda"))

MDH <- TD[ TFoodExpenditure_Per>0.8*FPLine & TFoodExpenditure_Per<1.2*FPLine]


PPH <- MDH[ ,.(.N,PPLine=weighted.mean(PersonalPLine,Weight),
               FPLine=mean(FPLine),EngelPersonal=mean(EngelPersonal)),by=.(Region,NewArea2)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")