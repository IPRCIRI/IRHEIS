# House Properties according to Province.R
# Builds the House Properties according to Province
#
# Copyright © 2020:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHouseProperties =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)

tenure <- data.table(Year=NA_integer_,Region=NA_integer_,ProvinceCode=NA_real_,
                           OwnLandandBuilding=NA_real_)[0]


for (year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:", year, "\n"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  MD<-merge(MD,HHHouseProperties[,.(HHID,tenure,room,area)],by="HHID")
  
  X1 <- MD[,.(OwnLandandBuilding=weighted.mean(tenure=="OwnLandandBuilding",Weight)),by=.(Region,ProvinceCode)]
  X1[,Year:=year]
  
  tenure <- rbind(tenure,X1)
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
