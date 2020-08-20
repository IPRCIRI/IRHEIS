 # Some Specific Calculations.R

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Some Specific Calculations =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

load( file = paste0(Settings$HEISProcessedPath,"Y",year,"LivestockExp.rda"))
load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BirdsMeatExp.rda"))
load( file = paste0(Settings$HEISProcessedPath,"Y",year,"FishandShrimpExp.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Behdasht1",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Behdasht2",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Behdasht3",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durable17",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durable18",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durable19",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durable20",".rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durable21",".rda"))

Specific<-merge(LivestockData,BirdsMeatData,all = TRUE)
Specific<-merge(Specific,FishandShrimpData,all = TRUE)
Specific<-merge(Specific,Behdasht1,all = TRUE)
Specific<-merge(Specific,Behdasht2,all = TRUE)
Specific<-merge(Specific,Behdasht3,all = TRUE)
Specific<-merge(Specific,Durable17,all = TRUE)
Specific<-merge(Specific,Durable18,all = TRUE)
Specific<-merge(Specific,Durable19,all = TRUE)
Specific<-merge(Specific,Durable20,all = TRUE)
Specific<-merge(Specific,Durable21,all = TRUE)

Specific[is.na(Specific)] <- 0
save(Specific, file=paste0(Settings$HEISProcessedPath,"Y",year,"Specific.rda"))

}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])


