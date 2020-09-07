# 35-Rent.R
# Builds the RentIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRentIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


RentTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rent))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Rentwt <- RentTable[Year==year]
  tab <- Rentwt$Table
  if(is.na(tab))
    next
  UTRentW <- Tables[[paste0("U",year,tab)]]
  RTRentW <- Tables[[paste0("R",year,tab)]]
  TRentW <- rbind(UTRentW,RTRentW,fill=TRUE)
  for(n in names(TRentW)){
    x <- which(Rentwt==n)
    if(length(x)>0)
      setnames(TRentW,n,names(Rentwt)[x])
  }
  pcols <- intersect(names(TRentW),c("HHID","Code","rent"))
  TRentW <- TRentW[,pcols,with=FALSE]
  if(year %in% 63:68){
    TRentW <- TRentW[Code %in% Rentwt$StartCode:Rentwt$EndCode]
  }
  TRentW[is.na(TRentW)] <- 0
  RentWageData <- TRentW[,lapply(.SD,sum),by=HHID]
  save(RentWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"RentWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
