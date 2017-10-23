# 35-Intra.R
# Builds the IntraIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHIntraIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


IntraTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Intra))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Intrawt <- IntraTable[Year==year]
  tab <- Intrawt$Table
  if(is.na(tab))
    next
  UTIntraW <- Tables[[paste0("U",year,tab)]]
  RTIntraW <- Tables[[paste0("R",year,tab)]]
  TIntraW <- rbind(UTIntraW,RTIntraW,fill=TRUE)
  for(n in names(TIntraW)){
    x <- which(Intrawt==n)
    if(length(x)>0)
      setnames(TIntraW,n,names(Intrawt)[x])
  }
  pcols <- intersect(names(TIntraW),c("HHID","Code","intra"))
  TIntraW <- TIntraW[,pcols,with=FALSE]
  if(year %in% 63:68){
    TIntraW <- TIntraW[Code %in% Intrawt$StartCode:Intrawt$EndCode]
  }
  
  TIntraW[is.na(TIntraW)] <- 0
  IntraWageData <- TIntraW[,lapply(.SD,sum),by=HHID]
  save(IntraWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"IntraWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
