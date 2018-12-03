# 35-Retirement.R
# Builds the RetirementIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRetirementIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


RetirementTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Retirement))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Retirementwt <- RetirementTable[Year==year]
  tab <- Retirementwt$Table
  if(is.na(tab))
    next
  UTRetirementW <- Tables[[paste0("U",year,tab)]]
  RTRetirementW <- Tables[[paste0("R",year,tab)]]
  TRetirementW <- rbind(UTRetirementW,RTRetirementW,fill=TRUE)
  for(n in names(TRetirementW)){
    x <- which(Retirementwt==n)
    if(length(x)>0)
      setnames(TRetirementW,n,names(Retirementwt)[x])
  }
  pcols <- intersect(names(TRetirementW),c("HHID","Code","retirement"))
  TRetirementW <- TRetirementW[,pcols,with=FALSE]
  if(year %in% 63:68){
    TRetirementW <- TRetirementW[Code %in% Retirementwt$StartCode:Retirementwt$EndCode]
  }
  
  TRetirementW[is.na(TRetirementW)] <- 0
  RetirementWageData <- TRetirementW[,lapply(.SD,sum),by=HHID]
  save(RetirementWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
