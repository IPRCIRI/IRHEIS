# 23-HHDurable.R
# Builds the Durable expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHDurable =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


DurableTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Durable))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- DurableTables[Year==year]
  tab <- ct$Table
  if(is.na(tab))
    next
  UTC <- Tables[[paste0("U",year,tab)]]
  RTC <- Tables[[paste0("R",year,tab)]]
  TC <- rbind(UTC,RTC)
  for(n in names(TC)){
    x <- which(ct==n)
    if(length(x)>0)
      setnames(TC,n,names(ct)[x])
  }
  pcols <- intersect(names(TC),c("HHID","Code","BuyingMethod","a","b","Durable_Exp","d"))
  TC <- TC[,pcols,with=FALSE]

  if(year %in% 84:94){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)]
    TC[,d:=as.numeric(d)]
  }
  if(year %in% 88:94){
    TC[,a:=as.numeric(a)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  DurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(DurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
