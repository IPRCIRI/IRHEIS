# 23-HHOther.R
# Builds the Other expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHOther =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


OtherTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Other))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- OtherTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Other_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Other_Exp:=as.numeric(Other_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  OtherData <- TC[,lapply(.SD,sum),by=HHID]
  save(OtherData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Others.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
