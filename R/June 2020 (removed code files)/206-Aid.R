# 35-Aid.R
# Builds the AidIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHAidIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


AidTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Aid))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Aidwt <- AidTable[Year==year]
  tab <- Aidwt$Table
  if(is.na(tab))
    next
  UTAidW <- Tables[[paste0("U",year,tab)]]
  RTAidW <- Tables[[paste0("R",year,tab)]]
  TAidW <- rbind(UTAidW,RTAidW,fill=TRUE)
  for(n in names(TAidW)){
    x <- which(Aidwt==n)
    if(length(x)>0)
      setnames(TAidW,n,names(Aidwt)[x])
  }
  pcols <- intersect(names(TAidW),c("HHID","Code","aid"))
  TAidW <- TAidW[,pcols,with=FALSE]
  if(year %in% 63:68){
    TAidW <- TAidW[Code %in% Aidwt$StartCode:Aidwt$EndCode]
  }
  if(year %in% 84:94){
    TAidW[,aid:=as.numeric(aid)]
  }
  TAidW[is.na(TAidW)] <- 0
  AidWageData <- TAidW[,lapply(.SD,sum),by=HHID]
  save(AidWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
