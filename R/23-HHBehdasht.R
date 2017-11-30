# 23-HHBehdasht.R
# Builds the Behdasht expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBehdasht =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


BehdashtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Behdasht))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- BehdashtTables[Year==year]
  tab <- mt$Table
  if(is.na(tab))
    next
  UTM <- Tables[[paste0("U",year,tab)]]
  RTM <- Tables[[paste0("R",year,tab)]]
  TM <- rbind(UTM,RTM)
  for(n in names(TM)){
    x <- which(mt==n)
    if(length(x)>0)
      setnames(TM,n,names(mt)[x])
  }
  pcols <- intersect(names(TM),c("HHID","Code","Behdasht_Exp"))
  TM <- TM[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:95){
    TM[,Behdasht_Exp:=as.numeric(Behdasht_Exp)]
  }
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  BehdashtData <- TM[,lapply(.SD,sum),by=HHID]
  save(BehdashtData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Behdashts.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
