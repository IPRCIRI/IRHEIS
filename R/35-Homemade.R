# 35-Homemade.R
# Builds the HomemadeIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHomemadeIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


HomemadeTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Homemade))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Homemadewt <- HomemadeTable[Year==year]
  tab <- Homemadewt$Table
  if(is.na(tab))
    next
  UTHomemadeW <- Tables[[paste0("U",year,tab)]]
  RTHomemadeW <- Tables[[paste0("R",year,tab)]]
  THomemadeW <- rbind(UTHomemadeW,RTHomemadeW,fill=TRUE)
  for(n in names(THomemadeW)){
    x <- which(Homemadewt==n)
    if(length(x)>0)
      setnames(THomemadeW,n,names(Homemadewt)[x])
  }
  pcols <- intersect(names(THomemadeW),c("HHID","Code","homemade"))
  THomemadeW <- THomemadeW[,pcols,with=FALSE]
  if(year %in% 63:68){
    THomemadeW <- THomemadeW[Code %in% Homemadewt$StartCode:Homemadewt$EndCode]
  }

  THomemadeW[is.na(THomemadeW)] <- 0
  HomemadeWageData <- THomemadeW[,lapply(.SD,sum),by=HHID]
  save(HomemadeWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"HomemadeWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
