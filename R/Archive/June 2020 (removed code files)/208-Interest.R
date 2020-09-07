# 35-Interest.R
# Builds the InterestIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHInterestIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


InterestTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Interest))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Interestwt <- InterestTable[Year==year]
  tab <- Interestwt$Table
  if(is.na(tab))
    next
  UTInterestW <- Tables[[paste0("U",year,tab)]]
  RTInterestW <- Tables[[paste0("R",year,tab)]]
  TInterestW <- rbind(UTInterestW,RTInterestW,fill=TRUE)
  for(n in names(TInterestW)){
    x <- which(Interestwt==n)
    if(length(x)>0)
      setnames(TInterestW,n,names(Interestwt)[x])
  }
  pcols <- intersect(names(TInterestW),c("HHID","Code","interest"))
  TInterestW <- TInterestW[,pcols,with=FALSE]
  if(year %in% 63:68){
    TInterestW <- TInterestW[Code %in% Interestwt$StartCode:Interestwt$EndCode]
  }
  TInterestW[is.na(TInterestW)] <- 0
  InterestWageData <- TInterestW[,lapply(.SD,sum),by=HHID]
  save(InterestWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"InterestWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
