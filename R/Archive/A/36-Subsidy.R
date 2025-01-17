# 36-Subsidy.R
# Builds the SubsidyIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSubsidy =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


SubsidyWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_SubsidyWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Subsidywt <- SubsidyWageTable[Year==year]
  tab <- Subsidywt$Table
  if(is.na(tab))
    next
  UTSubsidyW <- Tables[[paste0("U",year,tab)]]
  RTSubsidyW <- Tables[[paste0("R",year,tab)]]
  TSubsidyW <- rbind(UTSubsidyW,RTSubsidyW,fill=TRUE)
  for(n in names(TSubsidyW)){
    x <- which(Subsidywt==n)
    if(length(x)>0)
      setnames(TSubsidyW,n,names(Subsidywt)[x])
  }
  pcols <- intersect(names(TSubsidyW),c("HHID","indiv","dimension","month_number","Subsidy"))
  TSubsidyW <- TSubsidyW[,pcols,with=FALSE]
  

  
  TSubsidyW[is.na(TSubsidyW)] <- 0
   SubsidyWageData <- TSubsidyW[,lapply(.SD,sum),by=HHID]
   save(SubsidyWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
