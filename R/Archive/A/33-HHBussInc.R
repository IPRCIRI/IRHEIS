# 33- HHBussInc.R
# 
# Builds the bussiness income data.table for households
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBussIncTable =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


bussWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_BussWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  busswt <- bussWageTable[Year==year]
  tab <- busswt$Table
  if(is.na(tab))
    next
  UTbussW <- Tables[[paste0("U",year,tab)]]
  RTbussW <- Tables[[paste0("R",year,tab)]]
  TbussW <- rbind(UTbussW,RTbussW,fill=TRUE)
  for(n in names(TbussW)){
    x <- which(busswt==n)
    if(length(x)>0)
      setnames(TbussW,n,names(busswt)[x])
  }
  pcols <- intersect(names(TbussW),c("HHID","indiv","shaghel","shoghl","faaliat","job condition","agriculture","hour_in_day","day_in_week","cost1","cost2","cost3","cost4","cost5","sell","net_income_y"))
  TbussW <- TbussW[,pcols,with=FALSE]
  
  if(year %in% 69:94){
    TbussW <- TbussW[ agriculture ==2 ] 
  }
  
  TbussW[is.na(TbussW)] <- 0
  # bussWageData <- TbussW[,lapply(.SD,sum),by=HHID]
  # save(bussWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"bussWages.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
