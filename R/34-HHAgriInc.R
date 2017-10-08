# 34-AggrInc.R
#Builds the Agricultral Wages data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  HHAggrIncTable =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


AgriWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_AgriWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Agriwt <- AgriWageTable[Year==year]
  tab <- Agriwt$Table
  if(is.na(tab))
    next
  UTAgriW <- Tables[[paste0("U",year,tab)]]
  RTAgriW <- Tables[[paste0("R",year,tab)]]
  TAgriW <- rbind(UTAgriW,RTAgriW,fill=TRUE)
  for(n in names(TAgriW)){
    x <- which(Agriwt==n)
    if(length(x)>0)
      setnames(TAgriW,n,names(Agriwt)[x])
  }
  pcols <- intersect(names(TAgriW),c("HHID","indiv","shaghel","shoghl","faaliat","job condition","agriculture","hour_in_day","day_in_week","cost1","cost2","cost3","cost4","cost5","sell","net_income_y"))
  TAgriW <- TAgriW[,pcols,with=FALSE]
  
  if(year %in% 69:94){
    TAgriW <- TAgriW[ agriculture ==1 ] 
  }
  
  TAgriW[is.na(TAgriW)] <- 0
  # AgriWageData <- TAgriW[,lapply(.SD,sum),by=HHID]
  # save(AgriWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
