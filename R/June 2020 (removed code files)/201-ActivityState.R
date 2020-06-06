# 30-ActivityState.R
# Builds the Wages data.table for households
#
# Copyright © 2018: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPubWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


ActivityStateTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_ActivityState))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  act <- ActivityStateTables[Year==year]
  tab <- act$Table
  if(is.na(tab))
    next
  UTAct <- Tables[[paste0("U",year,tab)]]
  RTAct <- Tables[[paste0("R",year,tab)]]
  TAct <- rbind(UTAct,RTAct,fill=TRUE)
  
  TAct <- TAct[,lapply(.SD, as.numeric)]
  summary(TAct)
  
  for(n in names(TAct)){
    x <- which(act==n)
    if(length(x)>0)
      setnames(TAct,n,names(act)[x])
  }
  
  pcols <- intersect(names(TAct),c("HHID","IndivNo","ActivityState"))
  TAct <- TAct[,pcols,with=FALSE]
  TAct <- TAct[!is.na(ActivityState)]
  save(TAct, file = paste0(Settings$HEISProcessedPath,"Y",year,"ActivityState.rda"))
  TAct2<-TAct[ActivityState==1]
  TAct2<-TAct2[,kar:=1]
  TAct2 <- TAct2[,lapply(.SD,sum),by=HHID]
  TAct2[,ActivityState:=NULL]
  TAct2[,IndivNo:=NULL]
  save(TAct2, file = paste0(Settings$HEISProcessedPath,"Y",year,"ActivityState2.rda"))

  
}
  