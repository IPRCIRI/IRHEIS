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


AgriIncTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_AgriInc))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Agriwt <- AgriIncTable[Year==year]
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
  pcols <- intersect(names(TAgriW),c("HHID","IndivNo","WorkType","BSector","AgriNetIncomeY"))
  TAgriW <- TAgriW[,pcols,with=FALSE]
  
  if(year <= 68){
    TAgriW[,BSector:=1] 
  }else{
    TAgriW <- TAgriW[BSector==1] 
  }
  
  if(year >= 84){
    TAgriW[,AgriNetIncomeY:=as.numeric(AgriNetIncomeY)]
  }
  
  TAgriW[is.na(TAgriW)] <- 0
  TAgriW2<-TAgriW[WorkType==4,.(HHID,WorkType)]
  save(TAgriW2, file = paste0(Settings$HEISProcessedPath,"Y",year,"TAgriW2.rda"))
  
  
  AgriIncomeData <- TAgriW[,.(AgriNetIncomeY=sum(AgriNetIncomeY),
                              AgriEarners=.N,
                              Sector=1000)
                           #  Sector=factor(5,levels = Settings$SectorsNumbers, labels = Settings$SectorsNames))
                           ,by=HHID]
  
  
   save(AgriIncomeData, file = paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))

   Agri1<-AgriIncomeData[,.(HHID,AgriEarners)]
   save(Agri1, file = paste0(Settings$HEISProcessedPath,"Y",year,"Agri1.rda"))
   
   Agri2<-TAgriW[,.(HHID,IndivNo)]
   save(Agri2, file = paste0(Settings$HEISProcessedPath,"Y",year,"Agri2.rda"))
   
   }
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
