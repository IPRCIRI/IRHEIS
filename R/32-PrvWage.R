# 32-PrvWage.R
# Builds the PubWageData data.table for wage income data of 
# households working in private sector.
#
# Copyright © 2017-2018: Arin Shahbazian & Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ PrvWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PrvWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_PrvWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  prvwt <- PrvWageTable[Year==year]
  tab <- prvwt$Table
  if(is.na(tab))
    next
  if(length(tab)>1){
    prvwtu <- prvwt[Region=="U"]
    prvwtr <- prvwt[Region=="R"]
  }else{
    prvwtu <- prvwt
    prvwtr <- prvwt
  }
  
  
  tab <- prvwtu$Table
  UTprvW <- Tables[[paste0("U",year,tab)]]
  for(n in names(UTprvW)){
    x <- which(prvwtu==n)
    if(length(x)>0)
      setnames(UTprvW,n,names(prvwtu)[x])
  }
  
  tab <- prvwtr$Table
  RTprvW <- Tables[[paste0("R",year,tab)]]
  for(n in names(RTprvW)){
    x <- which(prvwtr==n)
    if(length(x)>0)
      setnames(RTprvW,n,names(prvwtr)[x])
  }
  
  TprvW <- rbind(RTprvW,UTprvW,fill=TRUE)
  
  pcols <- intersect(names(TprvW),c("HHID","IndivNo","WageSector","PrvWageNetIncomeY"))
  TprvW <- TprvW[,pcols,with=FALSE]
  
  # Consider Cooperative sector as prviate sector
  if(year <= 68){
    TprvW[,WageSector:= 3]
  }else{
    TprvW <- TprvW[ WageSector !=1 ] 
    if(year <=76){
      TprvW[,WageSector:=WageSector+1]
    }
  }
  # if(year %in% 69:76){
  #   TPrvW <- TPrvW[ WageSector ==2 ] 
  # } else if(year %in% 77:94){
  #   TPrvW <- TPrvW[ WageSector ==3 ] 
  # } 
  
  if(year >= 66){
    TprvW[,HHID:=as.numeric(HHID)]
  }
  
  if(year >= 86){
    TprvW[,PrvWageNetIncomeY:=as.numeric(PrvWageNetIncomeY)]
  }
  TprvW[,IndivNo:=as.numeric(IndivNo)]
  TprvW[is.na(IndivNo),IndivNo:=1]
  
  TprvW[is.na(TprvW)] <- 0
  TprvW <- TprvW[PrvWageNetIncomeY>0]
  
  PrvWageData <- TprvW[,.(PrvWageNetIncomeY=sum(PrvWageNetIncomeY),
                          PrvEarners=.N,
                          Sector=10)
                          # Sector=factor(WageSector,levels = Settings$SectorsNumbers, labels = Settings$SectorsNames))
                       ,by=HHID]
  
    save(PrvWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
 #  print(mean(PrvWageData$PrvWageNetIncomeY))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
