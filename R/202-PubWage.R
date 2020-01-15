# 31-PubWage.R
# Builds the PubWageData data.table for wage income data of 
# households working in public sector.
#
# Copyright © 2017-2018: Arin Shahbazian & Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ PubWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PubWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_PubWage))


for(year in 84:97){#(Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  pubwt <- PubWageTable[Year==year]
  tab <- pubwt$Table
  if(is.na(tab))
    next
  if(length(tab)>1){
    pubwtu <- pubwt[Region=="U"]
    pubwtr <- pubwt[Region=="R"]
  }else{
    pubwtu <- pubwt
    pubwtr <- pubwt
  }
  
  
  tab <- pubwtu$Table
  UTpubW <- Tables[[paste0("U",year,tab)]]
  for(n in names(UTpubW)){
    x <- which(pubwtu==n)
    if(length(x)>0)
      setnames(UTpubW,n,names(pubwtu)[x])
  }
  
  tab <- pubwtr$Table
  RTpubW <- Tables[[paste0("R",year,tab)]]
  for(n in names(RTpubW)){
    x <- which(pubwtr==n)
    if(length(x)>0)
      setnames(RTpubW,n,names(pubwtr)[x])
  }
  
  TpubW <- rbind(RTpubW,UTpubW,fill=TRUE)
  save(TpubW, file = paste0(Settings$HEISProcessedPath,"Y",year,"TpubW.rda"))
  
  
  #  print(names(TpubW))
  
  pcols <- intersect(names(TpubW),c("HHID","IndivNo","WageSector","HoursPerDay","DayPerWeek","PubWageNetIncomeY"))
  #pcols <- intersect(names(TpubW),c("HHID","indiv","shaghel","shoghl","current_shoghl","faaliat","section","hour_in_day","day_in_week","gross_income_m","gross_income_y","mostameri_m","mostameri_y","gheyremostameri_m","gheyremostameri_y","net_income_m","net_income_y"))
  TpubW <- TpubW[,pcols,with=FALSE]
  
  if(year <=68){
    TpubW[,WageSector:=1]
  }else{
    TpubW <- TpubW[WageSector==1]
  }
  if(year >= 66){
    TpubW[,HHID:=as.numeric(HHID)]
  }
  
  if(year >= 86){
    TpubW[,PubWageNetIncomeY:=as.numeric(PubWageNetIncomeY)]
  }
  TpubW[,IndivNo:=as.numeric(IndivNo)]
  TpubW[is.na(IndivNo),IndivNo:=1]
   
  TpubW[is.na(TpubW)] <- 0
  TpubW <- TpubW[PubWageNetIncomeY>0]

  if("HoursPerDay" %in% names(TpubW)){
    print(year)
  }
  PubWageData <- TpubW[,.(PubWageNetIncomeY=sum(PubWageNetIncomeY),
                          Hours=sum(HoursPerDay*DayPerWeek),
                          PubEarners=.N,
                          Sector=1)
#                          Sector=factor(WageSector,levels = Settings$SectorsNumbers, labels = Settings$SectorsNames))
                       ,by=HHID]
  
   save(PubWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
  
   Pub1<-PubWageData[,.(HHID,PubEarners)]
   save(Pub1, file = paste0(Settings$HEISProcessedPath,"Y",year,"Pub1.rda"))
   
   Pub2<-TpubW[,.(HHID,IndivNo)]
   save(Pub2, file = paste0(Settings$HEISProcessedPath,"Y",year,"Pub2.rda"))
   
   }
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
