# 33- HHBussInc.R
# 
# Builds the bussiness income data.table for households
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ BussInc =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


bussWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_BussInc))


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
  save(TbussW, file = paste0(Settings$HEISProcessedPath,"Y",year,"TbussW.rda"))
  
  
  for(n in names(TbussW)){
    x <- which(busswt==n)
    if(length(x)>0)
      setnames(TbussW,n,names(busswt)[x])
  }
  pcols <- intersect(names(TbussW),c("HHID","IndivNo","WorkType","BSector","BussNetIncomeY"))
  TbussW <- TbussW[,pcols,with=FALSE]
  
  
  if(year <= 68){
    TbussW[,BSector:=2]
  }else{
    TbussW <- TbussW[BSector==2] 
  }
  

  if(year >= 84){
    TbussW[,BussNetIncomeY:=as.numeric(BussNetIncomeY)]
  }

   TbussW[is.na(TbussW)] <- 0
   TbussW2<-TbussW[WorkType==4,.(HHID,WorkType)]
   save(TbussW2, file = paste0(Settings$HEISProcessedPath,"Y",year,"TbussW2.rda"))
   
   
   BussIncomeData <- TbussW[,.(BussNetIncomeY=sum(BussNetIncomeY),
                               BussEarners=.N,
                               Sector=100),
                              # Sector=factor(4,levels = Settings$SectorsNumbers, labels = Settings$SectorsNames))
                            ,by=HHID]


   save(BussIncomeData, file = paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))

   Buss1<-BussIncomeData[,.(HHID,BussEarners)]
   save(Buss1, file = paste0(Settings$HEISProcessedPath,"Y",year,"Buss1.rda"))
   
   Buss2<-TbussW[,.(HHID,IndivNo)]
   save(Buss2, file = paste0(Settings$HEISProcessedPath,"Y",year,"Buss2.rda"))
   
   }
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
