# 133-HHDurableExpenditures.R
# Builds the Durbable Expenditures detailed data.table for households
#
# Copyright © 2017-2020: Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)


cat("\n\n================ Section13:HHDurable =====================================\n")
DurableTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Durable))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- DurableTables[Year==year]
  tab <- ct$Table
  if(is.na(tab))
    next
  UTC <- Tables[[paste0("U",year,tab)]]
  RTC <- Tables[[paste0("R",year,tab)]]
  TC <- rbind(UTC,RTC)
  for(n in names(TC)){
    x <- which(ct==n)
    if(length(x)>0)
      setnames(TC,n,names(ct)[x])
  }
  pcols <- intersect(names(TC),c("HHID","Code","Durable_Exp","Durable_Sale"))
  TC <- TC[,pcols,with=FALSE]
  
 # if(year >= 84){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)/12]
    TC[,Durable_Sale:=as.numeric(Durable_Sale)/12]
#  }
  
  DurableData_Detail<-TC
  save(DurableData_Detail, file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableData_Detail.rda"))
}

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")