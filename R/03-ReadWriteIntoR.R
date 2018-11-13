# 03-ReadWriteIntoR
# Read and Save All Tables into R Format
#
# Copyright © 2015: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ ReadWriteIntoR =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

file.copy(from = Settings$D80LinkSource, to = Settings$D80LinkDest)

library(RODBC)
library(foreign)
library(data.table)

load(Settings$weightsFile)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  l <- dir(path=Settings$HEISAccessPath, pattern=glob2rx(paste0(year,".*")),ignore.case = TRUE)
  if(year==80)
    cns<-odbcConnectAccess2007(Settings$D80LinkDest)
  else
    cns<-odbcConnectAccess2007(paste0(Settings$HEISAccessPath, l))
  
  tbls <- data.table(sqlTables(cns))[TABLE_TYPE %in% c("TABLE","SYNONYM"),]$TABLE_NAME
  print(tbls)
  Tables <- vector(mode = "list")#, length = length(tbls))
  
  for (tbl in tbls) {
    D <- data.table(sqlFetch(cns,tbl,stringsAsFactors=FALSE))
    Tables[[tbl]] <- D
  }
  names(Tables) <- toupper(names(Tables))
#  Tables[[paste0("RU",year,"Weights")]] <- AllWeights[(Year==year),]
  close(cns)
  save(Tables,file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)