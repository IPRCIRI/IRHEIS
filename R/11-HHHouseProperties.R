# 11-HHHouseProperties.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHouseProperties =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)


P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))


years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  

  P2 <- rbind(Tables[[paste0("R",year,"P2")]],Tables[[paste0("U",year,"P2")]])
  nP2 <- names(P2)
  if(length(which(sapply(P2, is.character)))>0){
    P2c <- P2[,lapply(.SD,iconv,"WINDOWS-1252","UTF-8"), .SDcols=sapply(P2,is.character)] 
    P2nc <- P2[,!sapply(P2,is.character),with=FALSE]
    P2 <- cbind(P2c,P2nc)[,nP2,with=FALSE]
  }
  
  if(year==96){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[2:46]
    setnames(P2,names(a[ind]))
    
  }else if(year %in% 89:95){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[-1]
    setnames(P2,names(a[ind]))
  }
  

  f <- function(x){as.numeric(str_trim(x))}
  P2 <- P2[, lapply(.SD, f)] 
  HHHouseProperties<-P2
  
  save(HHHouseProperties, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
