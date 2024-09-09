# 105-ReadWriteIntoR
# Read and Save All Tables into R Format
#
# Copyright © 2015: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ ReadWriteIntoR =====================================\n")

library(yaml)

Settings <- yaml.load_file("Settings.yaml")

file.copy(from = Settings$D80LinkSource, to = Settings$D80LinkDest, overwrite = TRUE)

library(RODBC)
library(foreign)
library(data.table)

#load(Settings$weightsFile)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  l <- dir(path=Settings$HEISAccessPath, pattern=glob2rx(paste0(year,".*")),ignore.case = TRUE)
  if(year==80){
    cns<-odbcConnectAccess2007(Settings$D80LinkDest)
  }else{
    cns<-odbcConnectAccess2007(paste0(Settings$HEISAccessPath, l))
  }
  tbls <- data.table(sqlTables(cns))[TABLE_TYPE %in% c("TABLE","SYNONYM"),]$TABLE_NAME
  print(tbls)
  Tables <- vector(mode = "list")#, length = length(tbls))
  
  for (tbl in tbls) {
    D <- data.table(sqlFetch(cns,tbl,stringsAsFactors=FALSE))
    Tables[[tbl]] <- D
  }
  names(Tables) <- toupper(names(Tables))
  ### years after 1400 change to 100 and more
  #if (year==100){
    #names(Tables) <- gsub('1400','100',names(Tables))
 # }

  
  if (year >= 100){
    newyear <- paste0('10', substr(year, 2, 2))
    names(Tables) <- gsub(year, newyear, names(Tables))
  
#  Tables[[paste0("RU",year,"Weights")]] <- AllWeights[(Year==year),]
  close(cns)
  save(Tables,file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
