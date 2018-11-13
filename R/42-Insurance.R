# 42-Insurance
# Note: for now only health insurance, TODO: add other insurance types
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Insurance =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

InsuranceTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Insurance))


for(year in (Settings$startyear:Settings$endyear)){
  
  ty <- InsuranceTables[Year==year]
  tab <- ty$Table
  if(is.na(tab))
    next
  
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  UTI <- Tables[[paste0("U",year,tab)]]
  RTI <- Tables[[paste0("R",year,tab)]]
  rm(Tables)
  TI <- rbind(UTI,RTI)
  
  for(n in names(TI)){
    x <- which(ty==n)
    if(length(x)>0)
      setnames(TI,n,names(ty)[x])
  }
  
  pcols <- intersect(names(TI),c("HHID","Code","InsuredCount","InsuranceCosts"))
  TI <- TI[,pcols,with=FALSE]
  TI <- TI[Code %in% ty$StartCode:ty$EndCode]
  TI[,InsuredCount:=as.numeric(InsuredCount)]

  TI[is.na(TI)] <- 0
  TI[,THI:=ifelse(Code %in% c(ty$SS1,ty$SS2),1,0)]
  
  InsuranceData <- TI[,.(InsuredCount=sum(InsuredCount),THI=sum(THI)),by=HHID]
  InsuranceData[,THI:=ifelse(THI>0,1,0)]
  
  save(InsuranceData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Insurance.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)