# 229-FinServices ### NOT WORKING ANYMORE

# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ FinServices =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

FinServicesTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_FinServices))


for(year in (Settings$startyear:Settings$endyear)){
  
  ty <- FinServicesTables[Year==year]
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
  
  pcols <- intersect(names(TI),c("HHID","Code","FinServicesExp"))
  TI <- TI[,pcols,with=FALSE]
  TI <- TI[Code %in% ty$StartCode:ty$EndCode]
  TI[,FinServicesExp:=as.numeric(FinServicesExp)]

  TI[is.na(TI)] <- 0

  FinServicesData <- TI[,.(FinServicesExp =sum(FinServicesExp)),by=HHID]
  
  save(FinServicesData, file = paste0(Settings$HEISProcessedPath,"Y",year,"FinServices.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)