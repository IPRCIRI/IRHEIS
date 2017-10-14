# 23-HHClothl.R
# Builds the Cloth expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHCloth =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


ClothTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Cloth))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- ClothTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","BuyingMethod","a","b","Cloth_Exp"))
  TC <- TC[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:94){
    TC[,Cloth_Exp:=as.numeric(Cloth_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  ClothData <- TC[,lapply(.SD,sum),by=HHID]
  save(ClothData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Cloths.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
