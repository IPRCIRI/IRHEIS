# 23-HHHotel.R
# Builds the Hotel expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHotel =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


HotelTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hotel))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- HotelTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Hotel_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Hotel_Exp:=as.numeric(Hotel_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  HotelData <- TC[,lapply(.SD,sum),by=HHID]
  save(HotelData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Hotels.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
