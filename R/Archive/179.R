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


cat("\n\n================ Section13:HHDurable ==============================\n")
DurableTables <- data.table(read_excel(Settings$MetaDataFilePath,
                                       sheet=Settings$MDS_Durable))
DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItems))
year<-96
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
  
  DurableData_Detail <- merge(TC,DurableItems,by="Code",all = TRUE)
  DurableData_Detail <- DurableData_Detail[!is.na(HHID)]
  DurableData_Detail[is.na(Durable_Exp), Durable_Exp:=0]
  DurableData_Detail[is.na(Durable_Sale), Durable_Sale:=0]
  
  
  #save(DurableData_Detail, file = paste0(Settings$HEISProcessedPath,"Y",year,
   #                                      "DurableData_Detail.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  itemslist <- c("cellphone","washer","dishwasher","car",
                 "oven","tvbw","tvcr","refrigerator","frez_refrig","freezer",
                 "computer","cooler_gas","cooler_gas_movable")
  
  for(item in itemslist)
    if(!(item %in% names(HHHouseProperties))){
      HHHouseProperties[,(item):=FALSE]
    }
  
  OwnsDurableItems <- HHHouseProperties[,.(HHID,
                                           Cellphone=cellphone*1,
                                           Washer=(washer|dishwasher)*1,
                                           Car_Repair=car*1,
                                           Car_Tire=car*1,
                                           Oven=oven*1,
                                           TV=(tvbw |tvcr)*1,
                                           Refrigrator=(refrigerator|frez_refrig|freezer)*1,
                                           Car=car*1,
                                           Computer=computer*1,
                                           AC=(cooler_gas |cooler_gas_movable)*1,
                                           Car_Motor=car*1)]
  
  DurableData_Detail96<-copy((DurableData_Detail))
  OwnsDurableItems96<-copy(OwnsDurableItems)
  DurableData_Detail96[,Year:=96]
  OwnsDurableItems96[,Year:=96]
  
  #save(OwnsDurableItems,file=paste0(Settings$HEISProcessedPath,"Y",year,
  #                                  "OwnsDurableItems.rda"))

  year<-97
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
  
  DurableData_Detail <- merge(TC,DurableItems,by="Code",all = TRUE)
  DurableData_Detail <- DurableData_Detail[!is.na(HHID)]
  DurableData_Detail[is.na(Durable_Exp), Durable_Exp:=0]
  DurableData_Detail[is.na(Durable_Sale), Durable_Sale:=0]
  
  
  #save(DurableData_Detail, file = paste0(Settings$HEISProcessedPath,"Y",year,
  #                                      "DurableData_Detail.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  itemslist <- c("cellphone","washer","dishwasher","car",
                 "oven","tvbw","tvcr","refrigerator","frez_refrig","freezer",
                 "computer","cooler_gas","cooler_gas_movable")
  
  for(item in itemslist)
    if(!(item %in% names(HHHouseProperties))){
      HHHouseProperties[,(item):=FALSE]
    }
  
  OwnsDurableItems <- HHHouseProperties[,.(HHID,
                                           Cellphone=cellphone*1,
                                           Washer=(washer|dishwasher)*1,
                                           Car_Repair=car*1,
                                           Car_Tire=car*1,
                                           Oven=oven*1,
                                           TV=(tvbw |tvcr)*1,
                                           Refrigrator=(refrigerator|frez_refrig|freezer)*1,
                                           Car=car*1,
                                           Computer=computer*1,
                                           AC=(cooler_gas |cooler_gas_movable)*1,
                                           Car_Motor=car*1)]
  
  DurableData_Detail97<-copy((DurableData_Detail))
  OwnsDurableItems97<-copy(OwnsDurableItems)
  DurableData_Detail97[,Year:=97]
  OwnsDurableItems97[,Year:=97]
  #save(OwnsDurableItems,file=paste0(Settings$HEISProcessedPath,"Y",year,
  #                                  "OwnsDurableItems.rda"))
  
  
  year<-98
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
  
  DurableData_Detail <- merge(TC,DurableItems,by="Code",all = TRUE)
  DurableData_Detail <- DurableData_Detail[!is.na(HHID)]
  DurableData_Detail[is.na(Durable_Exp), Durable_Exp:=0]
  DurableData_Detail[is.na(Durable_Sale), Durable_Sale:=0]
  
  
  #save(DurableData_Detail, file = paste0(Settings$HEISProcessedPath,"Y",year,
  #                                      "DurableData_Detail.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  itemslist <- c("cellphone","washer","dishwasher","car",
                 "oven","tvbw","tvcr","refrigerator","frez_refrig","freezer",
                 "computer","cooler_gas","cooler_gas_movable")
  
  for(item in itemslist)
    if(!(item %in% names(HHHouseProperties))){
      HHHouseProperties[,(item):=FALSE]
    }
  
  OwnsDurableItems <- HHHouseProperties[,.(HHID,
                                           Cellphone=cellphone*1,
                                           Washer=(washer|dishwasher)*1,
                                           Car_Repair=car*1,
                                           Car_Tire=car*1,
                                           Oven=oven*1,
                                           TV=(tvbw |tvcr)*1,
                                           Refrigrator=(refrigerator|frez_refrig|freezer)*1,
                                           Car=car*1,
                                           Computer=computer*1,
                                           AC=(cooler_gas |cooler_gas_movable)*1,
                                           Car_Motor=car*1)]
  
  DurableData_Detail98<-copy((DurableData_Detail))
  OwnsDurableItems98<-copy(OwnsDurableItems)
  
  DurableData_Detail98[,Year:=98]
  OwnsDurableItems98[,Year:=98]
  #save(OwnsDurableItems,file=paste0(Settings$HEISProcessedPath,"Y",year,
  #                                  "OwnsDurableItems.rda"))
  
  
  DurableData_Detail<-rbind(DurableData_Detail98,DurableData_Detail97)
  DurableData_Detail<-rbind(DurableData_Detail,DurableData_Detail96)
  
  OwnsDurableItems<-rbind(OwnsDurableItems98,OwnsDurableItems97)
  OwnsDurableItems<-rbind(OwnsDurableItems,OwnsDurableItems96)
  
  save(DurableData_Detail, file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableData_Detail_Ostan.rda"))
  save(OwnsDurableItems,file=paste0(Settings$HEISProcessedPath,"Y",year,"OwnsDurableItems_Ostan.rda"))
  
  
endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")
