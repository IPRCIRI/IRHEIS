# 141-Groups Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017-2020: Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ NonFood Expenditures =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

sections_names <- c("Cigar","Cloth","Communication","Energy","Furniture",
                    "Hygiene","Medical","Transportation","Communication",
                    "Amusement","Education","Hotel","Restaurant","Other",
                    "Investment")
                # House and Durables have their own code
for(section in sections_names){
section_sheet <- eval(parse(text = paste0("Settings$MDS_",section)))
cat("\n\n================",section,"=====================================\n")
SectionTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=section_sheet))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  st <- SectionTables[Year==year]
  tab <- st$Table
  if(is.na(tab))
    next
  UTS <- Tables[[paste0("U",year,tab)]]
  RTS <- Tables[[paste0("R",year,tab)]]
  TS <- rbind(UTS,RTS)
  for(n in names(TS)){
    x <- which(st==n)
    if(length(x)>0)
      setnames(TS,n,names(st)[x])
  }
  pcols <- intersect(names(TS),c("HHID","Code",paste0(section,"_Exp")))
  TS <- TS[,pcols,with=FALSE]
  if(!is.na(st$StartCode)){
    TS <- TS[Code %in% st$StartCode:st$EndCode]
  }
  
  TS[,(paste0(section,"_Exp")):=as.numeric(get(paste0(section,"_Exp")))]

  TS[,Code:=NULL]
  TS[is.na(TS)] <- 0
  
  eval(parse(text = paste0(section,"Data <- TS[,lapply(.SD,sum),by=HHID]")))
  eval(parse(text = paste0("save(",section,"Data, file =paste0(Settings$HEISProcessedPath,\"Y\",year,\"",section,"s.rda\"))")))
  eval(parse(text = paste0("cat(section,\":\",",section,"Data[,mean(",section,"_Exp)])")))
}


}



cat("\n\n================ Section4:HHHouse =====================================\n")

HouseTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_House))
#p0<-0
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- HouseTables[Year==year][,1:8]
  tab <- ty$Table
  
  UTL <- Tables[[paste0("U",year,tab)]]
  RTL <- Tables[[paste0("R",year,tab)]]
  TL <- rbind(UTL,RTL)
  for(n in names(TL)){
    x <- which(ty==n)
    if(length(x)>0)
      setnames(TL,n,names(ty)[x])
  }
  pcols <- intersect(names(TL),c("HHID","Code","House_Exp"))
  TL <- TL[,pcols,with=FALSE]
  TL <- TL[Code %in% ty$StartCode:ty$EndCode]
  TL[,House_Exp:=as.numeric(House_Exp)]
  
  TL[,Code:=NULL]
  TL[is.na(TL)] <- 0
  
  HouseData <- TL[,lapply(.SD,sum),by=HHID]
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "HHHouseProperties.rda"))
  HousePropData <- HHHouseProperties[,.(HHID,room,area)]
  # cat("\n",year,",",mean(HouseData$ServiceExp,na.rm = TRUE))
  
  HouseData <- merge(HouseData,HousePropData,by = "HHID", all = TRUE)
  HouseData <- HouseData[,MeterPrice:=House_Exp/area]
  HouseData <- HouseData[,.(HHID,House_Exp,MeterPrice)]
  save(HouseData, file = paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
}



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
  
  if(year >= 84){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)/12]
   TC[,Durable_Sale:=as.numeric(Durable_Sale)/12]
  }
  
  DurableDataCodes<-TC
  save(DurableDataCodes, file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableDataCodes.rda"))
}

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")