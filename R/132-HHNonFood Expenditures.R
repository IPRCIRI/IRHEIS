# 132-HHNonFood Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017-2022: Arin Shahbazian, Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ NonFood Expenditures =============================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

sections_names <- c("Cigar","Cloth","Communication","Energy","Furniture",
                    "Hygiene","Medical","Transportation","Communication",
                    "Amusement","Education","Hotel","Restaurant","Other",
                    "Investment")
# House has different code / durables are in another file
for(section in sections_names){
  section_sheet <- eval(parse(text = paste0("Settings$MDS_",section)))
  cat("\n\n================",section,"=====================================\n")
  SectionTables <- data.table(read_excel(Settings$MetaDataFilePath,
                                         sheet=section_sheet))
  
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
    
    if(tab=="P3S13"){
      TS[,(paste0(section,"_Exp")):=get(paste0(section,"_Exp"))/12]
      #cat("======*****========")
    }
    
    TS[,Code:=NULL]
    TS[is.na(TS)] <- 0
    
    eval(parse(text = paste0(section,"Data <- TS[,lapply(.SD,sum),by=HHID]")))
    eval(parse(text = paste0("save(",section,
                             "Data, file =paste0(Settings$HEISProcessedPath,\"Y\",year,\"",
                             section,"s.rda\"))")))
    eval(parse(text = paste0("cat(section,\":\",",section,
                             "Data[,mean(",section,"_Exp)])")))
  }
}


cat("\n\n================ Section4:HHHouse =====================================\n")
HouseTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_House))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))

  ty <- HouseTables[Year==year][,1:10]
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
  mcs <- ty$MainCodes
  mcs <- substr(mcs,1,nchar(mcs)-1)
  maincodes <- eval(parse(text=paste0("c(",mcs,")")))
  rmcs <- ty$RM
  rmcs <- substr(rmcs,1,nchar(rmcs)-1)
  rmcodes <- eval(parse(text=paste0("c(",rmcs,")")))
  TLx <- TL[Code %in% maincodes]
  TLx <- TLx[,.(House_Exp=sum(House_Exp)),by=.(HHID,Code)]
  TLx[Code %in% rmcodes,Code:=max(rmcodes)]
  TLx[,hhmincode:=min(Code),by=HHID]
  TLx <- TLx[Code==hhmincode]
  TLx <- TLx[,.(MainHouse_Exp=sum(House_Exp)),by=.(HHID)]
  
  TL[,Code:=NULL]
  TL[is.na(TL)] <- 0
  
  HouseData <- TL[,lapply(.SD,sum),by=HHID]

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "HHHouseProperties.rda"))
  HousePropData <- HHHouseProperties[,.(HHID,room,area)]
  HousePropData[area==0,area:=NA]
  HousePropData <- merge(HousePropData,TLx,by = "HHID", all = TRUE)
  HousePropData <- HousePropData[,MeterPrice:=MainHouse_Exp/area]

  HouseData <- merge(HouseData,HousePropData,by = "HHID", all = TRUE)
  HouseData <- HouseData[,.(HHID,House_Exp,MainHouse_Exp,MeterPrice)]
  save(HouseData, file=paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
}

endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")
