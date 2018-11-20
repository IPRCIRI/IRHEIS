# 22-Groups Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Groups Expenditures =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ HHAmusement =====================================\n")
AmusementTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Amusement))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- AmusementTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Amusement_Exp"))
  TC <- TC[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:94){
    TC[,Amusement_Exp:=as.numeric(Amusement_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  AmusementData <- TC[,lapply(.SD,sum),by=HHID]
  save(AmusementData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Amusements.rda"))
}

cat("\n\n================ HHBehdasht =====================================\n")

BehdashtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Behdasht))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- BehdashtTables[Year==year]
  tab <- mt$Table
  if(is.na(tab))
    next
  UTM <- Tables[[paste0("U",year,tab)]]
  RTM <- Tables[[paste0("R",year,tab)]]
  TM <- rbind(UTM,RTM)
  for(n in names(TM)){
    x <- which(mt==n)
    if(length(x)>0)
      setnames(TM,n,names(mt)[x])
  }
  pcols <- intersect(names(TM),c("HHID","Code","Behdasht_Exp"))
  TM <- TM[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:95){
    TM[,Behdasht_Exp:=as.numeric(Behdasht_Exp)]
  }
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  BehdashtData <- TM[,lapply(.SD,sum),by=HHID]
  save(BehdashtData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Behdashts.rda"))
}

cat("\n\n================ HHCigar =====================================\n")

CigarTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Cigar))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- CigarTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Cigar_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  
  if(year %in% 84:94){
    TC[,Cigar_Exp:=as.numeric(Cigar_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  CigarData <- TC[,lapply(.SD,sum),by=HHID]
  save(CigarData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Cigars.rda"))
}


cat("\n\n================ HHCloth =====================================\n")

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
  pcols <- intersect(names(TC),c("HHID","Code","Cloth_Exp"))
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

cat("\n\n================ HHCommunication =====================================\n")

CommunicationTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Communication))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- CommunicationTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Communication_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Communication_Exp:=as.numeric(Communication_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  CommunicationData <- TC[,lapply(.SD,sum),by=HHID]
  save(CommunicationData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Communications.rda"))
}

cat("\n\n================ HHDurable =====================================\n")
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
  pcols <- intersect(names(TC),c("HHID","Code","Durable_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 84:95){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)]
  }
  TC$Durable_Exp<-TC$Durable_Exp/12
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  DurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(DurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
}

cat("\n\n================ HHEducationnExp =====================================\n")


EducationTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Education))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  et <- EducationTables[Year==year]
  tab <- et$Table
  if(is.na(tab))
    next
  UTE <- Tables[[paste0("U",year,tab)]]
  RTE <- Tables[[paste0("R",year,tab)]]
  TE <- rbind(UTE,RTE)
  for(n in names(TE)){
    x <- which(et==n)
    if(length(x)>0)
      setnames(TE,n,names(et)[x])
  }
  pcols <- intersect(names(TE),c("HHID","Code","EducExpenditure"))
  TE <- TE[,pcols,with=FALSE]
  #if(year %in% 89:94){
  #TE[,Code:=as.numeric(Code)]
  #}
  #if(year %in% 84:94){
  # TF[,Kilos:=as.numeric(Kilos)]
  TE[,EducExpenditure:=as.numeric(EducExpenditure)]
  TE <- TE[Code %in% et$StartCode:et$EndCode]
  TE$EducExpenditure<-TE$EducExpenditure/12
  TE[,Code:=NULL]
  TE[is.na(TE)] <- 0
  EducData <- TE[,lapply(.SD,sum),by=HHID]
  save(EducData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Education.rda"))
}

cat("\n\n================ Energy =====================================\n")

EnergyTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Energy))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- EnergyTables[Year==year]
  tab <- ty$Table
  
  UTE <- Tables[[paste0("U",year,tab)]]
  RTE <- Tables[[paste0("R",year,tab)]]
  TE <- rbind(UTE,RTE)
  for(n in names(TE)){
    x <- which(ty==n)
    if(length(x)>0)
      setnames(TE,n,names(ty)[x])
  }
  pcols <- intersect(names(TE),c("HHID","Code","Energy_Exp"))
  TE <- TE[,pcols,with=FALSE]
  TE <- TE[Code %in% ty$StartCode:ty$EndCode]
  if(year %in% 84:94){
    TE[,Energy_Exp:=as.numeric(Energy_Exp)]
  }
  #TL[,Energy_Exp:=as.numeric(Energy_Exp)]
  TE[,Code:=NULL]
  
  
  TE[is.na(TE)] <- 0
  
  EnergyData <- TE[,lapply(.SD,sum),by=HHID]
   save(EnergyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Energy.rda"))
}

cat("\n\n================ HHFurniture =====================================\n")

FurnitureTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Furniture))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- FurnitureTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Furniture_Exp"))
  TC <- TC[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:94){
    TC[,Furniture_Exp:=as.numeric(Furniture_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  FurnitureData <- TC[,lapply(.SD,sum),by=HHID]
  save(FurnitureData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Furnitures.rda"))
}

cat("\n\n================ HHHotel =====================================\n")

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
  TC[is.na(TC)] <- 0
  
  HotelData <- TC[,lapply(.SD,sum),by=HHID]
  HotelData[,Code:=NULL]
  save(HotelData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Hotels.rda"))
  
  TC<-TC[Code %in% 111111:111146]
  ResturantData <- TC[,lapply(.SD,sum),by=HHID]
  ResturantData[,Code:=NULL]
  names(ResturantData)[2]<-paste("Resturant_Exp")
  save(ResturantData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Resturants.rda"))
}

cat("\n\n================ HHInvestment =====================================\n")

InvestmentTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Investment))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- InvestmentTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Investment_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 84:94){
    TC[,Investment_Exp:=as.numeric(Investment_Exp)]
  }
  TC$Investment_Exp<-TC$Investment_Exp/12
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  InvestmentData <- TC[,lapply(.SD,sum),by=HHID]
  save(InvestmentData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Investments.rda"))
}

cat("\n\n================ HHMedical =====================================\n")

MedicalTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Medical))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- MedicalTables[Year==year]
  tab <- mt$Table
  if(is.na(tab))
    next
  UTM <- Tables[[paste0("U",year,tab)]]
  RTM <- Tables[[paste0("R",year,tab)]]
  TM <- rbind(UTM,RTM)
  for(n in names(TM)){
    x <- which(mt==n)
    if(length(x)>0)
      setnames(TM,n,names(mt)[x])
  }
  pcols <- intersect(names(TM),c("HHID","Code","Medical_Exp"))
  TM <- TM[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:95){
    TM[,Medical_Exp:=as.numeric(Medical_Exp)]
  }
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  MedicalData <- TM[,lapply(.SD,sum),by=HHID]
  save(MedicalData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Medicals.rda"))
}

cat("\n\n================ HHOther =====================================\n")

OtherTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Other))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- OtherTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Other_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Other_Exp:=as.numeric(Other_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  OtherData <- TC[,lapply(.SD,sum),by=HHID]
  save(OtherData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Others.rda"))
}

cat("\n\n================ HHTransportation =====================================\n")

TransportationTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Transportation))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- TransportationTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Transportation_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Transportation_Exp:=as.numeric(Transportation_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  TransportationData <- TC[,lapply(.SD,sum),by=HHID]
  save(TransportationData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Transportations.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
