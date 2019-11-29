# 141-Groups Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ NonFood Expenditures =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

cat("\n\n================ Section2:HHCigar =====================================\n")

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


cat("\n\n================ Section3:HHCloth =====================================\n")

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

cat("\n\n================ Section4:HouseandEnergy =====================================\n")

EnergyTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_HouseandEnergy))

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
  pcols <- intersect(names(TE),c("HHID","Code","HouseandEnergy_Exp"))
  TE <- TE[,pcols,with=FALSE]
  #TE <- TE[Code %in% ty$StartCode:ty$EndCode]
 # if(year %in% 84:94){
 #   TE[,HouseandEnergy_Exp:=as.numeric(HouseandEnergy_Exp)]
 # }
  #TL[,HouseandEnergy_Exp:=as.numeric(HouseandEnergy_Exp)]
  TE[,Code:=NULL]
  
  
  TE[,HouseandEnergy_Exp:=as.integer(HouseandEnergy_Exp)]
  TE[is.na(TE)] <- 0
  HouseandEnergyData <- TE[,lapply(.SD,sum),by=HHID]
  print(nrow(HouseandEnergyData))
  save(HouseandEnergyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"HouseandEnergys.rda"))
}

cat("\n\n================ Section4:HHHouse =====================================\n")

HouseTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_House))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- HouseTables[Year==year][,1:7]
  tab <- ty$Table
  
  UTL <- Tables[[paste0("U",year,tab)]]
  RTL <- Tables[[paste0("R",year,tab)]]
  TL <- rbind(UTL,RTL)
  for(n in names(TL)){
    x <- which(ty==n)
    if(length(x)>0)
      setnames(TL,n,names(ty)[x])
  }
  pcols <- intersect(names(TL),c("HHID","Code","ServiceExp"))
  TL <- TL[,pcols,with=FALSE]
  TL <- TL[Code %in% ty$StartCode:ty$EndCode]
  TL[,ServiceExp:=as.numeric(ServiceExp)]
  
  TL[,Code:=NULL]
  
  
  TL[is.na(TL)] <- 0
  
  HouseData <- TL[,lapply(.SD,sum),by=HHID]
  

  # cat("\n",year,",",mean(HouseData$ServiceExp,na.rm = TRUE))
  ty <- HouseTables[Year==year]
  rt <- Tables[[paste0("R",year,ty$HATable)]]
  ut <- Tables[[paste0("U",year,ty$HATable)]]
  
  if(year <= 68){
    rt$New <- NA
    setnames(rt,"New",ty$HACode)
  }
  ns <- c(ty$HAHHID,ty$HRCode,ty$HACode)
  
  TRA <- rbind( rt[,ns,with=FALSE], ut[,ns,with=FALSE])
  
  setnames(TRA,ty$HAHHID,"HHID")
  setnames(TRA,ty$HRCode,"Rooms")
  setnames(TRA,ty$HACode,"Area")
  
  TRA[Area==0,Area:=NA]
  
  HouseData <- merge(HouseData,TRA,by = "HHID", all = TRUE)
  HouseData$MetrPrice <-HouseData$ServiceExp/HouseData$Area
  save(HouseData, file = paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
  # cat(summary(HouseData[,ServiceExp/Area]))
  cat(HouseData[,mean(MetrPrice)],"\n")
  cat(HouseData[,median(MetrPrice)],"\n")
  cat(HouseData[,mean(ServiceExp)],"\n")
  cat(HouseData[,median(ServiceExp)],"\n")
}

cat("\n\n================ Section5:HHFurniture =====================================\n")

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


cat("\n\n================ Section6-1:HHHygiene =====================================\n")

HygieneTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hygiene))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- HygieneTables[Year==year]
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
  pcols <- intersect(names(TM),c("HHID","Code","Hygiene_Exp"))
  TM <- TM[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:96){
    TM[,Hygiene_Exp:=as.numeric(Hygiene_Exp)]
  }
  # TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  HygieneData <- TM[,lapply(.SD,sum),by=HHID]
  save(HygieneData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Hygienes.rda"))
}

cat("\n\n================ Section6-2:HHMedical =====================================\n")

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
  if(year %in% 84:96){
    TM[,Medical_Exp:=as.numeric(Medical_Exp)]
  }
  # TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  MedicalData <- TM[,lapply(.SD,sum),by=HHID]
  save(MedicalData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Medicals.rda"))
}


cat("\n\n================ Section7:HHTransportation =====================================\n")

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

cat("\n\n================ Section8:HHCommunication =====================================\n")

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

cat("\n\n================ Section9:HHAmusement =====================================\n")
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



cat("\n\n================ Section10:HHEducationnExp =====================================\n")


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
  pcols <- intersect(names(TE),c("HHID","Code","Education_Exp"))
  TE <- TE[,pcols,with=FALSE]
  #if(year %in% 89:94){
  #TE[,Code:=as.numeric(Code)]
  #}
  #if(year %in% 84:94){
  # TF[,Kilos:=as.numeric(Kilos)]
  TE[,Education_Exp:=as.numeric(Education_Exp)]
  TE <- TE[Code %in% et$StartCode:et$EndCode]
  TE$Education_Exp<-TE$Education_Exp/12
  TE[,Code:=NULL]
  TE[is.na(TE)] <- 0
  EducData <- TE[,lapply(.SD,sum),by=HHID]
  save(EducData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Education.rda"))
}

cat("\n\n================ Section11:HHHotelRestaurant =====================================\n")

HotelRestaurantTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_HotelRestaurant))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- HotelRestaurantTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","HotelRestaurant_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,HotelRestaurant_Exp:=as.numeric(HotelRestaurant_Exp)]
  }
  TC[is.na(TC)] <- 0
  
  HotelRestaurantData <- TC[,lapply(.SD,sum),by=HHID]
  HotelRestaurantData[,Code:=NULL]
  save(HotelRestaurantData, file = paste0(Settings$HEISProcessedPath,"Y",year,"HotelRestaurants.rda"))
  
  TC<-TC[Code %in% 111111:111146]
  ResturantData <- TC[,lapply(.SD,sum),by=HHID]
  ResturantData[,Code:=NULL]
  names(ResturantData)[2]<-paste("Resturant_Exp")
  save(ResturantData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Resturants.rda"))
}

cat("\n\n================ Section12:HHOther =====================================\n")

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
  pcols <- intersect(names(TC),c("HHID","Code","Durable_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 84:96){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)]
  }
  TC$Durable_Exp<-TC$Durable_Exp/12
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  DurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(DurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
}


cat("\n\n================ Section14:HHInvestment =====================================\n")

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


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
