# 40-Houses
# 
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Houses =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

HouseTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_House))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- HouseTables[Year==year]
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
  TL <- TL[Code %in% ty$OwnedHouseServiceStartCode:ty$OwnedHouseServiceEndCode]
  TL[,House_Exp:=as.numeric(House_Exp)]

  TL[,Code:=NULL]


  TL[is.na(TL)] <- 0
  
  HouseData <- TL[,lapply(.SD,sum),by=HHID]
  
  #   if("HouseValue" %in% names(HousesData)){
  #     cat("\n",year,",",
  # #        mean(HousesData$ServiceFee/HousesData$HouseValue,na.rm = TRUE))
  #         mean(HousesData$ServiceFee,na.rm = TRUE), mean(HousesData$HouseValue,na.rm = TRUE))
  # 
  #   }
  #   if("HHouseValue" %in% names(HousesData)){
  #     cat(",",
  # #        mean(HousesData$HServiceFee/HousesData$HHouseValue,na.rm = TRUE))
  #         mean(HousesData$HServiceFee,na.rm = TRUE), mean(HousesData$HHouseValue,na.rm = TRUE))
  #   }
  
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

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
