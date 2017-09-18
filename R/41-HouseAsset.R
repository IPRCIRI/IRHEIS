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
  pcols <- intersect(names(TL),c("HHID","Code","ServiceExp"))
  TL <- TL[,pcols,with=FALSE]
  TL <- TL[Code %in% ty$StartCode:ty$EndCode]
  if(year %in% 84:94){
    TL[,ServiceExp:=as.numeric(ServiceExp)]
  }
  #TL[,ServiceExp:=as.numeric(ServiceExp)]
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
  
  save(HouseData, file = paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
