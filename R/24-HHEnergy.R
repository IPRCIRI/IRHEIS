# 24-Energy.R
# 
#
# Copyright © 2016:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Energy =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

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
  
  #   if("EnergyValue" %in% names(EnergysData)){
  #     cat("\n",year,",",
  # #        mean(EnergysData$ServiceFee/EnergysData$EnergyValue,na.rm = TRUE))
  #         mean(EnergysData$ServiceFee,na.rm = TRUE), mean(EnergysData$EnergyValue,na.rm = TRUE))
  # 
  #   }
  #   if("HEnergyValue" %in% names(EnergysData)){
  #     cat(",",
  # #        mean(EnergysData$HServiceFee/EnergysData$HEnergyValue,na.rm = TRUE))
  #         mean(EnergysData$HServiceFee,na.rm = TRUE), mean(EnergysData$HEnergyValue,na.rm = TRUE))
  #   }
  
  # cat("\n",year,",",mean(EnergyData$ServiceExp,na.rm = TRUE))
  
  save(EnergyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Energy.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
