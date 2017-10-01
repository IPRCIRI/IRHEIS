# 32-HHMedical.R
# Builds the Medical expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMedical =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


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
  pcols <- intersect(names(TM),c("HHID","Code","BuyingMethod","Medical_Exp","Method"))
  TM <- TM[,pcols,with=FALSE]
  #TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:94){
    TM[,Medical_Exp:=as.numeric(Medical_Exp)]
  }
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  MedicalData <- TM[,lapply(.SD,sum),by=HHID]
  save(MedicalData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Medicals.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
