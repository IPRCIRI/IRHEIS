# 21-HHPanirs.R
# Builds the Panir expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- PanirTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:94){
    TF[,PanirExpenditure:=as.numeric(PanirExpenditure)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$PanirGram<-TF$Kilos*1000+TF$Grams
  TF$PanirGram<- TF$PanirGram/30
  PanirData <- TF[,lapply(.SD,sum),by=HHID]
  save(PanirData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Panirs.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
