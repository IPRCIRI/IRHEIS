# 21-HHMahis.R
# Builds the Mahi expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- MahiTables[Year==year]
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
    TF[,Grams:=as.numeric(Grams)]
    TF[,Kilos:=as.numeric(Kilos)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$MahiGram<-TF$Kilos*1000+TF$Grams
  TF$MahiGram<- TF$MahiGram/30
  MahiData <- TF[,lapply(.SD,sum),by=HHID]
  save(MahiData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Mahis.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
