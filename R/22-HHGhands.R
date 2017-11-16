# 21-HHGhands.R
# Builds the Ghand expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGhands =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


GhandTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- GhandTables[Year==year]
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
    TF[,GhandExpenditure:=as.numeric(GhandExpenditure)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$GhandGram<-TF$Kilos*1000+TF$Grams
  TF$GhandGram<- TF$GhandGram/30
  GhandData <- TF[,lapply(.SD,sum),by=HHID]
  save(GhandData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Ghands.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
