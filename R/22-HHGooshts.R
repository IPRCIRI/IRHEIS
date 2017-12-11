# 21-HHGooshts.R
# Builds the Goosht expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- GooshtTables[Year==year]
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
    TF[,Kilos:=as.numeric(Kilos)]
    TF[,Grams:=as.numeric(Grams)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$GooshtGram<-TF$Kilos*1000+TF$Grams
  TF$GooshtGram<- TF$GooshtGram/30
  GooshtData <- TF[,lapply(.SD,sum),by=HHID]
  save(GooshtData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Gooshts.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
