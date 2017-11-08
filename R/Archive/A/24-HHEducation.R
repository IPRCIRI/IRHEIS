# 30-HHEducationnExp.R
# Builds the Educationn Expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHEducationnExp =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


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
  TE[,Code:=NULL]
  TE[is.na(TE)] <- 0
  EducData <- TE[,lapply(.SD,sum),by=HHID]
  save(EducData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Education.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
