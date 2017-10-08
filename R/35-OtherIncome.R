# 35-OtherIncome.R
# Builds the OtherIncome data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHOtherIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


OtherWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_OtherWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  Otherwt <- OtherWageTable[Year==year]
  tab <- Otherwt$Table
  if(is.na(tab))
    next
  UTOtherW <- Tables[[paste0("U",year,tab)]]
  RTOtherW <- Tables[[paste0("R",year,tab)]]
  TOtherW <- rbind(UTOtherW,RTOtherW,fill=TRUE)
  for(n in names(TOtherW)){
    x <- which(Otherwt==n)
    if(length(x)>0)
      setnames(TOtherW,n,names(Otherwt)[x])
  }
  pcols <- intersect(names(TOtherW),c("HHID","Code","indiv","retirement","rent","interest","aid","homemade","intra"))
  TOtherW <- TOtherW[,pcols,with=FALSE]
  
  #if(year %in% 63:68){
 #   TOtherW <- TOtherW[ section ==1 ] 
 # } else if(year %in% 77:94){
  #  TOtherW <- TOtherW[ section ==1 ] 
  #} 
  
  TOtherW[is.na(TOtherW)] <- 0
   OtherWageData <- TOtherW[,lapply(.SD,sum),by=HHID]
   save(OtherWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"OtherWage.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
