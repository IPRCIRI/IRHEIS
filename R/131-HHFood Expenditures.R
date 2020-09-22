# 131-HHFoods.R
# Builds the Food expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ HHFoods =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Food))

for (year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:", year, "\n"))
  load(file = paste0(Settings$HEISRawPath, "Y", year, "Raw.rda"))
  ft <- FoodTables[Year == year]
  tab <- ft$Table
  if (is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","FoodExpenditure","BuyingMethod"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  TF <- TF[,FoodExpenditure:=as.numeric(FoodExpenditure)]
  TF <- TF[,Code:=NULL]
  TF[is.na(TF)] <- 0

  TotalFoodExpData <- TF[,lapply(.SD,sum),by=HHID,.SDcols=c("FoodExpenditure")]
  save(TotalFoodExpData, file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
}
endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")