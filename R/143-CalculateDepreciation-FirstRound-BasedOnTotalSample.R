# 143-CalculateDepreciation based on total sample averages
# 
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Durable Items Depretiation =======================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

source("142-Calculate_OwnedDurableItemsDepreciation_FunctionDef.R")
DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItemsDepr))

year <- 98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_Detail.rda"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "OwnsDurableItems.rda"))

  
  OwnedDurableItemsDepreciation <- 
    Calculate_OwnedDurableItemsDepreciation(
      DurableData_ExpDetail = DurableData_Detail,
      DurableItems_OwningDetail = OwnsDurableItems,
      by = "Item",
      Decile = NULL,
      DurableItems = DurableItems)
  
  
  save(OwnedDurableItemsDepreciation,
       file=paste0(Settings$HEISProcessedPath,"Y",
                   year,"OwnedDurableItemsDepreciation.rda"))
}

endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")