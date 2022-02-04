# 143-CalculateDepreciation based on total sample averages
# 
# Copyright © 2020-2022:  Arin Shahbazian & Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Durable Items Depretiation =======================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

source("000-FunctionDefs.R")
DurableItemsDepr <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItemsDepr))
DurableGroups <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_DurableGroups))


#year <- 83
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_NetDetail.rda"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "OwnsDurableItems.rda"))

  g2 <- DurableGroups[year >= StartYear & year <= EndYear & Group==2]$Code
  #setdiff(g2,DurableItems$)
  OwnedDurableItemsDepreciation <- 
    Calculate_OwnedDurableItemsDepreciation(
      DurableData_ExpDetail = DD,
      DurableItems_OwningDetail = OwnsDurableItems,
      by = "Item",
      Decile = NULL,
      DurableItems = DurableItemsDepr,
      g2 = g2)
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durable_4Groups.rda"))
  A <- merge(Durable_4Groups,OwnedDurableItemsDepreciation,by="HHID")
  cat(A[,.(mean(OwnedDurableItemsDepreciation)/mean(Durable_Dep))]$V1)
  save(OwnedDurableItemsDepreciation,
       file=paste0(Settings$HEISProcessedPath,"Y",
                   year,"OwnedDurableItemsDepreciation.rda"))
}

endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")