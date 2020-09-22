# 142-CalculateDepreciation based on total sample averages
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

DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItemsDepr))

year <- 98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_Detail.rda"))
  DurableValues <- DurableData_Detail[,.(Value=mean(Durable_Exp)),by=Item]
  DurableDepr <- merge(DurableValues,DurableItems,by="Item")
  DurableDepr[,DepreciationValue:=Value*Depri/100]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "OwnsDurableItems.rda"))
  Ownsm <- melt(data = OwnsDurableItems,id.vars = "HHID",
                measure.vars = names(OwnsDurableItems)[-1],
                variable.name = "Item",value.name = "Owns")
  Ownsm <- Ownsm[Owns==1]
  
  D <- merge(Ownsm,DurableDepr,by="Item")
  
  OwnedDurableItemsDepreciation <- D[,.(OwnedDurableItemsDepreciation=sum(.SD$DepreciationValue)),by=HHID]
  
  save(OwnedDurableItemsDepreciation,
       file=paste0(Settings$HEISProcessedPath,"Y",
                   year,"OwnedDurableItemsDepreciation.rda"))
}

endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")