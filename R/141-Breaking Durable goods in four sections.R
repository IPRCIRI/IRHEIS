#141-Breaking Durable goods in four section
# 
# Copyright © 2020:  Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Breaking Durable goods in four section =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
#library(ggplot2)
#library(spatstat)
#library(dplyr)
###########################################################
######Copy code files from TotalDurableDetails folder######
#####################to Data Processed folder##############
#########################################################
DurableGroups <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_DurableGroups))

mst <- min(DurableGroups$StartYear)

for(year in (max(Settings$startyear,mst):Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"DurableDataCodes.rda"))

  g1 <- DurableGroups[year >= StartYear & year <= EndYear && Group==1]$Code
  g2 <- DurableGroups[year >= StartYear & year <= EndYear & Group==2]$Code
  g3 <- DurableGroups[year >= StartYear & year <= EndYear & Group==3]$Code
  g4 <- DurableGroups[year >= StartYear & year <= EndYear & Group==4]$Code
  
  D1 <- DurableDataCodes[Code %in% g1, .(Add_to_NonDurable = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D2 <- DurableDataCodes[Code %in% g2, .(Durable_Dep = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D3 <- DurableDataCodes[Code %in% g3, .(Durable_NoDep = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D4 <- DurableDataCodes[Code %in% g4, .(Durable_Emergency = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  
 
  Durable_Detail <- merge(D1,D2,all=TRUE)
  Durable_Detail <- merge(Durable_Detail,D3,all=TRUE)
  Durable_Detail <- merge(Durable_Detail,D4,all = TRUE)
  
  Durable_Detail[is.na(Durable_Detail)] <- 0

    gunion <- union(union(g1,g2),union(g3,g4))

    Dx <- DurableDataCodes[! (Code %in% gunion),]
    if(nrow(Dx)>0) print(table(Dx[,Code]))

  save(Durable_Detail, file=paste0(Settings$HEISProcessedPath,"Y",year,"Durable_Detail.rda"))

}

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")