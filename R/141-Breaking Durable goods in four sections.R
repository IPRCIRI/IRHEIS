#141-Breaking Durable goods in four section
# 
# Copyright © 2020:  Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Breaking Durable goods in four groups =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)

DurableGroups <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_DurableGroups))

mst <- min(DurableGroups$StartYear)

for(year in (max(Settings$startyear,mst):Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"DurableData_Detail.rda"))
  
  g1 <- DurableGroups[year >= StartYear & year <= EndYear & Group==1]$Code
  g2 <- DurableGroups[year >= StartYear & year <= EndYear & Group==2]$Code
  g3 <- DurableGroups[year >= StartYear & year <= EndYear & Group==3]$Code
  g4 <- DurableGroups[year >= StartYear & year <= EndYear & Group==4]$Code
  
  D1 <- DurableData_Detail[Code %in% g1, .(Add_to_NonDurable = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D2 <- DurableData_Detail[Code %in% g2, .(Durable_Dep = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D3 <- DurableData_Detail[Code %in% g3, .(Durable_NoDep = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  D4 <- DurableData_Detail[Code %in% g4, .(Durable_Emergency = sum(Durable_Exp,na.rm = TRUE)),by=HHID]
  
  Durable_4Groups <- merge(D1,D2,all=TRUE)
  Durable_4Groups <- merge(Durable_4Groups,D3,all=TRUE)
  Durable_4Groups <- merge(Durable_4Groups,D4,all = TRUE)
  
  Durable_4Groups[is.na(Durable_4Groups)] <- 0
  
  gunion <- union(union(g1,g2),union(g3,g4))
  
  Dx <- DurableData_Detail[! (Code %in% gunion),]
  if(nrow(Dx)>0) print(table(Dx[,Code]))
  
  save(Durable_4Groups, file=paste0(Settings$HEISProcessedPath,"Y",year,"Durable_4Groups.rda"))
  
}

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")