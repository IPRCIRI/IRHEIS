# 31-Wage.R
# Builds the Wages data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PubWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Wage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  wt <- PubWageTable[Year==year]
  tab <- wt$Table
   if(is.na(tab))
      next
   UTW <- Tables[[paste0("U",year,tab)]]
   RTW <- Tables[[paste0("R",year,tab)]]
   TW <- rbind(UTW,RTW,fill=TRUE)
   for(n in names(TW)){
    x <- which(wt==n)
   if(length(x)>0)
      setnames(TW,n,names(wt)[x])
    }
   pcols <- intersect(names(TW),c("HHID","indiv","shaghel","shoghl","faaliat","section","hour_in_day","day_in_week","gross_income_m","gross_income_y","mostameri_m","mostameri_y","gheyremostameri_m","gheyremostameri_y","net_income_m","net_income_y"))
    TW <- TW[,pcols,with=FALSE]
    
 
   TW[is.na(TW)] <- 0
   WageData <- TW[,lapply(.SD,sum),by=HHID]
   # save(WageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Wages.rda"))
  }
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
