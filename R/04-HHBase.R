# 04-HHBase.R
# Builds the base data.table for households
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBase =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

# BigD <- data.table(HHID=NA,Region=NA,Year=NA,Quarter=NA,Month=NA,ProvinceCode=NA)[0]


for(year in (Settings$startyear:Settings$endyear))
{
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))

  
  if(year < 87){           # RxxData & UxxData tables are provided Since 1387
    RData <- Tables[[paste0("R",year,"P2")]][,1,with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"P2")]][,1,with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Region"))
    HHBase[,Year:=year]
    if(year==74)
      HHBase[,HHIDs:=formatC(HHID, width = 8, flag = "0")]
    else if(year<77)
      HHBase[,HHIDs:=formatC(HHID, width = 7, flag = "0")]
    else if(year %in% 77:86)
      HHBase[,HHIDs:=formatC(HHID, width = 9, flag = "0")]
    
    if(year < 77)
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,4,4))]
    else
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,6,6))]
    HHBase[,Month:=NA_integer_]
  }else{
    RData <- Tables[[paste0("R",year,"DATA")]][,c(1:2),with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"DATA")]][,c(1:2),with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Month","Region"))
    HHBase[,Month:=ifelse(Month==1,12,Month - 1)]
    HHBase[,Quarter:=(Month-1)%/%3+1]
    HHBase[,HHIDs:=as.character(HHID)]
  }
  HHBase <- HHBase[!is.na(HHID)]
  HHBase[,ProvinceCode:=as.integer(str_sub(HHIDs,2,3))]
  HHBase[,Year:=year]
  HHBase <- HHBase[,list(HHID,Region,Year,Quarter,Month,ProvinceCode)]
  
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))

  rm(HHBase)
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)