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


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  if(year >86 & year < 92 ){ 
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"ShCode.rda"))
  }
  
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
  
  if(year >86 & year < 92 ){ 
 HHBase<-merge(HHBase,ShCode,by="HHID",all.x = TRUE)
    }
  
  HHBase <- HHBase[!is.na(HHID)]
  HHBase[,ProvinceCode:=as.integer(str_sub(HHIDs,2,3))]
  
  if(year <87 | year > 91 ){
  HHBase[,CountyCode:=as.integer(str_sub(HHIDs,2,5))]
  }
  
  if(year >86 & year < 92 ){ 
    HHBase[,CountyCode:=as.integer(SHCode)]
  }
  
  HHBase[,Year:=year]
  HHBase <- HHBase[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode)]
  
  HHBase[,NewArea:=ProvinceCode]
  HHBase[Region=="Urban" & 
           CountyCode %in% c(2301,303,603,707,
                             916,1002,3001,502,2202),
         NewArea:=CountyCode]
  
  #Tehran-Alborz
  if(year >76 & year < 92 ){ 
    HHBase[ Region=="Urban" & CountyCode %in% c(2305),
    NewArea:=3001] 
              }
  
  if(year >76 & year < 92 ){ 
    HHBase[CountyCode %in% c(2308,2315,2309),
           NewArea:=30]
              }
  
  #Khorasan
  if(year >76 & year < 87 ){ 
    HHBase[Region=="Urban" & CountyCode %in% c(916),
           NewArea:=916]
                           }
  
  if(year >76 & year < 87 ){ 
    HHBase[CountyCode %in% c(901,902,909,924,925),
           NewArea:=28]
                           }
  
  if(year >76 & year < 87 ){ 
      HHBase[CountyCode %in% c(903,911,912,921),
             NewArea:=29]
  }
  
  #Ghazvin
  if(year >76 & year < 82 ){ 
    HHBase[CountyCode %in% c(2311),
           NewArea:=26]
  }
  
  #Golestan
  if(year >76 & year < 82 ){ 
    HHBase[CountyCode %in% c(212,203,209,211,213,217),
           NewArea:=27]
  }
  
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))

 # rm(HHBase)
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)