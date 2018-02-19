# 31-PubWage.R
# Builds the Wages data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPubWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PubWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_PubWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  pubwt <- PubWageTable[Year==year]
  tab <- pubwt$Table
   if(is.na(tab))
      next
   UTpubW <- Tables[[paste0("U",year,tab)]]
   RTpubW <- Tables[[paste0("R",year,tab)]]
   TpubW <- rbind(UTpubW,RTpubW,fill=TRUE)
   print(names(TpubW))
   for(n in names(TpubW)){
    x <- which(pubwt==n)
   if(length(x)>0)
      setnames(TpubW,n,names(pubwt)[x])
   }
   pcols <- intersect(names(TpubW),c("HHID","section","net_income_pub"))
   #pcols <- intersect(names(TpubW),c("HHID","indiv","shaghel","shoghl","current_shoghl","faaliat","section","hour_in_day","day_in_week","gross_income_m","gross_income_y","mostameri_m","mostameri_y","gheyremostameri_m","gheyremostameri_y","net_income_m","net_income_y"))
    TpubW <- TpubW[,pcols,with=FALSE]
    
    if(year %in% 69:76){
      TpubW <- TpubW[ section ==1 ] 
    } else if(year >= 77){
      TpubW <- TpubW[ section ==1 ] 
    } 
    if(year >= 66){
      TpubW[,HHID:=as.numeric(HHID)]
    }
    
    if(year >= 86){
      TpubW[,net_income_pub:=as.numeric(net_income_pub)]
    }
  
   TpubW[is.na(TpubW)] <- 0
   PubWageData <- TpubW[,lapply(.SD,sum),by=HHID]
    save(PubWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
  }
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
