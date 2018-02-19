# 32-Private Wage.R
# Builds the Private Wages data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPrvWage =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PrvWageTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_PrvWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  prvwt <- PrvWageTable[Year==year]
  tab <- prvwt$Table
  if(is.na(tab))
    next
  UTPrvW <- Tables[[paste0("U",year,tab)]]
  RTPrvW <- Tables[[paste0("R",year,tab)]]
  TPrvW <- rbind(UTPrvW,RTPrvW,fill=TRUE)
  for(n in names(TPrvW)){
    x <- which(prvwt==n)
    if(length(x)>0)
      setnames(TPrvW,n,names(prvwt)[x])
  }
  pcols <- intersect(names(TPrvW),c("HHID","section","net_income_prv"))
  TPrvW <- TPrvW[,pcols,with=FALSE]
  
  if(year %in% 69:76){
    TPrvW <- TPrvW[ section ==2 ] 
  } else if(year %in% 77:94){
    TPrvW <- TPrvW[ section ==3 ] 
  } 
  if(year %in% 74:94){
    TPrvW[,HHID:=as.numeric(HHID)]
  }
  
  if(year %in% 86:94){
    TPrvW[,net_income_prv:=as.numeric(net_income_prv)]
  }
  
  TPrvW[is.na(TPrvW)] <- 0
  
  A <- TPrvW[,.(net_income_prv=sum(net_income_prv)),by=HHID]
  if("sector" %in% names(TPrvW)){
    B <- TPrvW[TPrvW[,.I[net_income_prv==max(net_income_prv)],by=HHID][,V1]][,.(HHID,prvsection=section)]
  }else{
    B <- TPrvW[,.(net_income_prv,prvsection=NA),by=HHID][,.(HHID,prvsection=NA)]
  }
  PrvWageData <- merge(A,B,by="HHID")
  
  
   save(PrvWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
