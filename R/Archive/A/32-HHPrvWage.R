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
  pcols <- intersect(names(TPrvW),c("HHID","indiv","shaghel","shoghl","current_shoghl","faaliat","section","hour_in_day","day_in_week","gross_income_m","gross_income_y","mostameri_m","mostameri_y","gheyremostameri_m","gheyremostameri_y","net_income_m","net_income_y"))
  TPrvW <- TPrvW[,pcols,with=FALSE]
  
  if(year %in% 69:76){
    TPrvW <- TPrvW[ section ==2 ] 
  } else if(year %in% 77:94){
    TPrvW <- TPrvW[ section ==3 ] 
  } 
  
  TPrvW[is.na(TPrvW)] <- 0
 # PrvWageData <- TPrvW[,lapply(.SD,sum),by=HHID]
  # save(PrvWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
