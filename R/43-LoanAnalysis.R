# 43-Loans Analysis
# 
#
# Copyright © 2018: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Loans Analysis =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(survey)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda")))
    next
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  D <- merge(HHBase[,.(HHID, Region, Year, Quarter, Month, ProvinceCode)], LoansData, all.x = TRUE)
  D[, GotLoan := ifelse(!is.na(ServiceFee),1,0)]
  D <- merge(D, HHI,all = TRUE)
  D <- merge(D, HHWeights[,.(HHID,Weight)], all = TRUE)
  D <- D[!is.na(Weight)]
  
  svDataL <- svydesign(ids = ~1,data = D,weights = D$Weight)
  
  probit <- svyglm(GotLoan ~  Region + HSex + HAge + I(HAge^2) 
                       + HEmployed + HMarritalState + Size + HLiterate 
                       , design=svDataL, family=binomial(link="probit"))
  
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
