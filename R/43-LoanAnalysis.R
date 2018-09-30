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


bigX <- data.table(WorkClass=NA_character_,X=NA_real_,Year=NA_integer_)[0]

for(year in (Settings$startyear:Settings$endyear)){
#  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  cat(paste0("\n",year,","))
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda")) |
     !file.exists(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda")) )
    next
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  D <- merge(HHBase[,.(HHID, Region, Year, Quarter, Month, ProvinceCode)], LoansData,by="HHID", all.x = TRUE)
  D[, GotLoan := ifelse(!is.na(ServiceFee),1,0)]
  D <- merge(D, HHI,all = TRUE)
  D <- merge(D, HHWeights[,.(HHID,Weight)], all = TRUE)
  D <- merge(D, IncomeTable)
  D <- D[!is.na(Weight)]
  
  X <- D[,.(X=sum(GotLoan*Weight)/sum(Weight)*100),by=.(WorkClass,ProvinceCode)]
  X[,Year:=year]
  bigX <- rbind(bigX,X)
  
 svDataL <- svydesign(ids = ~1,data = D,weights = D$Weight)

 probit_model <- svyglm(GotLoan ~  Region + HSex + HAge + I(HAge^2)
                      + HEmployed + HMarritalState + Size + HLiterate + WorkClass + NetIncome
                      , design=svDataL, family=binomial(link="probit"))
 print(summary(probit_model))
 mgf <- svycontrast(probit_model, quote(
    (exp(`(Intercept)` + WorkClassPub) / (exp(`(Intercept)` + WorkClassPub) + 1)) - 
      (exp(`(Intercept)`) / (exp(`(Intercept)`) + 1))))
 x <- mgf[1]
 y <- sqrt(attr(mgf,"var"))
 cat(",",x*100,",",(x-1.96*y)*100,",",(x+1.96*y)*100)
  # Can't use custom functions like expit :_(
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

library(XLConnect)
writeWorksheetToFile("D:/Loans.xlsx",bigX,"Sheet1")
