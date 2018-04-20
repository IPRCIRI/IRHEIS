# 35-OtherIncome.R
# Builds the OtherIncome data.table for households
#
# Copyright © 2018: Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHOtherIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


OtherIncomeTable <- data.table(read_excel(Settings$MetaDataFilePath,
                                          sheet=Settings$MDS_OtherInc))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  OtherITb <- OtherIncomeTable[Year==year]
  tab <- OtherITb$Table
  if(is.na(tab))
    next
  UTOtherInc <- Tables[[paste0("U",year,tab)]]
  RTOtherInc <- Tables[[paste0("R",year,tab)]]
  TOtherInc <- rbind(UTOtherInc,RTOtherInc,fill=TRUE)
  TOtherInc <- TOtherInc[,lapply(.SD, as.integer)]
  if(!is.na(OtherITb$Code)){  # 63 to 68
    setnames(TOtherInc,OtherITb$HHID,"HHID")
    setnames(TOtherInc,OtherITb$Code,"Code")
    setnames(TOtherInc,OtherITb$Value,"Value")
    TOtherInc[Code %in% eval(parse(text = OtherITb$Retirement)),
              Retirement:=sum(Value),by=HHID]
    TOtherInc[Code %in% eval(parse(text = OtherITb$Rent)),
              Rent:=sum(Value),by=HHID]
    TOtherInc[Code %in% eval(parse(text = OtherITb$Interest)),
              Interest:=sum(Value),by=HHID]
    TOtherInc[Code %in% eval(parse(text = OtherITb$Aid)),
              Aid:=sum(Value),by=HHID]
    TOtherInc[Code %in% eval(parse(text = OtherITb$HomeProduction)),
              HomeProduction:=sum(Value),by=HHID]
  }else{ # 69+
    for(n in names(TOtherInc)){
      x <- which(OtherITb==n)
      if(length(x)>0)
        setnames(TOtherInc,n,names(OtherITb)[x])
    }
  }
  pcols <- intersect(names(TOtherInc),
                     c("HHID","Code","Retirement","Rent",
                       "Interest","Aid","HomeProduction","Intra"))
  TOtherInc <- TOtherInc[,lapply(.SD, sum, na.rm=TRUE),by=HHID,.SDcols=pcols]
  repna <- function(x) replace(x, which(is.na(x)), 0)
  TOtherInc <- TOtherInc[,lapply(.SD, repna), .SDcols=pcols]
  TOtherInc[, OtherIncome := Reduce(`+`, .SD), .SDcols=pcols]
  save(TOtherInc, 
       file = paste0(Settings$HEISProcessedPath,"Y",year,"OtherIncome.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])
cat(" seconds.")