# 35-OtherIncome.R
# Builds the OtherIncome data.table for households
#
# Copyright © 2018: Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ OtherIncome =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#source("funcdefs.R")

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
  UOtherIncomeData <- Tables[[paste0("U",year,tab)]]
  ROtherIncomeData <- Tables[[paste0("R",year,tab)]]
  OtherIncomeData <- rbind(UOtherIncomeData,ROtherIncomeData,fill=TRUE)
  OtherIncomeData <- OtherIncomeData[,lapply(.SD, as.numeric)]

  if(!is.na(OtherITb$Code)){  # 63 to 68
    setnames(OtherIncomeData,OtherITb$HHID,"HHID")
    setnames(OtherIncomeData,OtherITb$Code,"Code")
    setnames(OtherIncomeData,OtherITb$Value,"Value")
    OtherIncomeData[Code %in% eval(parse(text = OtherITb$Retirement)),
              Retirement:=sum(Value),by=HHID]
    OtherIncomeData[Code %in% eval(parse(text = OtherITb$Rent)),
              Rent:=sum(Value),by=HHID]
    OtherIncomeData[Code %in% eval(parse(text = OtherITb$Interest)),
              Interest:=sum(Value),by=HHID]
    OtherIncomeData[Code %in% eval(parse(text = OtherITb$Aid)),
              Aid:=sum(Value),by=HHID]
    OtherIncomeData[Code %in% eval(parse(text = OtherITb$HomeProduction)),
              HomeProduction:=sum(Value),by=HHID]
  }else{ # 69+
    for(n in names(OtherIncomeData)){
      x <- which(OtherITb==n)
      if(length(x)>0)
        setnames(OtherIncomeData,n,names(OtherITb)[x])
    }
  }
  pcols <- intersect(names(OtherIncomeData),
                     c("Code","Retirement","Rent",
                       "Interest","Aid","HomeProduction","Intra"))
  OtherIncomeData <- OtherIncomeData[,lapply(.SD, sum, na.rm=TRUE),
                                     by=HHID,.SDcols=pcols]
  OtherIncomeData <- OtherIncomeData[,lapply(.SD, function(x){x[is.na(x)]<-0;return(x)}), by=HHID, .SDcols=pcols]
  OtherIncomeData[, OtherIncome := Reduce(`+`, .SD), .SDcols=pcols]
  save(OtherIncomeData, 
       file = paste0(Settings$HEISProcessedPath,"Y",year,"OtherIncome.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])
cat(" seconds. ")