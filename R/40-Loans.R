# 40-Loans
# 
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Loans =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

LoanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Loans))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- LoanTables[Year==year]
  tab <- ty$Table
  
  UTL <- Tables[[paste0("U",year,tab)]]
  RTL <- Tables[[paste0("R",year,tab)]]
  TL <- rbind(UTL,RTL)
  for(n in names(TL)){
    x <- which(ty==n)
    if(length(x)>0)
    setnames(TL,n,names(ty)[x])
  }
  pcols <- intersect(names(TL),c("HHID","Code","ServiceFee","LoanValue","BankName"))
  TL <- TL[,pcols,with=FALSE]
  TL <- TL[Code %in% c(ty$NonHousingLoan,ty$HousingLoan)]
  TL[,ServiceFee:=as.numeric(ServiceFee)]
  if("LoanValue" %in% pcols){
    TL[,LoanValue:=as.numeric(LoanValue)]
    TL[,p:=ServiceFee/LoanValue]
  }
  HTL <- TL[Code==ty$HousingLoan]
  setnames(HTL,names(HTL)[3:ncol(HTL)],paste0("H",names(HTL)[3:ncol(HTL)]))
  HTL[,Code:=NULL]
  NTL <- TL[Code==ty$NonHousingLoan]
  NTL[,Code:=NULL]
  
  LoansData <- merge(NTL,HTL,all = TRUE,by="HHID")
  LoansData[,BankName:=NULL]
  LoansData[,HBankName:=NULL]
  LoansData[is.na(LoansData)] <- 0
  
  LoansData <- LoansData[,lapply(.SD,sum),by=HHID]
  
#   if("LoanValue" %in% names(LoansData)){
#     cat("\n",year,",",
# #        mean(LoansData$ServiceFee/LoansData$LoanValue,na.rm = TRUE))
#         mean(LoansData$ServiceFee,na.rm = TRUE), mean(LoansData$LoanValue,na.rm = TRUE))
# 
#   }
#   if("HLoanValue" %in% names(LoansData)){
#     cat(",",
# #        mean(LoansData$HServiceFee/LoansData$HLoanValue,na.rm = TRUE))
#         mean(LoansData$HServiceFee,na.rm = TRUE), mean(LoansData$HLoanValue,na.rm = TRUE))
#   }
  
  # cat("\n",year,",",mean(LoansData$ServiceFee,na.rm = TRUE),",",mean(LoansData$HServiceFee,na.rm = TRUE),
  #     ",",mean(LoansData$LoanValue,na.rm = TRUE),",",mean(LoansData$HLoanValue,na.rm = TRUE))
  
  save(LoansData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda"))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
