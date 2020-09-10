
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)


cat("\n\n================ HHFinance =====================================\n")
FinanceTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Finance))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- FinanceTables[Year==year]
  tab <- ct$Table
  if(is.na(tab))
    next
  UTC <- Tables[[paste0("U",year,tab)]]
  RTC <- Tables[[paste0("R",year,tab)]]
  TC <- rbind(UTC,RTC)
  for(n in names(TC)){
    x <- which(ct==n)
    if(length(x)>0)
      setnames(TC,n,names(ct)[x])
  }
  pcols <- intersect(names(TC),c("HHID","Code","Vam","BuyingMethod","Finance_Exp","Finance_Sale"))
  TC <- TC[,pcols,with=FALSE]
  TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  if(year %in% 84:96){
    TC[,Finance_Exp:=as.numeric(Finance_Exp)]
    TC[,Finance_Sale:=as.numeric(Finance_Sale)]
  }
  TC<-TC[BuyingMethod==1]
  TC$Finance_Exp<-TC$Finance_Exp/12
  TC$Finance_Sale<-TC$Finance_Sale/12
  TC[,Code:=NULL]
  TC[,BuyingMethod:=NULL]
  TC[is.na(TC)] <- 0
  FinanceData <- TC[,lapply(.SD,sum),by=HHID]
  save(FinanceData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Finances.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoors.rda"))
  
  MD<-merge(MD,FinanceData,by="HHID",all.x=TRUE)
  MD[is.na(MD)] <- 0
  
  MD[,Decile_Pop:=sum(Weight),by=Decile]
  MD[Finance_Exp>0,having_vam_Pop:=sum(Weight),by=Decile]
  MD[,having_vam:=ifelse(Finance_Exp>0,1,0)]
  MD[,weighted.mean(having_vam,Weight),by=Decile][order(Decile)]
  MD[Finance_Exp>0,weighted.mean(Finance_Exp*12/Total_Exp_Month,Weight),by=Decile][order(Decile)]
  
  MD[,Y:=sum(Weight*Total_Exp_Month)]
  MD[as.numeric(Percentile)>97,X:=sum(Weight*Total_Exp_Month)]
  MD[,a:=X/Y]
  
  }


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
