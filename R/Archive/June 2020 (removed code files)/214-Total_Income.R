# 38-Total_Income.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ TotalIncome =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")


#source("funcdefs.R")

library(readxl)
library(data.table)
bigdt <- data.table(V1=NA_integer_,N=NA_integer_,year=NA_integer_)[0]
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear: ",year,"\n"))
  
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OtherIncome.rda"))

   PubWageData[,Sector:=NULL]
   PrvWageData[,Sector:=NULL]
   BussIncomeData[,Sector:=NULL]
   AgriIncomeData[,Sector:=NULL]
   
   #MyIncome<-merge(HHBase,PubWageData,by =c("HHID"),all=TRUE)
   
   IncomeTable <- merge(HHBase[,.(HHID)],PubWageData,by="HHID",all.x = TRUE)
   IncomeTable <- merge(IncomeTable,PrvWageData,by="HHID",all.x = TRUE)
   IncomeTable <- merge(IncomeTable,BussIncomeData,by="HHID",all.x = TRUE)
   IncomeTable <- merge(IncomeTable,AgriIncomeData,by="HHID",all.x = TRUE)
   IncomeTable <- merge(IncomeTable,OtherIncomeData,by="HHID",all.x = TRUE)

   
   IncomeTable <- IncomeTable[,lapply(.SD, function(x){x[is.na(x)]<-0;return(x)})]
   
   
   IncomeTable[, NetIncome := PubWageNetIncomeY+PrvWageNetIncomeY+BussNetIncomeY+AgriNetIncomeY+OtherIncome]
   
   IncomeTable[PubWageNetIncomeY>0 | PubEarners>0, WorkClass:="Pub"]
   IncomeTable[is.na(WorkClass) & PrvWageNetIncomeY >0, WorkClass:="Prv"]
   IncomeTable[is.na(WorkClass) & BussNetIncomeY>0 , WorkClass:="Buss"]
   IncomeTable[is.na(WorkClass) & AgriNetIncomeY>0 , WorkClass:="Agri"]
   IncomeTable[is.na(WorkClass) & OtherIncome>0, WorkClass:="Retr"]
   
   IncomeTable[,WorkClass:=factor(WorkClass)]

 
   save(IncomeTable, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])
cat(" seconds. ")