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

library(readxl)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"bussWages.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RentWage.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InterestWage.rda"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HomemadeWage.rda"))
    
   if(year %in% 90:94){
     load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
        }
   if(year %in% 78:94){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"IntraWage.rda"))
   }
    MyIncome<-merge(HHBase,PubWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,PrvWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,bussWageData,by =c("HHID"),all=TRUE)
   MyIncome<-merge(MyIncome,AgriWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,RetirementWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,RentWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,InterestWageData,by =c("HHID"),all=TRUE)
   MyIncome<-merge(MyIncome,AidWageData,by =c("HHID"),all=TRUE)
    MyIncome<-merge(MyIncome,HomemadeWageData,by =c("HHID"),all=TRUE)
   

    if(year %in% 90:94){
      MyIncome<-merge(MyIncome,SubsidyWageData,by =c("HHID"),all=TRUE)
    }
    if(year %in% 78:94){
      MyIncome<-merge(MyIncome,IntraWageData,by =c("HHID"),all=TRUE)
    }
  
  save(MyIncome, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)