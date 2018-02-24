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
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
   # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
   # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RentWage.rda"))
   # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InterestWage.rda"))
   # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))
   # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HomemadeWage.rda"))

   if(year %in% 90:94){
     load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
   }
   
   if(year %in% 78:94){
#    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"IntraWage.rda"))
   }
   
   #MyIncome<-merge(HHBase,PubWageData,by =c("HHID"),all=TRUE)
   
   SX <- rbind(PubWageData[,.(HHID,Sector)],
               PrvWageData[,.(HHID,Sector)],
               BussIncomeData[,.(HHID,Sector)],
               AgriIncomeData[,.(HHID,Sector)])
   SX[,n:=.N, by=HHID]
   
  # MyIncome<-merge(HHBase,PubWageData,by =c("HHID"),all=TRUE)
   MyIncome<-PubWageData[HHBase[,.(HHID)],on="HHID"]
   MyIncome<-merge(MyIncome,PrvWageData,by =c("HHID"),all=TRUE)
   setnames(MyIncome,c("Sector.x","Sector.y"),c("Sector.Pub","Sector.Prv"))
   MyIncome<-merge(MyIncome,BussIncomeData,by =c("HHID"),all=TRUE)
   MyIncome<-merge(MyIncome,AgriIncomeData,by =c("HHID"),all=TRUE)
   setnames(MyIncome,c("Sector.x","Sector.y"),c("Sector.Bus","Sector.Agr"))
   # MyIncome<-merge(MyIncome,RentWageData,by =c("HHID"),all=TRUE)
   # MyIncome<-merge(MyIncome,InterestWageData,by =c("HHID"),all=TRUE)
   # MyIncome<-merge(MyIncome,AidWageData,by =c("HHID"),all=TRUE)
   # MyIncome<-merge(MyIncome,HomemadeWageData,by =c("HHID"),all=TRUE)


   #  if(year >= 90){
   #    MyIncome<-merge(MyIncome,SubsidyWageData,by =c("HHID"),all=TRUE)
   #  }
   # if(year >= 78){
   #    MyIncome<-merge(MyIncome,IntraWageData,by =c("HHID"),all=TRUE)
   #  }
   # 
   MyIncome[is.na(MyIncome)] <- 0
   # MyIncome[is.na(Sector.Pub),Sector.Pub:=0]
   # MyIncome[is.na(Sector.Prv),Sector.Prv:=0]
   # MyIncome[is.na(Sector.Bus),Sector.Bus:=0]
   # MyIncome[is.na(Sector.Agr),Sector.Agr:=0]
   MyIncome <- MyIncome[,lapply(.SD,sum),by=HHID]
   MyIncome[,XSector:=Sector.Agr+10*Sector.Pub+100*Sector.Prv+1000*Sector.Bus]
   
   MyIncome[PubWageNetIncomeY>0,Sector:="Pub"]
   MyIncome[,XIncome:= max(BussNetIncomeY,AgriNetIncomeY,PrvWageNetIncomeY),by=HHID ]
   MyIncome[is.na(Sector) & XIncome>0 & BussNetIncomeY==XIncome, Sector:="Bus"]
   MyIncome[is.na(Sector) & XIncome>0 & AgriNetIncomeY==XIncome, Sector:="Agr"]
   MyIncome[is.na(Sector) & XIncome>0 & PrvWageNetIncomeY==XIncome, Sector:="Prv"]
   table(MyIncome$Sector,useNA = "always")
table(MyIncome$XSector,MyIncome$Sector,useNA = "always")  
summary(MyIncome[is.na(Sector)])
  save(MyIncome, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  
}
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)