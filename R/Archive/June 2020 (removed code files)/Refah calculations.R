# Step 7,8,9 and other
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)


year<-97

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"job.rda"))

MD<-merge(MD,job[,.(HHID,PubWageNetIncomeY,PrvWageNetIncomeY)],by=c("HHID"))

MD[FinalPoor==1,weighted.mean(Total_Exp_Month_Per,Weight),by="Region"]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per>0,
   sum((PovertyLine-Total_Exp_Month_Per)*Size*Weight),by="Region"]

MD[FinalPoor==1,
   sum((PovertyLine-Total_Exp_Month_Per)*Size*Weight)]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per_nondurable>0,
   sum((PovertyLine-Total_Exp_Month_Per_nondurable)*Size*Weight),by="Region"]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per_nondurable>0,
   sum((PovertyLine-Total_Exp_Month_Per_nondurable)*Size*Weight)]

MD[NewArea==2301,sum(Weight*Size)]

MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)][order(Region)]

MD[,weighted.mean(Bundle_Value,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Region)][order(Region)]

MD[,weighted.mean(HHEngle,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(HHEngle,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(HHEngle,Weight),by=.(Region)][order(Region)]

load(file = paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
MD<-merge(MD,RetirementWageData,by="HHID",all.x = TRUE)

RT<-MD[,.(HHID,Region,HAge,HActivityState,Decile,retirement,Weight)]
RT[is.na(RT)] <- 0
RT[,Old:=ifelse(retirement>0 & HAge>48,1,0)]
RT[,weighted.mean(Old,Weight)]
RT[,weighted.mean(Old,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old,Weight),by=Region][order(Region)]

RT[,Old1:=ifelse(retirement>0 & HAge>48 & 
                   (PubWageNetIncomeY==0 & PrvWageNetIncomeY==0 & 
                      BussNetIncomeY==0 & AgriNetIncomeY==0),1,0)]
RT[,weighted.mean(Old1,Weight)]
RT[,weighted.mean(Old1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old1,Weight),by=Region][order(Region)]

RT[,Old2:=ifelse(retirement>0 & HAge>48 & HActivityState!="Employed" &
                   (PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                      BussNetIncomeY>0 | AgriNetIncomeY>0),1,0)]
RT[,weighted.mean(Old2,Weight)]
RT[,weighted.mean(Old2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old2,Weight),by=Region][order(Region)]

RT[,Old3:=ifelse(retirement>0 & HAge>48 & HActivityState=="Employed" &
                   (PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                      BussNetIncomeY>0 | AgriNetIncomeY>0),1,0)]
RT[,weighted.mean(Old3,Weight)]
RT[,weighted.mean(Old3,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old3,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old3,Weight),by=Region][order(Region)]


load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
RT<-merge(RT,TotalDurable[,.(HHID,Premium_gheyredarmani_mostakhdem,
                             Premium_gheyredarmani_karfarma,Premium_retirement_mostakhdem,
                             Premium_retirement_karfarma,Premium_retirement_general,
                             Premium_retirement_Rural_Household,Premium_retirement_Rural_Govern,
                             Premium_retirement_bank)])

RT[,pub1:=ifelse(HActivityState=="Employed" &
                   PubWageNetIncomeY>0 ,1,0)]
RT[,weighted.mean(pub1,Weight)]
RT[,weighted.mean(pub1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(pub1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(pub1,Weight),by=Region][order(Region)]


RT[,pub2:=ifelse(PubWageNetIncomeY>0 ,1,0)]
RT[,weighted.mean(pub2,Weight)]
RT[,weighted.mean(pub2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(pub2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(pub2,Weight),by=Region][order(Region)]


load(file = paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))

RT<-merge(RT,PubWageData[,.(HHID,PubWageNetIncomeY)],all.x = TRUE)
RT<-merge(RT,PrvWageData[,.(HHID,PrvWageNetIncomeY)],all.x = TRUE)
RT<-merge(RT,BussIncomeData[,.(HHID,BussNetIncomeY)],all.x = TRUE)
RT<-merge(RT,AgriIncomeData[,.(HHID,AgriNetIncomeY)],all.x = TRUE)
RT[is.na(RT)] <- 0

a<-RT[AgriNetIncomeY!=0,.(HHID,Region,AgriNetIncomeY,
                          Premium_gheyredarmani_mostakhdem,Weight)]
a[,weighted.mean(Premium_gheyredarmani_mostakhdem>0,Weight),by=Region]

b<-RT[BussNetIncomeY!=0,.(HHID,Region,BussNetIncomeY,
                          Premium_gheyredarmani_mostakhdem,Weight)]
b[,weighted.mean(Premium_gheyredarmani_mostakhdem>0,Weight),by=Region]

RT[,P:=ifelse(HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]

RT[,P:=ifelse(HActivityState=="Income without Work",1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]


RT[,P:=ifelse(PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                BussNetIncomeY>0 | AgriNetIncomeY>0,1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]


RT[,P1:=ifelse((Premium_gheyredarmani_mostakhdem>0 |
                  Premium_gheyredarmani_karfarma>0) & 
                 HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P1,Weight)]
RT[,weighted.mean(P1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P1,Weight),by=Region][order(Region)]

RT[,P2:=ifelse(Premium_gheyredarmani_karfarma>0 & HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P2,Weight)]
RT[,weighted.mean(P2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P2,Weight),by=Region][order(Region)]

RT[,P3:=ifelse(Premium_retirement_mostakhdem>0,1 ,0)]
RT[,weighted.mean(P3,Weight)]
RT[,weighted.mean(P3,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P4:=ifelse(Premium_retirement_karfarma>0,1 ,0)]
RT[,weighted.mean(P4,Weight)]
RT[,weighted.mean(P4,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P5:=ifelse(Premium_retirement_general>0,1 ,0)]
RT[,weighted.mean(P5,Weight)]
RT[,weighted.mean(P5,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P6:=ifelse(Premium_retirement_Rural_Household>0,1 ,0)]
RT[,weighted.mean(P6,Weight)]
RT[,weighted.mean(P6,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P7:=ifelse(Premium_retirement_Rural_Govern>0,1 ,0)]
RT[,weighted.mean(P7,Weight)]
RT[,weighted.mean(P7,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P8:=ifelse(Premium_retirement_bank>0,1 ,0)]
RT[,weighted.mean(P8,Weight)]
RT[,weighted.mean(P8,Weight),by=.(Region,Decile)][order(Region,Decile)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")