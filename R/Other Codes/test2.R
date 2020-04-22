#test-zahra shahidi

rm(list=ls())

library(yaml)

library(readxl)
library(stringr)
year<-96

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
Total<-merge(TD,SMD[,.(HHID,Decile,Percentile)],by="HHID")
Total<-merge(Total,MD[,.(HHID,Bundle_Value,PEngel,Engel,FPLine,PovertyLine,FinalPoor,FinalFoodPoor)])
Total[,weighted.mean(FinalPoor,Weight*Size),by=.(Region)]
Total[,weighted.mean(FinalPoor,Weight*Size),by=.(ProvinceCode)]


Total<-Total[,Diet:=ifelse(FinalFoodPoor==1 & FinalPoor==0,1,0)]
Total[,weighted.mean(Diet,Weight*Size),by=.(Region)]




TotalD <- Total[Diet==1,.(.N,Engle1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                          FPLine=mean(FPLine)),by=.(Region,NewArea_Name)]

Totals<-Total[FinalPoor==1,.(sample2=.N,Engle2=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight)),
             by=.(Region,NewArea_Name)]

Totalt<-merge(TotalD,Totals[,.(Region,NewArea_Name,Engle2,sample2)])



Totalt<-Totalt[,NewArea_Name:=NULL]
Totalt <- Totalt[,lapply(.SD,sum),by=.(Region)]
