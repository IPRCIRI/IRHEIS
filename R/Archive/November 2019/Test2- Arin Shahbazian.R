#Test2- Arin Shahbazian
rm(list=ls())

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

year<-96

load( file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
TD<-MD
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

Total<-merge(TD,SMD[,.(HHID,Decile,Percentile)],by="HHID")
Total<-merge(Total,MD[,.(HHID,Bundle_Value,PEngel,Engel,
                         FPLine,PovertyLine,FinalFoodPoor,FinalPoor)])

Total[,weighted.mean(FinalPoor,Weight),by=.(Region)]
Total[,weighted.mean(FinalPoor,Weight*Size),by=.(Region)]

Total[,weighted.mean(FinalPoor,Weight*Size),by=.(ProvinceCode)]

Total<-Total[,Diet:=ifelse(FinalFoodPoor==1 & FinalPoor==0,1,0)]
Total[,weighted.mean(Diet,Weight*Size),by=.(Region)]
Total[NewArea_Name=="Sh_Tehran",weighted.mean(Diet,Weight)]

TotalD <- Total[Diet==1,
              .(.N,Engle1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight)),
              by=.(Region,NewArea_Name)]
TotalD<-TotalD[,Sample:=N]
TotalD[,N:=NULL]

TotalS<-Total[FinalPoor==1,
              .(.N,Engle2=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight)),
              by=.(Region,NewArea_Name)]
TotalS<-TotalS[,Sample2:=N]
TotalD[,N:=NULL]

TotalT<-merge(TotalD,TotalS[,.(Region,NewArea_Name,Engle2,Sample2)])
              

