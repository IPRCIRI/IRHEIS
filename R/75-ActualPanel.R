# 70-ActualPanel
# 
#
# Copyright © 2018-2019: Majid Einian
# Licence: GPL-3
# 
rm(list = ls())

starttime <- proc.time()
cat("\n\n================  =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

year <- 92

load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
D92 <- merge(MD,IncomeTable,by="HHID", all.x=TRUE)
D92[is.na(WorkClass), WorkClass:="Rtr"]
D92 <- D92[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode,NewArea,
              HEmployed,HUnemployed,
              Weight,EqSizeRevOECD,
              HBY=1300+Year-HAge,
              CPI=70.916,
              Total_Exp_Month,Total_Exp_Month_nondurable,
              Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,
              NetIncome,WorkClass)]
rm(MD,IncomeTable)

year <- 93
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
D93 <- merge(MD,IncomeTable,by="HHID", all.x=TRUE)
D93[is.na(WorkClass), WorkClass:="Rtr"]
D93 <- D93[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode,NewArea
              ,HEmployed,HUnemployed
              ,Weight,EqSizeRevOECD,
              HBY=1300+Year-HAge,
              CPI=81.948,
              Total_Exp_Month,Total_Exp_Month_nondurable,
              Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,
              NetIncome,WorkClass)]
rm(MD,IncomeTable)

year <- 94
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
D94 <- merge(MD,IncomeTable,by="HHID", all.x=TRUE)
D94[is.na(WorkClass), WorkClass:="Rtr"]
D94 <- D94[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode,NewArea
              ,HEmployed,HUnemployed
              ,Weight,EqSizeRevOECD,
              HBY=1300+Year-HAge,
              CPI=91.714,
              Total_Exp_Month,Total_Exp_Month_nondurable,
              Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,
              NetIncome,WorkClass)]
rm(MD,IncomeTable)

year <- 95
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
D95 <- merge(MD,IncomeTable,by="HHID", all.x=TRUE)
D95[is.na(WorkClass), WorkClass:="Rtr"]
D95 <- D95[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode,NewArea
              ,HEmployed,HUnemployed
              ,Weight,EqSizeRevOECD,
              HBY=1300+Year-HAge,
              CPI=100.000,
              Total_Exp_Month,Total_Exp_Month_nondurable,
              Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,
              NetIncome,WorkClass)]
rm(MD,IncomeTable)

year <- 96
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
D96 <- merge(MD,IncomeTable,by="HHID", all.x=TRUE)
D96[is.na(WorkClass), WorkClass:="Rtr"]
D96 <- D96[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode,NewArea
              ,HEmployed,HUnemployed
              ,Weight,EqSizeRevOECD,
              HBY=1300+Year-HAge,
              CPI=109.650,
              Total_Exp_Month,Total_Exp_Month_nondurable,
              Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,
              NetIncome,WorkClass)]
rm(MD,IncomeTable)



P23 <- merge(D92,D93,by="HHID",all = FALSE)
P34 <- merge(D93,D94,by="HHID",all = FALSE)
P45 <- merge(D94,D95,by="HHID",all = FALSE)
P56 <- merge(D95,D96,by="HHID",all = FALSE)

P24 <- merge(D92,D94,by="HHID",all = FALSE)
P35 <- merge(D93,D95,by="HHID",all = FALSE)
P46 <- merge(D94,D96,by="HHID",all = FALSE)

P234 <- merge(P23,D94,by="HHID",all = FALSE)
P345 <- merge(P34,D95,by="HHID",all = FALSE)
P456 <- merge(P45,D96,by="HHID",all = FALSE)


P <- P23

for(P in list(P23,P34,P45,P56)){
  P[NetIncome.x==0,NetIncome.x:=NA]
  P[NetIncome.y==0,NetIncome.y:=NA]
  P[,c0:=log(Total_Exp_Month_Per_nondurable.x/CPI.x*100)]
  P[,c1:=log(Total_Exp_Month_Per_nondurable.y/CPI.y*100)]
  P[,y0:=log(NetIncome.x/EqSizeRevOECD.x/CPI.x*100)]
  P[,y1:=log(NetIncome.y/EqSizeRevOECD.y/CPI.y*100)]
  #sd(P$y0,na.rm = TRUE)
  #sd(P$y1,na.rm = TRUE)
  print(cov.wt(P[!is.na(y0) & !is.na(y1),.(y0,y1)],wt = P[!is.na(y0) & !is.na(y1)]$Weight.x,cor = TRUE)$cor[2,1])
}
 table(P[,.(Region.x,Region.y)])
 summary(P[,Quarter.x-Quarter.y])
 summary(P[,Month.x-Month.y])
 table(P[,.(Month.x,Month.y)])
 table(P[,.(ProvinceCode.x-ProvinceCode.y)])
# table(P[,.(HActivityState.x,HActivityState.y)])
 table(P[,.(WorkClass.x,WorkClass.y)])
# table(P[,.(WorkClass.x,HActivityState.y)])
 

 


 
L <- P[,.(sum(Weight.x,na.rm = TRUE),sum(Weight.y, na.rm = TRUE)),by=.(WorkClass.x,WorkClass.y)][order(WorkClass.x,WorkClass.y)]
L[,sh.x:=V1/sum(V1)*100]
L[,sh.y:=V2/sum(V2)*100]
library(reshape2)
acast(L,WorkClass.x~WorkClass.y,value.var = "sh.x")


