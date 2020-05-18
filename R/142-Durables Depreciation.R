# 142-Durables Depreciation.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Durables Depreciation =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(spatstat)
library(scales)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))


load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
HHWeights<- as.data.table(HHWeights)
HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
HHHouseProperties<-merge(HHHouseProperties,HHWeights)
HHHouseProperties<-merge(HHHouseProperties,HHBase)
HHHouseProperties<-merge(HHHouseProperties,MD[,.(HHID,Decile,FinalPoor,Total_Exp_Month,cluster3)])
HHHouseProperties<-merge(HHHouseProperties,TotalDurable)



A1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight)]
A2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (Auto2_rani>0 | Auto1_Khareji>0 | 
                             Auto2_Khareji>0 | Auto1_Irani>0),weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight)]

B1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]
B2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (TV_Rangi_Irani>0 | TV_Rangi_Khareji>0),weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]

C1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(freezer2,Weight)]
C2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (freezer2>0),weighted.mean(freezer2,Weight)]

D1<-   HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                         weighted.mean(OjaghGaz,Weight)]
D2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (OjaghGaz>0),weighted.mean(OjaghGaz,Weight)]

E1<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Mashin_Lebasshooyi,Weight)]
E2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Mashin_Lebasshooyi>0),weighted.mean(Mashin_Lebasshooyi,Weight)]

F1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(Mobile,Weight)]
F2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (Mobile>0),weighted.mean(Mobile,Weight)]

G1<- HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                       weighted.mean(Cooler_Gaz,Weight)]
G2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (Cooler_Gaz>0),weighted.mean(Cooler_Gaz,Weight)]

H1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(PC,Weight)]
H2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (PC>0),weighted.mean(PC,Weight)]

I1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(Lastik_Mashin,Weight)]
I2<- HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                         (Lastik_Mashin>0),weighted.mean(Lastik_Mashin,Weight)]

J1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(Motor_Machin,Weight)]
J2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (Motor_Machin>0),weighted.mean(Motor_Machin,Weight)]

K1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                        weighted.mean(Tamirat_Asasi,Weight)]
K2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                          (Tamirat_Asasi>0),weighted.mean(Tamirat_Asasi,Weight)]

Value<-HHHouseProperties[,.(HHID,Auto2_rani,Auto1_Khareji,Auto2_Khareji,Auto1_Irani,
                            TV_Rangi_Irani,TV_Rangi_Khareji,freezer2,OjaghGaz,
                            Mashin_Lebasshooyi,Mobile,Cooler_Gaz,PC,
                            Lastik_Mashin,Motor_Machin,Tamirat_Asasi)]

save(Value, file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))

}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)