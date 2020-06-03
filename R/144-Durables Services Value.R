# 144-Durables Services Value.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Durables Services Value =====================================\n")

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

load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
HHWeights<- as.data.table(HHWeights)
HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
HHHouseProperties<-merge(HHHouseProperties,HHWeights)
HHHouseProperties<-merge(HHHouseProperties,HHBase)
HHHouseProperties<-merge(HHHouseProperties,TotalDurable)



A1<-  HHHouseProperties[ ,weighted.mean(`71116`+`71117`+`71111`+`71112`,Weight)]
A2<-  HHHouseProperties[(`71116`>0 | `71117`>0 | 
                             `71111`>0 | `71112`>0),weighted.mean(`71116`+`71117`+`71111`+`71112`,Weight)]

B1<-  HHHouseProperties[,weighted.mean(`91128`+`91129`,Weight)]
B2<-  HHHouseProperties[(`91128`>0 | `91129`>0),weighted.mean(`91128`+`91129`,Weight)]

C1<-  HHHouseProperties[, weighted.mean(`53112`+`53111`,Weight)]
C2<-    HHHouseProperties[(`53112`+`53111`>0),weighted.mean(`53112`+`53111`,Weight)]

D1<-   HHHouseProperties[,weighted.mean(`53116`,Weight)]
D2<-    HHHouseProperties[(`53116`>0),weighted.mean(`53116`,Weight)]

E1<-    HHHouseProperties[, weighted.mean(`53113`,Weight)]
E2<-    HHHouseProperties[(`53113`>0),weighted.mean(`53113`,Weight)]

F1<-  HHHouseProperties[,weighted.mean(`82113`,Weight)]
F2<-  HHHouseProperties[(`82113`>0),weighted.mean(`82113`,Weight)]

G1<- HHHouseProperties[,weighted.mean(`53125`,Weight)]
G2<-  HHHouseProperties[(`53125`>0),weighted.mean(`53125`,Weight)]

H1<-  HHHouseProperties[,weighted.mean(`91311`,Weight)]
H2<-  HHHouseProperties[(`91311`>0),weighted.mean(`91311`,Weight)]

if (year!=92){
  J1<-  HHHouseProperties[,weighted.mean(`72118`,Weight)]
  J2<-  HHHouseProperties[(`72118`>0),weighted.mean(`72118`,Weight)]
}

I1<-  HHHouseProperties[, weighted.mean(`72111`,Weight)]
I2<- HHHouseProperties[(`72111`>0),weighted.mean(`72111`,Weight)]



K1<-  HHHouseProperties[, weighted.mean(`72319`,Weight)]
K2<-  HHHouseProperties[(`72319`>0),weighted.mean(`72319`,Weight)]

if (year!=92){
Value<-HHHouseProperties[,.(HHID,`71116`,`71117`,`71111`,`71112`,
                            `91128`,`91129`,`53112`,`53111`,`53116`,
                            `53113`,`82113`,`53125`,`91311`,
                            `72111`,`72118`,`72319`)]
}

if (year==92){
Value<-HHHouseProperties[,.(HHID,`71116`,`71117`,`71111`,`71112`,
                            `91128`,`91129`,`53112`,`53111`,`53116`,
                            `53113`,`82113`,`53125`,`91311`,
                            `72111`,`72319`)]
}

save(Value, file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))

}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)