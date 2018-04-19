# 70-ActualPanel
# 
#
# Copyright © 2018: Majid Einian
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
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
D92 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D92 <- merge(D92,MyIncome,by="HHID", all.x=TRUE)
D92 <- merge(D92,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
D92[is.na(Sector), Sector:="Rtr"]
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


year <- 93
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
D93 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D93 <- merge(D93,MyIncome,by="HHID", all.x=TRUE)
D93 <- merge(D93,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
D93[is.na(Sector), Sector:="Rtr"]
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


year <- 94
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
D94 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D94 <- merge(D94,MyIncome,by="HHID", all.x=TRUE)
D94 <- merge(D94,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
D94[is.na(Sector), Sector:="Rtr"]
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


year <- 95
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
D95 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D95 <- merge(D95,MyIncome,by="HHID", all.x=TRUE)
D95 <- merge(D95,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
D95[is.na(Sector), Sector:="Rtr"]
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


P23 <- merge(D92,D93,by="HHID",all = FALSE)
P34 <- merge(D93,D94,by="HHID",all = FALSE)
P45 <- merge(D94,D95,by="HHID",all = FALSE)

P24 <- merge(D92,D94,by="HHID",all = FALSE)
P35 <- merge(D93,D95,by="HHID",all = FALSE)

P234 <- merge(P23,D94,by="HHID",all = FALSE)
P345 <- merge(P34,D95,by="HHID",all = FALSE)

P <- P35


# table(P[,.(Region.x,Region.y)])
# summary(P[,Quarter.x-Quarter.y])
# summary(P[,Month.x-Month.y])
# table(P[,.(Month.x,Month.y)])
# table(P[,.(ProvinceCode.x-ProvinceCode.y)])
# table(P[,.(HActivityState.x,HActivityState.y)])
# table(P[,.(Sector.x,Sector.y)])
# table(P[,.(Sector.x,HActivityState.y)])
L <- P[,.(sum(Weight.x,na.rm = TRUE),sum(Weight.y, na.rm = TRUE)),by=.(Sector.x,Sector.y)][order(Sector.x,Sector.y)]
L[,sh.x:=V1/sum(V1)*100]
L[,sh.y:=V2/sum(V2)*100]
library(reshape2)
acast(L,Sector.x~Sector.y,value.var = "sh.x")
