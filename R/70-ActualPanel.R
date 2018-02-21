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
load(paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"bussWages.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,92,".rda"))
D92 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D92 <- merge(D92,PubWageData,by="HHID", all.x=TRUE)
D92 <- merge(D92,PrvWageData,by="HHID", all.x=TRUE)
D92 <- merge(D92,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
D92[pubsection==1, WorkClass:="Govt"]
D92[is.na(pubsection) & prvsection=="Agriculture", WorkClass:="Agg"]
D92[is.na(WorkClass) & HNonWageSector=="NonAgriculture", WorkClass:="Buss"]
D92[is.na(HNonWageSector) & HWageWorkType %in% c("Priv","Coop") , WorkClass:="Priv"]
D92[is.na(HNonWageSector) & is.na(HWageWorkType), WorkClass:="Retr"]

rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


load(paste0(Settings$HEISProcessedPath,"Y",93,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",93,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",93,"PubWage.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",93,"PrvWages.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,93,".rda"))
D93 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D93 <- merge(D93,PubWageData,by="HHID", all.x=TRUE)
D93 <- merge(D93,PrvWageData,by="HHID", all.x=TRUE)
D93 <- merge(D93,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


load(paste0(Settings$HEISProcessedPath,"Y",94,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",94,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",94,"PubWage.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",94,"PrvWages.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,94,".rda"))
D94 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D94 <- merge(D94,PubWageData,by="HHID", all.x=TRUE)
D94 <- merge(D94,PrvWageData,by="HHID", all.x=TRUE)
D94 <- merge(D94,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


load(paste0(Settings$HEISProcessedPath,"Y",95,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",95,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",95,"PubWage.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",95,"PrvWages.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,95,".rda"))
D95 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D95 <- merge(D95,PubWageData,by="HHID", all.x=TRUE)
D95 <- merge(D95,PrvWageData,by="HHID", all.x=TRUE)
D95 <- merge(D95,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
rm(HHBase,HHI,PubWageData,PrvWageData,HHWeights)


P23 <- merge(D92,D93,by="HHID",all = FALSE)
P34 <- merge(D93,D94,by="HHID",all = FALSE)
P45 <- merge(D94,D95,by="HHID",all = FALSE)

P234 <- merge(P23,D94,by="HHID",all = FALSE)
P345 <- merge(P34,D95,by="HHID",all = FALSE)

P <- P23
P[is.na(pubsection.x),pubsection.x:=0]
P[is.na(prvsection.x),prvsection.x:=0]
P[is.na(pubsection.y),pubsection.y:=0]
P[is.na(prvsection.y),prvsection.y:=0]

P[,Sect.x:=pubsection.x+10*prvsection.x]
P[,Sect.y:=pubsection.y+10*prvsection.y]

table(P[,.(Region.x,Region.y)])
summary(P[,Quarter.x-Quarter.y])
summary(P[,Month.x-Month.y])
table(P[,.(Month.x,Month.y)])
table(P[,.(ProvinceCode.x-ProvinceCode.y)])
table(P[,.(HActivityState.x,HActivityState.y)])
table(P[,.(Sect.x,Sect.y)])
L <- P[,.(sum(Weight.x,na.rm = TRUE),sum(Weight.y, na.rm = TRUE)),by=.(Sect.x,Sect.y)][order(Sect.x,Sect.y)]


P24 <- merge(D92,D94,by="HHID",all = FALSE)
table(P24[,.(HActivityState.x,HActivityState.y)])
