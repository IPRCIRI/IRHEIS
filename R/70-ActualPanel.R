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

load(paste0(Settings$HEISProcessedPath,"Y",92,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",92,"HHI.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",92,"PubWage.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",92,"PrvWages.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,92,".rda"))
D92 <- merge(HHBase,HHI,by="HHID", all.x=TRUE)
D92 <- merge(D92,PubWageData,by="HHID", all.x=TRUE)
D92 <- merge(D92,PrvWageData,by="HHID", all.x=TRUE)
D92 <- merge(D92,HHWeights[,.(HHID,Weight)],by="HHID", all.x=TRUE)
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

table(P[,.(Region.x,Region.y)])
summary(P[,Quarter.x-Quarter.y])
summary(P[,Month.x-Month.y])
table(P[,.(Month.x,Month.y)])
table(P[,.(ProvinceCode.x-ProvinceCode.y)])
table(P[,.(HActivityState.x,HActivityState.y)])
table(P[,.(pubsection.x,pubsection.y)])


P24 <- merge(D92,D94,by="HHID",all = FALSE)
table(P24[,.(HActivityState.x,HActivityState.y)])
