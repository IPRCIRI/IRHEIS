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
D92 <- merge(HHBase,HHI,by="HHID")


load(paste0(Settings$HEISProcessedPath,"Y",93,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",93,"HHI.rda"))
D93 <- merge(HHBase,HHI,by="HHID")


load(paste0(Settings$HEISProcessedPath,"Y",94,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",94,"HHI.rda"))
D94 <- merge(HHBase, HHI, by = "HHID")


load(paste0(Settings$HEISProcessedPath,"Y",95,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",95,"HHI.rda"))
load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,95,".rda"))
D95 <- merge(HHBase,HHI,by="HHID")
D95 <- merge(D95,HHWeights)




h23 <- intersect(D92$HHID,D93$HHID)
h234 <- intersect(h23,D94$HHID)
h2345 <- intersect(h234,D95$HHID)

P23 <- merge(D92,D93,by="HHID",all = FALSE)
P34 <- merge(D93,D94,by="HHID",all = FALSE)
P45 <- merge(D94,D95,by="HHID",all = FALSE)

P <- P23

table(P[,.(Region.x,Region.y)])
summary(P[,Quarter.x-Quarter.y])
summary(P[,Month.x-Month.y])
table(P[,.(Month.x,Month.y)])
table(P[,.(ProvinceCode.x-ProvinceCode.y)])
table(P[,.(HActivityState.x,HActivityState.y)])


P24 <- merge(D92,D94,by="HHID",all = FALSE)
table(P24[,.(HActivityState.x,HActivityState.y)])
