rm(list=ls())
starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")
year<-95

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralEngel.rda"))





load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor2.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor3.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"CBNUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"CBNRural.rda"))


MDU<-MDU[,.(HHID,Region,NewArea,InitialPoor,Weight)]
MDR<-MDR[,.(HHID,Region,NewArea,InitialPoor,Weight)]
CBNUrban<-CBNUrban[,.(HHID,ProvinceCode,Poor2,Weight,Percentile)]
CBNRural<-CBNRural[,.(HHID,ProvinceCode,Poor2,Weight,Percentile)]

#UTMD <- merge(MDU,CBNOld,by=c("HHID"))
MDUPoor<-MDU[InitialPoor==1]
CBNUrban<-CBNUrban[Poor2==1]
MDRPoor<-MDR[InitialPoor==1]
CBNRural<-CBNRural[Poor2==1]
