# 21-HHPanirs.R
# Builds the Panir expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
tab <- ft$Table
if(is.na(tab))
  next
UTF <- Tables[[paste0("U","95",tab)]]
RTF <- Tables[[paste0("R","95",tab)]]
TF <- rbind(UTF,RTF)
for(n in names(TF)){
  x <- which(ft==n)
  if(length(x)>0)
    setnames(TF,n,names(ft)[x])
}
pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Price"))
TF <- TF[,pcols,with=FALSE]
TF <- TF[Code==11428]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11428:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11428<-TF$Kilos*1000+TF$Grams
TF$PanirGram11428<- TF$PanirGram11428/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11428 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11428, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11428.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
tab <- ft$Table
if(is.na(tab))
  next
UTF <- Tables[[paste0("U","95",tab)]]
RTF <- Tables[[paste0("R","95",tab)]]
TF <- rbind(UTF,RTF)
for(n in names(TF)){
  x <- which(ft==n)
  if(length(x)>0)
    setnames(TF,n,names(ft)[x])
}
pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Price"))
TF <- TF[,pcols,with=FALSE]
TF <- TF[Code==11429]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11429:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11429<-TF$Kilos*1000+TF$Grams
TF$PanirGram11429<- TF$PanirGram11429/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11429 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11429, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11429.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
tab <- ft$Table
if(is.na(tab))
  next
UTF <- Tables[[paste0("U","95",tab)]]
RTF <- Tables[[paste0("R","95",tab)]]
TF <- rbind(UTF,RTF)
for(n in names(TF)){
  x <- which(ft==n)
  if(length(x)>0)
    setnames(TF,n,names(ft)[x])
}
pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Price"))
TF <- TF[,pcols,with=FALSE]
TF <- TF[Code==11430]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11430:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11430<-TF$Kilos*1000+TF$Grams
TF$PanirGram11430<- TF$PanirGram11430/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11430 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11430, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11430.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
tab <- ft$Table
if(is.na(tab))
  next
UTF <- Tables[[paste0("U","95",tab)]]
RTF <- Tables[[paste0("R","95",tab)]]
TF <- rbind(UTF,RTF)
for(n in names(TF)){
  x <- which(ft==n)
  if(length(x)>0)
    setnames(TF,n,names(ft)[x])
}
pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Price"))
TF <- TF[,pcols,with=FALSE]
TF <- TF[Code==11431]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11431:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11431<-TF$Kilos*1000+TF$Grams
TF$PanirGram11431<- TF$PanirGram11431/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11431 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11431, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11431.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
tab <- ft$Table
if(is.na(tab))
  next
UTF <- Tables[[paste0("U","95",tab)]]
RTF <- Tables[[paste0("R","95",tab)]]
TF <- rbind(UTF,RTF)
for(n in names(TF)){
  x <- which(ft==n)
  if(length(x)>0)
    setnames(TF,n,names(ft)[x])
}
pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Price"))
TF <- TF[,pcols,with=FALSE]
TF <- TF[Code==11432]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11432:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11432<-TF$Kilos*1000+TF$Grams
TF$PanirGram11432<- TF$PanirGram11432/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11432 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11432, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11432.rda"))
#}
endtime <- proc.time()

cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11428.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11429.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11430.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11431.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11432.rda"))
Panir_Data<-merge(PanirData11428,PanirData11429,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11430,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11431,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11432,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data[is.na(Panir_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Panirs.rda"))
#Panir_Data<-merge(PanirData,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-Panir_Data[,Panirgram:=PanirGram11428+PanirGram11429+PanirGram11430+PanirGram11431+PanirGram11432]
Panir_Data<-Panir_Data[,PanirPrice:=(Price11428*PanirGram11428+Price11429*PanirGram11429+Price11430*PanirGram11430+Price11431*PanirGram11431+Price11432*PanirGram11432)/(PanirGram11428+PanirGram11429+PanirGram11430+PanirGram11431+PanirGram11432)]
save(Panir_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Panir_Data.rda"))

