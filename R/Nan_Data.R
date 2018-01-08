# 21-HHNans.R
# Builds the Nan expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11141]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11141:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11141<-TF$Kilos*1000+TF$Grams
TF$NanGram11141<- TF$NanGram11141/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11141 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11141, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11141.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11142]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11142:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11142<-TF$Kilos*1000+TF$Grams
TF$NanGram11142<- TF$NanGram11142/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11142 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11142, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11142.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11143]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11143:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11143<-TF$Kilos*1000+TF$Grams
TF$NanGram11143<- TF$NanGram11143/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11143 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11143, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11143.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11144]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11144:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11144<-TF$Kilos*1000+TF$Grams
TF$NanGram11144<- TF$NanGram11144/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11144 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11144, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11144.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11151]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11151:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11151<-TF$Kilos*1000+TF$Grams
TF$NanGram11151<- TF$NanGram11151/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11151 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11151, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11151.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11152]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11152:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11152<-TF$Kilos*1000+TF$Grams
TF$NanGram11152<- TF$NanGram11152/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11152 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11152, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11152.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11153]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11153:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11153<-TF$Kilos*1000+TF$Grams
TF$NanGram11153<- TF$NanGram11153/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11153 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11153, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11153.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11141.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11142.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11143.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11144.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11151.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11152.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11153.rda"))
Nan_Data<-merge(NanData11141,NanData11142,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11143,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11144,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11151,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11152,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11153,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data[is.na(Nan_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Nans.rda"))
#Nan_Data<-merge(NanData,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-Nan_Data[,Nangram:=NanGram11141+NanGram11142+NanGram11143+NanGram11144+NanGram11151+NanGram11152+NanGram11153]
Nan_Data<-Nan_Data[,NanPrice:=(Price11141*NanGram11141+Price11142*NanGram11142+Price11143*NanGram11143+Price11144*NanGram11144+Price11151*NanGram11151+Price11152*NanGram11152+Price11153*NanGram11153)/(NanGram11141+NanGram11142+NanGram11143+NanGram11144+NanGram11151+NanGram11152+NanGram11153)]
save(Nan_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Nan_Data.rda"))

