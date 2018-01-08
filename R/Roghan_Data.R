# 21-HHRoghans.R
# Builds the Roghan expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11511]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11511:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11511<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11511<- TF$RoghanGram11511/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11511 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11511, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11511.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11512]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11512:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11512<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11512<- TF$RoghanGram11512/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11512 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11512, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11512.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11521]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11521:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11521<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11521<- TF$RoghanGram11521/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11521 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11521, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11521.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11522]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11522:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11522<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11522<- TF$RoghanGram11522/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11522 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11522, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11522.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11523]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11523:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11523<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11523<- TF$RoghanGram11523/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11523 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11523, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11523.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11531]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11531:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11531<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11531<- TF$RoghanGram11531/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11531 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11531, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11531.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11532]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11532:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11532<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11532<- TF$RoghanGram11532/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11532 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11532, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11532.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11533]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11533:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11533<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11533<- TF$RoghanGram11533/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11533 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11533, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11533.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11511.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11512.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11521.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11522.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11523.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11531.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11532.rda"))
Roghan_Data<-merge(RoghanData11511,RoghanData11512,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11521,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11522,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11523,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11531,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11532,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11533,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data[is.na(Roghan_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Roghans.rda"))
#Roghan_Data<-merge(RoghanData,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-Roghan_Data[,Roghangram:=RoghanGram11511+RoghanGram11512+RoghanGram11521+RoghanGram11522+RoghanGram11523+RoghanGram11531+RoghanGram11532+RoghanGram11533]
Roghan_Data<-Roghan_Data[,RoghanPrice:=(Price11511*RoghanGram11511+Price11512*RoghanGram11512+Price11521*RoghanGram11521+Price11522*RoghanGram11522+Price11523*RoghanGram11523+Price11531*RoghanGram11531+Price11532*RoghanGram11532+Price11533*RoghanGram11533)/(RoghanGram11511+RoghanGram11512+RoghanGram11521+RoghanGram11522+RoghanGram11523+RoghanGram11531+RoghanGram11532+RoghanGram11533)]
save(Roghan_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Roghan_Data.rda"))

