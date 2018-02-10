# 21-HHMasts.R
# Builds the Mast expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMasts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11424]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11424:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11424<-TF$Kilos*1000+TF$Grams
TF$MastGram11424<- TF$MastGram11424/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11424 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11424, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11424.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMasts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11425]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11425:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11425<-TF$Kilos*1000+TF$Grams
TF$MastGram11425<- TF$MastGram11425/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11425 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11425, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11425.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMasts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11426]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11426:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11426<-TF$Kilos*1000+TF$Grams
TF$MastGram11426<- TF$MastGram11426/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11426 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11426, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11426.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11424.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11425.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11426.rda"))
Mast_Data<-merge(MastData11424,MastData11425,by =c("HHID"),all=TRUE)
Mast_Data<-merge(MastData11426,Mast_Data,by =c("HHID"),all=TRUE)
Mast_Data[is.na(Mast_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Masts.rda"))
#Mast_Data<-merge(MastData,Mast_Data,by =c("HHID"),all=TRUE)
Mast_Data<-Mast_Data[,Mastgram:=MastGram11424+MastGram11425+MastGram11426]
Mast_Data<-Mast_Data[,MastPrice:=(Price11424*MastGram11424+Price11425*MastGram11425+Price11426*MastGram11426)/(MastGram11424+MastGram11425+MastGram11426)]
save(Mast_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Mast_Data.rda"))

