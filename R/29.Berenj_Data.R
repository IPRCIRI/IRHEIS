# 21-HHBerenjs.R
# Builds the Berenj expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))



#for(year in (Settings$startyear:Settings$endyear)){
  #cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
  ft <- BerenjTables[Year=="95"]
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
  TF <- TF[Code==11111]
  if("95" %in% 84:94){
    TF[,Kilos:=as.numeric(Kilos)]
    TF[,Grams:=as.numeric(Grams)]
  }
  TF[,Price11111:=as.numeric(Price)]
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$BerenjGram11111<-TF$Kilos*1000+TF$Grams
  TF$BerenjGram11111<- TF$BerenjGram11111/30
  TF[,Grams:=NULL]
  TF[,Kilos:=NULL]
  TF[,Price:=NULL]
  BerenjData11111 <- TF[,lapply(.SD,sum),by=HHID]
  save(BerenjData11111, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11111.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11112]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11112:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11112<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11112<- TF$BerenjGram11112/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11112 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11112, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11112.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11113]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11113:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11113<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11113<- TF$BerenjGram11113/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11113 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11113, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11113.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11114]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11114:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11114<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11114<- TF$BerenjGram11114/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11114 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11114, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11114.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11115]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11115:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11115<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11115<- TF$BerenjGram11115/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11115 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11115, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11115.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11116]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11116:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11116<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11116<- TF$BerenjGram11116/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11116 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11116, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11116.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11117]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11117:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11117<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11117<- TF$BerenjGram11117/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11117 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11117, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11117.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBerenjs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- BerenjTables[Year=="95"]
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
TF <- TF[Code==11118]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11118:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$BerenjGram11118<-TF$Kilos*1000+TF$Grams
TF$BerenjGram11118<- TF$BerenjGram11118/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
BerenjData11118 <- TF[,lapply(.SD,sum),by=HHID]
save(BerenjData11118, file = paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11118.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11111.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11112.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11113.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11114.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11115.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11116.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","BerenjData11117.rda"))
Berenj_Data<-merge(BerenjData11111,BerenjData11112,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11113,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11114,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11115,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11116,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11117,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-merge(BerenjData11118,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data[is.na(Berenj_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Berenjs.rda"))
#Berenj_Data<-merge(BerenjData,Berenj_Data,by =c("HHID"),all=TRUE)
Berenj_Data<-Berenj_Data[,Berenjgram:=BerenjGram11111+BerenjGram11112+BerenjGram11113+BerenjGram11114+BerenjGram11115+BerenjGram11116+BerenjGram11117+BerenjGram11118]
Berenj_Data<-Berenj_Data[,BerenjPrice:=(Price11111*BerenjGram11111+Price11112*BerenjGram11112+Price11113*BerenjGram11113+Price11114*BerenjGram11114+Price11115*BerenjGram11115+Price11116*BerenjGram11116+Price11117*BerenjGram11117+Price11118*BerenjGram11118)/(BerenjGram11111+BerenjGram11112+BerenjGram11113+BerenjGram11114+BerenjGram11115+BerenjGram11116+BerenjGram11117+BerenjGram11118)]
save(Berenj_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Berenj_Data.rda"))

