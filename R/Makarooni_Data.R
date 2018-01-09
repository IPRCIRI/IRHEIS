# 21-HHMakaroonis.R
# Builds the Makarooni expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11161]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11161:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11161<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11161<- TF$MakarooniGram11161/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11161 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11161, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11161.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11162]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11162:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11162<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11162<- TF$MakarooniGram11162/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11162 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11162, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11162.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11163]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11163:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11163<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11163<- TF$MakarooniGram11163/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11163 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11163, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11163.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11164]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11164:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11164<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11164<- TF$MakarooniGram11164/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11164 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11164, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11164.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11165]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11165:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11165<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11165<- TF$MakarooniGram11165/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11165 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11165, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11165.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11166]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11166:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11166<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11166<- TF$MakarooniGram11166/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11166 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11166, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11166.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11161.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11162.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11163.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11164.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11165.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11166.rda"))
Makarooni_Data<-merge(MakarooniData11161,MakarooniData11162,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11163,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11164,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11165,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11166,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data[is.na(Makarooni_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Makaroonis.rda"))
#Makarooni_Data<-merge(MakarooniData,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-Makarooni_Data[,Makaroonigram:=MakarooniGram11161+MakarooniGram11162+MakarooniGram11163+MakarooniGram11164+MakarooniGram11165+MakarooniGram11166]
Makarooni_Data<-Makarooni_Data[,MakarooniPrice:=(Price11161*MakarooniGram11161+Price11162*MakarooniGram11162+Price11163*MakarooniGram11163+Price11164*MakarooniGram11164+Price11165*MakarooniGram11165+Price11166*MakarooniGram11166)/(MakarooniGram11161+MakarooniGram11162+MakarooniGram11163+MakarooniGram11164+MakarooniGram11165+MakarooniGram11166)]
save(Makarooni_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Makarooni_Data.rda"))

