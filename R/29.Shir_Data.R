# 21-HHShirs.R
# Builds the Shir expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHShirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


ShirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- ShirTables[Year=="95"]
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
TF <- TF[Code==11411]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11411:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$ShirGram11411<-TF$Kilos*1000+TF$Grams
TF$ShirGram11411<- TF$ShirGram11411/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
ShirData11411 <- TF[,lapply(.SD,sum),by=HHID]
save(ShirData11411, file = paste0(Settings$HEISProcessedPath,"Y","95","ShirData11411.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHShirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

ShirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- ShirTables[Year=="95"]
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
TF <- TF[Code==11412]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11412:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$ShirGram11412<-TF$Kilos*1000+TF$Grams
TF$ShirGram11412<- TF$ShirGram11412/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
ShirData11412 <- TF[,lapply(.SD,sum),by=HHID]
save(ShirData11412, file = paste0(Settings$HEISProcessedPath,"Y","95","ShirData11412.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHShirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

ShirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- ShirTables[Year=="95"]
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
TF <- TF[Code==11413]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11413:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$ShirGram11413<-TF$Kilos*1000+TF$Grams
TF$ShirGram11413<- TF$ShirGram11413/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
ShirData11413 <- TF[,lapply(.SD,sum),by=HHID]
save(ShirData11413, file = paste0(Settings$HEISProcessedPath,"Y","95","ShirData11413.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHShirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

ShirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- ShirTables[Year=="95"]
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
TF <- TF[Code==11414]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11414:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$ShirGram11414<-TF$Kilos*1000+TF$Grams
TF$ShirGram11414<- TF$ShirGram11414/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
ShirData11414 <- TF[,lapply(.SD,sum),by=HHID]
save(ShirData11414, file = paste0(Settings$HEISProcessedPath,"Y","95","ShirData11414.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")



load(file=paste0(Settings$HEISProcessedPath,"Y","95","ShirData11411.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","ShirData11412.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","ShirData11413.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","ShirData11414.rda"))
Shir_Data<-merge(ShirData11411,ShirData11412,by =c("HHID"),all=TRUE)
Shir_Data<-merge(ShirData11413,Shir_Data,by =c("HHID"),all=TRUE)
Shir_Data<-merge(ShirData11414,Shir_Data,by =c("HHID"),all=TRUE)
Shir_Data[is.na(Shir_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Shirs.rda"))
#Shir_Data<-merge(ShirData,Shir_Data,by =c("HHID"),all=TRUE)
Shir_Data<-Shir_Data[,Shirgram:=ShirGram11411+ShirGram11412+ShirGram11413+ShirGram11414]
Shir_Data<-Shir_Data[,ShirPrice:=(Price11411*ShirGram11411+Price11412*ShirGram11412+Price11413*ShirGram11413+Price11414*ShirGram11414)/(ShirGram11411+ShirGram11412+ShirGram11413+ShirGram11414)]
save(Shir_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Shir_Data.rda"))

