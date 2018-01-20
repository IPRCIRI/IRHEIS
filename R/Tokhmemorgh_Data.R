# 21-HHTokhmemorghs.R
# Builds the Tokhmemorgh expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHTokhmemorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


TokhmemorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- TokhmemorghTables[Year=="95"]
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
TF <- TF[Code==11441]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11441:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$TokhmemorghGram11441<-TF$Kilos*1000+TF$Grams
TF$TokhmemorghGram11441<- TF$TokhmemorghGram11441/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
TokhmemorghData11441 <- TF[,lapply(.SD,sum),by=HHID]
save(TokhmemorghData11441, file = paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11441.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHTokhmemorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

TokhmemorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- TokhmemorghTables[Year=="95"]
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
TF <- TF[Code==11442]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11442:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$TokhmemorghGram11442<-TF$Kilos*1000+TF$Grams
TF$TokhmemorghGram11442<- TF$TokhmemorghGram11442/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
TokhmemorghData11442 <- TF[,lapply(.SD,sum),by=HHID]
save(TokhmemorghData11442, file = paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11442.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHTokhmemorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

TokhmemorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- TokhmemorghTables[Year=="95"]
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
TF <- TF[Code==11443]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11443:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$TokhmemorghGram11443<-TF$Kilos*1000+TF$Grams
TF$TokhmemorghGram11443<- TF$TokhmemorghGram11443/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
TokhmemorghData11443 <- TF[,lapply(.SD,sum),by=HHID]
save(TokhmemorghData11443, file = paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11443.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11441.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11442.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","TokhmemorghData11443.rda"))
Tokhmemorgh_Data<-merge(TokhmemorghData11441,TokhmemorghData11442,by =c("HHID"),all=TRUE)
Tokhmemorgh_Data<-merge(TokhmemorghData11443,Tokhmemorgh_Data,by =c("HHID"),all=TRUE)
Tokhmemorgh_Data[is.na(Tokhmemorgh_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Tokhmemorghs.rda"))
#Tokhmemorgh_Data<-merge(TokhmemorghData,Tokhmemorgh_Data,by =c("HHID"),all=TRUE)
Tokhmemorgh_Data<-Tokhmemorgh_Data[,Tokhmemorghgram:=TokhmemorghGram11441+TokhmemorghGram11442+TokhmemorghGram11443]
Tokhmemorgh_Data<-Tokhmemorgh_Data[,TokhmemorghPrice:=(Price11441*TokhmemorghGram11441+Price11442*TokhmemorghGram11442+Price11443*TokhmemorghGram11443)/(TokhmemorghGram11441+TokhmemorghGram11442+TokhmemorghGram11443)]
#Tokhmemorgh_Data<-Tokhmemorgh_Data[,TokhmemorghPrice:=(Price11441^TokhmemorghGram11441*Price11442^TokhmemorghGram11442*Price11443^TokhmemorghGram11443)^(1/(TokhmemorghGram11441+TokhmemorghGram11442+TokhmemorghGram11443))]
save(Tokhmemorgh_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Tokhmemorgh_Data.rda"))

