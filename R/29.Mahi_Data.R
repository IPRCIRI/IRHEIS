# 21-HHMahis.R
# Builds the Mahi expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11311]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11311:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11311<-TF$Kilos*1000+TF$Grams
TF$MahiGram11311<- TF$MahiGram11311/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11311 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11311, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11311.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11312]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11312:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11312<-TF$Kilos*1000+TF$Grams
TF$MahiGram11312<- TF$MahiGram11312/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11312 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11312, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11312.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11313]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11313:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11313<-TF$Kilos*1000+TF$Grams
TF$MahiGram11313<- TF$MahiGram11313/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11313 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11313, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11313.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11314]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11314:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11314<-TF$Kilos*1000+TF$Grams
TF$MahiGram11314<- TF$MahiGram11314/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11314 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11314, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11314.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11315]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11315:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11315<-TF$Kilos*1000+TF$Grams
TF$MahiGram11315<- TF$MahiGram11315/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11315 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11315, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11315.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11316]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11316:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11316<-TF$Kilos*1000+TF$Grams
TF$MahiGram11316<- TF$MahiGram11316/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11316 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11316, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11316.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11317]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11317:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11317<-TF$Kilos*1000+TF$Grams
TF$MahiGram11317<- TF$MahiGram11317/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11317 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11317, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11317.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11318]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11318:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11318<-TF$Kilos*1000+TF$Grams
TF$MahiGram11318<- TF$MahiGram11318/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11318 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11318, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11318.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11319]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11319:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11319<-TF$Kilos*1000+TF$Grams
TF$MahiGram11319<- TF$MahiGram11319/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11319 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11319, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11319.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMahis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MahiTables[Year=="95"]
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
TF <- TF[Code==11321]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11321:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MahiGram11321<-TF$Kilos*1000+TF$Grams
TF$MahiGram11321<- TF$MahiGram11321/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MahiData11321 <- TF[,lapply(.SD,sum),by=HHID]
save(MahiData11321, file = paste0(Settings$HEISProcessedPath,"Y","95","MahiData11321.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11311.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11312.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11313.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11314.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11315.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11316.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11317.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11318.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11319.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MahiData11321.rda"))
Mahi_Data<-merge(MahiData11311,MahiData11312,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11313,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11314,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11315,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11316,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11317,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11318,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11319,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-merge(MahiData11321,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data[is.na(Mahi_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mahis.rda"))
#Mahi_Data<-merge(MahiData,Mahi_Data,by =c("HHID"),all=TRUE)
Mahi_Data<-Mahi_Data[,Mahigram:=MahiGram11311+MahiGram11312+MahiGram11313+MahiGram11314+MahiGram11315+MahiGram11316+MahiGram11317+MahiGram11318]
Mahi_Data<-Mahi_Data[,MahiPrice:=(Price11311*MahiGram11311+Price11312*MahiGram11312+Price11313*MahiGram11313+Price11314*MahiGram11314+Price11315*MahiGram11315+Price11316*MahiGram11316+Price11317*MahiGram11317+Price11318*MahiGram11318+Price11319*MahiGram11319+Price11321*MahiGram11321)/(MahiGram11311+MahiGram11312+MahiGram11313+MahiGram11314+MahiGram11315+MahiGram11316+MahiGram11317+MahiGram11318+MahiGram11319+MahiGram11321)]
save(Mahi_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Mahi_Data.rda"))

