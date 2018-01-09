# 21-HHMives.R
# Builds the Mive expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11611]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11611:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11611<-TF$Kilos*1000+TF$Grams
TF$MiveGram11611<- TF$MiveGram11611/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11611 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11611, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11611.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11612]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11612:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11612<-TF$Kilos*1000+TF$Grams
TF$MiveGram11612<- TF$MiveGram11612/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11612 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11612, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11612.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11613]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11613:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11613<-TF$Kilos*1000+TF$Grams
TF$MiveGram11613<- TF$MiveGram11613/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11613 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11613, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11613.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11614]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11614:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11614<-TF$Kilos*1000+TF$Grams
TF$MiveGram11614<- TF$MiveGram11614/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11614 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11614, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11614.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11615]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11615:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11615<-TF$Kilos*1000+TF$Grams
TF$MiveGram11615<- TF$MiveGram11615/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11615 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11615, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11615.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11616]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11616:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11616<-TF$Kilos*1000+TF$Grams
TF$MiveGram11616<- TF$MiveGram11616/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11616 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11616, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11616.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11617]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11617:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11617<-TF$Kilos*1000+TF$Grams
TF$MiveGram11617<- TF$MiveGram11617/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11617 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11617, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11617.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11618]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11618:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11618<-TF$Kilos*1000+TF$Grams
TF$MiveGram11618<- TF$MiveGram11618/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11618 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11618, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11618.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11619]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11619:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11619<-TF$Kilos*1000+TF$Grams
TF$MiveGram11619<- TF$MiveGram11619/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11619 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11619, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11619.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11621]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11621:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11621<-TF$Kilos*1000+TF$Grams
TF$MiveGram11621<- TF$MiveGram11621/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11621 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11621, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11621.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11622]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11622:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11622<-TF$Kilos*1000+TF$Grams
TF$MiveGram11622<- TF$MiveGram11622/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11622 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11622, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11622.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11623]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11623:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11623<-TF$Kilos*1000+TF$Grams
TF$MiveGram11623<- TF$MiveGram11623/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11623 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11623, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11623.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11624]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11624:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11624<-TF$Kilos*1000+TF$Grams
TF$MiveGram11624<- TF$MiveGram11624/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11624 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11624, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11624.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11625]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11625:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11625<-TF$Kilos*1000+TF$Grams
TF$MiveGram11625<- TF$MiveGram11625/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11625 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11625, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11625.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11631]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11631:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11631<-TF$Kilos*1000+TF$Grams
TF$MiveGram11631<- TF$MiveGram11631/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11631 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11631, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11631.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11632]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11632:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11632<-TF$Kilos*1000+TF$Grams
TF$MiveGram11632<- TF$MiveGram11632/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11632 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11632, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11632.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11633]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11633:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11633<-TF$Kilos*1000+TF$Grams
TF$MiveGram11633<- TF$MiveGram11633/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11633 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11633, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11633.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11634]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11634:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11634<-TF$Kilos*1000+TF$Grams
TF$MiveGram11634<- TF$MiveGram11634/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11634 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11634, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11634.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11635]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11635:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11635<-TF$Kilos*1000+TF$Grams
TF$MiveGram11635<- TF$MiveGram11635/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11635 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11635, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11635.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11641]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11641:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11641<-TF$Kilos*1000+TF$Grams
TF$MiveGram11641<- TF$MiveGram11641/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11641 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11641, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11641.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11642]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11642:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11642<-TF$Kilos*1000+TF$Grams
TF$MiveGram11642<- TF$MiveGram11642/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11642 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11642, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11642.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMives =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MiveTables[Year=="95"]
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
TF <- TF[Code==11643]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11643:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MiveGram11643<-TF$Kilos*1000+TF$Grams
TF$MiveGram11643<- TF$MiveGram11643/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MiveData11643 <- TF[,lapply(.SD,sum),by=HHID]
save(MiveData11643, file = paste0(Settings$HEISProcessedPath,"Y","95","MiveData11643.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11611.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11612.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11613.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11614.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11615.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11616.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11617.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11618.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11619.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11621.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11622.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11623.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11624.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11625.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11631.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11632.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11633.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11634.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11635.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11641.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11642.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MiveData11643.rda"))
Mive_Data<-merge(MiveData11611,MiveData11612,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11613,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11614,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11615,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11616,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11617,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11618,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11619,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11621,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11622,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11623,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11624,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11625,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11631,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11632,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11633,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11634,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11635,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11641,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11642,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-merge(MiveData11643,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data[is.na(Mive_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mives.rda"))
#Mive_Data<-merge(MiveData,Mive_Data,by =c("HHID"),all=TRUE)
Mive_Data<-Mive_Data[,Mivegram:=MiveGram11611+MiveGram11612+MiveGram11613+MiveGram11614+MiveGram11615+MiveGram11616+MiveGram11617+MiveGram11618+MiveGram11619+MiveGram11621+MiveGram11622+MiveGram11623+MiveGram11624+MiveGram11625+MiveGram11631+MiveGram11632+MiveGram11633+MiveGram11634+MiveGram11635+MiveGram11641+MiveGram11642+MiveGram11643]
Mive_Data<-Mive_Data[,MivePrice:=(Price11611*MiveGram11611+Price11612*MiveGram11612+Price11613*MiveGram11613+Price11614*MiveGram11614+Price11615*MiveGram11615+Price11616*MiveGram11616+Price11617*MiveGram11617+Price11618*MiveGram11618+Price11619*MiveGram11619+Price11621*MiveGram11621+Price11622*MiveGram11622+Price11623*MiveGram11623+Price11624*MiveGram11624+Price11625*MiveGram11625+Price11631*MiveGram11631+Price11632*MiveGram11632+Price11633*MiveGram11633+Price11634*MiveGram11634+Price11635*MiveGram11635+Price11641*MiveGram11641+Price11642*MiveGram11642+Price11643*MiveGram11643)/(MiveGram11611+MiveGram11612+MiveGram11613+MiveGram11614+MiveGram11615+MiveGram11616+MiveGram11617+MiveGram11618+MiveGram11619+MiveGram11621+MiveGram11622+MiveGram11623+MiveGram11624+MiveGram11625+MiveGram11631+MiveGram11632+MiveGram11633+MiveGram11634+MiveGram11635+MiveGram11641+MiveGram11642+MiveGram11643)]
save(Mive_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Mive_Data.rda"))

