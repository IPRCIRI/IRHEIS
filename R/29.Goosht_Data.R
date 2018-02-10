# 21-HHGooshts.R
# Builds the Goosht expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11211]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11211:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11211<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11211<- TF$GooshtGram11211/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11211 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11211, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11211.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11212]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11212:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11212<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11212<- TF$GooshtGram11212/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11212 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11212, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11212.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11213]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11213:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11213<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11213<- TF$GooshtGram11213/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11213 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11213, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11213.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11214]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11214:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11214<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11214<- TF$GooshtGram11214/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11214 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11214, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11214.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11215]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11215:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11215<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11215<- TF$GooshtGram11215/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11215 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11215, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11215.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11216]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11216:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11216<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11216<- TF$GooshtGram11216/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11216 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11216, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11216.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11217]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11217:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11217<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11217<- TF$GooshtGram11217/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11217 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11217, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11217.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11218]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11218:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11218<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11218<- TF$GooshtGram11218/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11218 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11218, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11218.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGooshts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GooshtTables[Year=="95"]
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
TF <- TF[Code==11221]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11221:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GooshtGram11221<-TF$Kilos*1000+TF$Grams
TF$GooshtGram11221<- TF$GooshtGram11221/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GooshtData11221 <- TF[,lapply(.SD,sum),by=HHID]
save(GooshtData11221, file = paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11221.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11211.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11212.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11213.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11214.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11215.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11216.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11217.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11218.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GooshtData11221.rda"))
Goosht_Data<-merge(GooshtData11211,GooshtData11212,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11213,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11214,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11215,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11216,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11217,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11218,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-merge(GooshtData11221,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data[is.na(Goosht_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Gooshts.rda"))
#Goosht_Data<-merge(GooshtData,Goosht_Data,by =c("HHID"),all=TRUE)
Goosht_Data<-Goosht_Data[,Gooshtgram:=GooshtGram11211+GooshtGram11212+GooshtGram11213+GooshtGram11214+GooshtGram11215+GooshtGram11216+GooshtGram11217+GooshtGram11218]
Goosht_Data<-Goosht_Data[,GooshtPrice:=(Price11211*GooshtGram11211+Price11212*GooshtGram11212+Price11213*GooshtGram11213+Price11214*GooshtGram11214+Price11215*GooshtGram11215+Price11216*GooshtGram11216+Price11217*GooshtGram11217+Price11218*GooshtGram11218+Price11221*GooshtGram11221)/(GooshtGram11211+GooshtGram11212+GooshtGram11213+GooshtGram11214+GooshtGram11215+GooshtGram11216+GooshtGram11217+GooshtGram11218+GooshtGram11221)]
save(Goosht_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Goosht_Data.rda"))

