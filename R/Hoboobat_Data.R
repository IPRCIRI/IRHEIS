# 21-HHHoboobats.R
# Builds the Hoboobat expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11761]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11761:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11761<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11761<- TF$HoboobatGram11761/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11761 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11761, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11761.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11762]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11762:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11762<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11762<- TF$HoboobatGram11762/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11762 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11762, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11762.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11763]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11763:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11763<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11763<- TF$HoboobatGram11763/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11763 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11763, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11763.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11764]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11764:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11764<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11764<- TF$HoboobatGram11764/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11764 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11764, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11764.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11765]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11765:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11765<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11765<- TF$HoboobatGram11765/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11765 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11765, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11765.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11766]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11766:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11766<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11766<- TF$HoboobatGram11766/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11766 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11766, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11766.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11767]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11767:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11767<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11767<- TF$HoboobatGram11767/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11767 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11767, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11767.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11768]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11768:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11768<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11768<- TF$HoboobatGram11768/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11768 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11768, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11768.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHoboobats =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- HoboobatTables[Year=="95"]
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
TF <- TF[Code==11769]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11769:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$HoboobatGram11769<-TF$Kilos*1000+TF$Grams
TF$HoboobatGram11769<- TF$HoboobatGram11769/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
HoboobatData11769 <- TF[,lapply(.SD,sum),by=HHID]
save(HoboobatData11769, file = paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11769.rda"))
#}
endtime <- proc.time()


load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11761.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11762.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11763.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11764.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11765.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11766.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11767.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11768.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HoboobatData11769.rda"))
Hoboobat_Data<-merge(HoboobatData11761,HoboobatData11762,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11763,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11764,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11765,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11766,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11767,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11768,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-merge(HoboobatData11769,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data[is.na(Hoboobat_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Hoboobats.rda"))
#Hoboobat_Data<-merge(HoboobatData,Hoboobat_Data,by =c("HHID"),all=TRUE)
Hoboobat_Data<-Hoboobat_Data[,Hoboobatgram:=HoboobatGram11761+HoboobatGram11762+HoboobatGram11763+HoboobatGram11764+HoboobatGram11765+HoboobatGram11766+HoboobatGram11767+HoboobatGram11768]
Hoboobat_Data<-Hoboobat_Data[,HoboobatPrice:=(Price11761*HoboobatGram11761+Price11762*HoboobatGram11762+Price11763*HoboobatGram11763+Price11764*HoboobatGram11764+Price11765*HoboobatGram11765+Price11766*HoboobatGram11766+Price11767*HoboobatGram11767+Price11768*HoboobatGram11768+Price11769*HoboobatGram11769)/(HoboobatGram11761+HoboobatGram11762+HoboobatGram11763+HoboobatGram11764+HoboobatGram11765+HoboobatGram11766+HoboobatGram11767+HoboobatGram11768+HoboobatGram11769)]
save(Hoboobat_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Hoboobat_Data.rda"))

