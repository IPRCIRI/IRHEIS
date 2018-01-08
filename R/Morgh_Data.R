# 21-HHMorghs.R
# Builds the Morgh expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11231]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11231:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11231<-TF$Kilos*1000+TF$Grams
TF$MorghGram11231<- TF$MorghGram11231/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11231 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11231, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11231.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11232]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11232:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11232<-TF$Kilos*1000+TF$Grams
TF$MorghGram11232<- TF$MorghGram11232/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11232 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11232, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11232.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11233]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11233:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11233<-TF$Kilos*1000+TF$Grams
TF$MorghGram11233<- TF$MorghGram11233/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11233 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11233, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11233.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11234]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11234:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11234<-TF$Kilos*1000+TF$Grams
TF$MorghGram11234<- TF$MorghGram11234/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11234 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11234, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11234.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11235]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11235:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11235<-TF$Kilos*1000+TF$Grams
TF$MorghGram11235<- TF$MorghGram11235/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11235 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11235, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11235.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11236]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11236:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11236<-TF$Kilos*1000+TF$Grams
TF$MorghGram11236<- TF$MorghGram11236/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11236 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11236, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11236.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11237]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11237:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11237<-TF$Kilos*1000+TF$Grams
TF$MorghGram11237<- TF$MorghGram11237/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11237 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11237, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11237.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11238]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11238:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11238<-TF$Kilos*1000+TF$Grams
TF$MorghGram11238<- TF$MorghGram11238/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11238 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11238, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11238.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMorghs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MorghTables[Year=="95"]
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
TF <- TF[Code==11239]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11239:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MorghGram11239<-TF$Kilos*1000+TF$Grams
TF$MorghGram11239<- TF$MorghGram11239/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MorghData11239 <- TF[,lapply(.SD,sum),by=HHID]
save(MorghData11239, file = paste0(Settings$HEISProcessedPath,"Y","95","MorghData11239.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11231.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11232.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11233.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11234.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11235.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11236.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11237.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11238.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MorghData11239.rda"))
Morgh_Data<-merge(MorghData11231,MorghData11232,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11233,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11234,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11235,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11236,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11237,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11238,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-merge(MorghData11239,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data[is.na(Morgh_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Morghs.rda"))
#Morgh_Data<-merge(MorghData,Morgh_Data,by =c("HHID"),all=TRUE)
Morgh_Data<-Morgh_Data[,Morghgram:=MorghGram11231+MorghGram11232+MorghGram11233+MorghGram11234+MorghGram11235+MorghGram11236+MorghGram11237+MorghGram11238]
Morgh_Data<-Morgh_Data[,MorghPrice:=(Price11231*MorghGram11231+Price11232*MorghGram11232+Price11233*MorghGram11233+Price11234*MorghGram11234+Price11235*MorghGram11235+Price11236*MorghGram11236+Price11237*MorghGram11237+Price11238*MorghGram11238+Price11239*MorghGram11239)/(MorghGram11231+MorghGram11232+MorghGram11233+MorghGram11234+MorghGram11235+MorghGram11236+MorghGram11237+MorghGram11238+MorghGram11239)]
save(Morgh_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Morgh_Data.rda"))

