# 21-HHSabzis.R
# Builds the Sabzi expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11711]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11711:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11711<-TF$Kilos*1000+TF$Grams
TF$SabziGram11711<- TF$SabziGram11711/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11711 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11711, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11711.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11712]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11712:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11712<-TF$Kilos*1000+TF$Grams
TF$SabziGram11712<- TF$SabziGram11712/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11712 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11712, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11712.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11713]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11713:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11713<-TF$Kilos*1000+TF$Grams
TF$SabziGram11713<- TF$SabziGram11713/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11713 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11713, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11713.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11714]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11714:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11714<-TF$Kilos*1000+TF$Grams
TF$SabziGram11714<- TF$SabziGram11714/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11714 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11714, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11714.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11715]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11715:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11715<-TF$Kilos*1000+TF$Grams
TF$SabziGram11715<- TF$SabziGram11715/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11715 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11715, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11715.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11716]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11716:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11716<-TF$Kilos*1000+TF$Grams
TF$SabziGram11716<- TF$SabziGram11716/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11716 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11716, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11716.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11721]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11721:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11721<-TF$Kilos*1000+TF$Grams
TF$SabziGram11721<- TF$SabziGram11721/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11721 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11721, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11721.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF$SabziGram11618<-TF$Kilos*1000+TF$Grams
TF$SabziGram11618<- TF$SabziGram11618/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11618 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11618, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11618.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11722]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11722:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11722<-TF$Kilos*1000+TF$Grams
TF$SabziGram11722<- TF$SabziGram11722/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11722 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11722, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11722.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11723]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11723:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11723<-TF$Kilos*1000+TF$Grams
TF$SabziGram11723<- TF$SabziGram11723/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11723 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11723, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11723.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11724]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11724:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11724<-TF$Kilos*1000+TF$Grams
TF$SabziGram11724<- TF$SabziGram11724/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11724 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11724, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11724.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11725]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11725:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11725<-TF$Kilos*1000+TF$Grams
TF$SabziGram11725<- TF$SabziGram11725/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11725 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11725, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11725.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11726]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11726:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11726<-TF$Kilos*1000+TF$Grams
TF$SabziGram11726<- TF$SabziGram11726/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11726 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11726, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11726.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11727]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11727:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11727<-TF$Kilos*1000+TF$Grams
TF$SabziGram11727<- TF$SabziGram11727/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11727 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11727, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11727.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")



rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11732]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11732:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11732<-TF$Kilos*1000+TF$Grams
TF$SabziGram11732<- TF$SabziGram11732/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11732 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11732, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11732.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11733]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11733:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11733<-TF$Kilos*1000+TF$Grams
TF$SabziGram11733<- TF$SabziGram11733/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11733 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11733, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11733.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11734]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11734:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11734<-TF$Kilos*1000+TF$Grams
TF$SabziGram11734<- TF$SabziGram11734/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11734 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11734, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11734.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11735]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11735:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11735<-TF$Kilos*1000+TF$Grams
TF$SabziGram11735<- TF$SabziGram11735/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11735 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11735, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11735.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11741]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11741:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11741<-TF$Kilos*1000+TF$Grams
TF$SabziGram11741<- TF$SabziGram11741/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11741 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11741, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11741.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11742]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11742:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11742<-TF$Kilos*1000+TF$Grams
TF$SabziGram11742<- TF$SabziGram11742/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11742 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11742, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11742.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11751]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11751:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11751<-TF$Kilos*1000+TF$Grams
TF$SabziGram11751<- TF$SabziGram11751/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11751 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11751, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11751.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11742]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11742:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11742<-TF$Kilos*1000+TF$Grams
TF$SabziGram11742<- TF$SabziGram11742/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11742 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11742, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11742.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11751]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11751:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11751<-TF$Kilos*1000+TF$Grams
TF$SabziGram11751<- TF$SabziGram11751/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11751 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11751, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11751.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11752]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11752:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11752<-TF$Kilos*1000+TF$Grams
TF$SabziGram11752<- TF$SabziGram11752/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11752 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11752, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11752.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11753]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11753:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11753<-TF$Kilos*1000+TF$Grams
TF$SabziGram11753<- TF$SabziGram11753/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11753 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11753, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11753.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSabzis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SabziTables[Year=="95"]
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
TF <- TF[Code==11754]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11754:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SabziGram11754<-TF$Kilos*1000+TF$Grams
TF$SabziGram11754<- TF$SabziGram11754/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SabziData11754 <- TF[,lapply(.SD,sum),by=HHID]
save(SabziData11754, file = paste0(Settings$HEISProcessedPath,"Y","95","SabziData11754.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")



load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11711.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11712.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11713.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11714.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11715.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11716.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11721.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11618.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11722.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11723.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11724.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11725.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11726.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11727.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11732.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11733.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11734.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11735.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11741.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11742.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11751.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11752.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11753.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","SabziData11754.rda"))
Sabzi_Data<-merge(SabziData11711,SabziData11712,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11713,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11714,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11715,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11716,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11721,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11618,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11722,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11723,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11724,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11725,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11726,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11727,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11732,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11733,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11734,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11735,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11741,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11742,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11751,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11752,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11753,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-merge(SabziData11754,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data[is.na(Sabzi_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sabzis.rda"))
#Sabzi_Data<-merge(SabziData,Sabzi_Data,by =c("HHID"),all=TRUE)
Sabzi_Data<-Sabzi_Data[,Sabzigram:=SabziGram11711+SabziGram11712+SabziGram11713+SabziGram11714+SabziGram11715+SabziGram11716+SabziGram11721+SabziGram11618+SabziGram11722+SabziGram11723+SabziGram11724+SabziGram11725+SabziGram11726+SabziGram11727+SabziGram11732+SabziGram11733+SabziGram11734+SabziGram11735+SabziGram11741+SabziGram11742+SabziGram11751+SabziGram11752+SabziGram11753+SabziGram11754]
Sabzi_Data<-Sabzi_Data[,SabziPrice:=(Price11711*SabziGram11711+Price11712*SabziGram11712+Price11713*SabziGram11713+Price11714*SabziGram11714+Price11715*SabziGram11715+Price11716*SabziGram11716+Price11721*SabziGram11721+Price11618*SabziGram11618+Price11722*SabziGram11722+Price11723*SabziGram11723+Price11724*SabziGram11724+Price11725*SabziGram11725+Price11726*SabziGram11726+Price11727*SabziGram11727+Price11732*SabziGram11732+Price11733*SabziGram11733+Price11734*SabziGram11734+Price11735*SabziGram11735+Price11741*SabziGram11741+Price11742*SabziGram11742+Price11751*SabziGram11751+Price11752*SabziGram11752+Price11753*SabziGram11753+Price11754*SabziGram11754)/(SabziGram11711+SabziGram11712+SabziGram11713+SabziGram11714+SabziGram11715+SabziGram11716+SabziGram11721+SabziGram11618+SabziGram11722+SabziGram11723+SabziGram11724+SabziGram11725+SabziGram11726+SabziGram11727+SabziGram11732+SabziGram11733+SabziGram11734+SabziGram11735+SabziGram11741+SabziGram11742+SabziGram11751+SabziGram11752+SabziGram11753+SabziGram11754)]
save(Sabzi_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Sabzi_Data.rda"))

