# 21-HHSibzaminis.R
# Builds the Sibzamini expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSibzaminis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


SibzaminiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sibzamini))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- SibzaminiTables[Year=="95"]
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
TF <- TF[Code==11731]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11731:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$SibzaminiGram11731<-TF$Kilos*1000+TF$Grams
TF$SibzaminiGram11731<- TF$SibzaminiGram11731/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
SibzaminiData11731 <- TF[,lapply(.SD,sum),by=HHID]
save(SibzaminiData11731, file = paste0(Settings$HEISProcessedPath,"Y","95","SibzaminiData11731.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","SibzaminiData11731.rda"))

Sibzamini_Data<-SibzaminiData11731
Sibzamini_Data[is.na(Sibzamini_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sibzaminis.rda"))
#Sibzamini_Data<-merge(SibzaminiData,Sibzamini_Data,by =c("HHID"),all=TRUE)
Sibzamini_Data<-Sibzamini_Data[,Sibzaminigram:=SibzaminiGram11731]
Sibzamini_Data<-Sibzamini_Data[,SibzaminiPrice:=(Price11731*SibzaminiGram11731)/(SibzaminiGram11731)]
save(Sibzamini_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Sibzamini_Data.rda"))

