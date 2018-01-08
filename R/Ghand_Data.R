# 21-HHGhands.R
# Builds the Ghand expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGhands =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


GhandTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GhandTables[Year=="95"]
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
TF <- TF[Code==11811]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11811:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GhandGram11811<-TF$Kilos*1000+TF$Grams
TF$GhandGram11811<- TF$GhandGram11811/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GhandData11811 <- TF[,lapply(.SD,sum),by=HHID]
save(GhandData11811, file = paste0(Settings$HEISProcessedPath,"Y","95","GhandData11811.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHGhands =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

GhandTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- GhandTables[Year=="95"]
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
TF <- TF[Code==11812]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11812:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$GhandGram11812<-TF$Kilos*1000+TF$Grams
TF$GhandGram11812<- TF$GhandGram11812/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
GhandData11812 <- TF[,lapply(.SD,sum),by=HHID]
save(GhandData11812, file = paste0(Settings$HEISProcessedPath,"Y","95","GhandData11812.rda"))
#}
endtime <- proc.time()

load(file=paste0(Settings$HEISProcessedPath,"Y","95","GhandData11811.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GhandData11812.rda"))
Ghand_Data<-merge(GhandData11811,GhandData11812,by =c("HHID"),all=TRUE)
Ghand_Data[is.na(Ghand_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Ghands.rda"))
#Ghand_Data<-merge(GhandData,Ghand_Data,by =c("HHID"),all=TRUE)
Ghand_Data<-Ghand_Data[,Ghandgram:=GhandGram11811+GhandGram11812]
Ghand_Data<-Ghand_Data[,GhandPrice:=(Price11811*GhandGram11811+Price11812*GhandGram11812)/(GhandGram11811+GhandGram11812)]
save(Ghand_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Ghand_Data.rda"))

