# 29-FoodGroup-Data.R
# Builds the food group price-weight data.table for households
#
# Copyright © 2018: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ HHGhands =====================================\n")
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

load(file=paste0(Settings$HEISProcessedPath,"Y","95","GhandData11811.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","GhandData11812.rda"))
Ghand_Data<-merge(GhandData11811,GhandData11812,by =c("HHID"),all=TRUE)
Ghand_Data[is.na(Ghand_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Ghands.rda"))
#Ghand_Data<-merge(GhandData,Ghand_Data,by =c("HHID"),all=TRUE)
Ghand_Data<-Ghand_Data[,Ghandgram:=GhandGram11811+GhandGram11812]
Ghand_Data<-Ghand_Data[,GhandPrice:=(Price11811*GhandGram11811+Price11812*GhandGram11812)/(GhandGram11811+GhandGram11812)]
save(Ghand_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Ghand_Data.rda"))

cat("\n\n================ HHGoosht =====================================\n")

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


cat("\n\n================ HHHoboobat =====================================\n")

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


cat("\n\n================ HHMahi =====================================\n")

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


cat("\n\n================ HHMakarooni =====================================\n")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11161]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11161:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11161<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11161<- TF$MakarooniGram11161/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11161 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11161, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11161.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11162]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11162:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11162<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11162<- TF$MakarooniGram11162/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11162 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11162, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11162.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11163]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11163:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11163<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11163<- TF$MakarooniGram11163/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11163 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11163, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11163.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11164]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11164:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11164<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11164<- TF$MakarooniGram11164/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11164 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11164, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11164.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11165]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11165:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11165<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11165<- TF$MakarooniGram11165/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11165 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11165, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11165.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMakaroonis =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MakarooniTables[Year=="95"]
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
TF <- TF[Code==11166]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11166:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MakarooniGram11166<-TF$Kilos*1000+TF$Grams
TF$MakarooniGram11166<- TF$MakarooniGram11166/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MakarooniData11166 <- TF[,lapply(.SD,sum),by=HHID]
save(MakarooniData11166, file = paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11166.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11161.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11162.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11163.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11164.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11165.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MakarooniData11166.rda"))
Makarooni_Data<-merge(MakarooniData11161,MakarooniData11162,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11163,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11164,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11165,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-merge(MakarooniData11166,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data[is.na(Makarooni_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Makaroonis.rda"))
#Makarooni_Data<-merge(MakarooniData,Makarooni_Data,by =c("HHID"),all=TRUE)
Makarooni_Data<-Makarooni_Data[,Makaroonigram:=MakarooniGram11161+MakarooniGram11162+MakarooniGram11163+MakarooniGram11164+MakarooniGram11165+MakarooniGram11166]
Makarooni_Data<-Makarooni_Data[,MakarooniPrice:=(Price11161*MakarooniGram11161+Price11162*MakarooniGram11162+Price11163*MakarooniGram11163+Price11164*MakarooniGram11164+Price11165*MakarooniGram11165+Price11166*MakarooniGram11166)/(MakarooniGram11161+MakarooniGram11162+MakarooniGram11163+MakarooniGram11164+MakarooniGram11165+MakarooniGram11166)]
save(Makarooni_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Makarooni_Data.rda"))


cat("\n\n================ HHMast =====================================\n")

MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11424]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11424:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11424<-TF$Kilos*1000+TF$Grams
TF$MastGram11424<- TF$MastGram11424/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11424 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11424, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11424.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMasts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11425]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11425:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11425<-TF$Kilos*1000+TF$Grams
TF$MastGram11425<- TF$MastGram11425/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11425 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11425, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11425.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHMasts =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- MastTables[Year=="95"]
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
TF <- TF[Code==11426]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11426:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$MastGram11426<-TF$Kilos*1000+TF$Grams
TF$MastGram11426<- TF$MastGram11426/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
MastData11426 <- TF[,lapply(.SD,sum),by=HHID]
save(MastData11426, file = paste0(Settings$HEISProcessedPath,"Y","95","MastData11426.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11424.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11425.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MastData11426.rda"))
Mast_Data<-merge(MastData11424,MastData11425,by =c("HHID"),all=TRUE)
Mast_Data<-merge(MastData11426,Mast_Data,by =c("HHID"),all=TRUE)
Mast_Data[is.na(Mast_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Masts.rda"))
#Mast_Data<-merge(MastData,Mast_Data,by =c("HHID"),all=TRUE)
Mast_Data<-Mast_Data[,Mastgram:=MastGram11424+MastGram11425+MastGram11426]
Mast_Data<-Mast_Data[,MastPrice:=(Price11424*MastGram11424+Price11425*MastGram11425+Price11426*MastGram11426)/(MastGram11424+MastGram11425+MastGram11426)]
save(Mast_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Mast_Data.rda"))


cat("\n\n================ HHMive =====================================\n")

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


cat("\n\n================ HHMorgh =====================================\n")

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


cat("\n\n================ HHNan =====================================\n")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11141]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11141:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11141<-TF$Kilos*1000+TF$Grams
TF$NanGram11141<- TF$NanGram11141/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11141 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11141, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11141.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11142]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11142:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11142<-TF$Kilos*1000+TF$Grams
TF$NanGram11142<- TF$NanGram11142/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11142 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11142, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11142.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11143]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11143:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11143<-TF$Kilos*1000+TF$Grams
TF$NanGram11143<- TF$NanGram11143/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11143 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11143, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11143.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11144]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11144:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11144<-TF$Kilos*1000+TF$Grams
TF$NanGram11144<- TF$NanGram11144/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11144 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11144, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11144.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11151]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11151:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11151<-TF$Kilos*1000+TF$Grams
TF$NanGram11151<- TF$NanGram11151/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11151 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11151, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11151.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11152]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11152:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11152<-TF$Kilos*1000+TF$Grams
TF$NanGram11152<- TF$NanGram11152/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11152 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11152, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11152.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHNans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- NanTables[Year=="95"]
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
TF <- TF[Code==11153]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11153:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$NanGram11153<-TF$Kilos*1000+TF$Grams
TF$NanGram11153<- TF$NanGram11153/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
NanData11153 <- TF[,lapply(.SD,sum),by=HHID]
save(NanData11153, file = paste0(Settings$HEISProcessedPath,"Y","95","NanData11153.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11141.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11142.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11143.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11144.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11151.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11152.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","NanData11153.rda"))
Nan_Data<-merge(NanData11141,NanData11142,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11143,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11144,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11151,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11152,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-merge(NanData11153,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data[is.na(Nan_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Nans.rda"))
#Nan_Data<-merge(NanData,Nan_Data,by =c("HHID"),all=TRUE)
Nan_Data<-Nan_Data[,Nangram:=NanGram11141+NanGram11142+NanGram11143+NanGram11144+NanGram11151+NanGram11152+NanGram11153]
Nan_Data<-Nan_Data[,NanPrice:=(Price11141*NanGram11141+Price11142*NanGram11142+Price11143*NanGram11143+Price11144*NanGram11144+Price11151*NanGram11151+Price11152*NanGram11152+Price11153*NanGram11153)/(NanGram11141+NanGram11142+NanGram11143+NanGram11144+NanGram11151+NanGram11152+NanGram11153)]
save(Nan_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Nan_Data.rda"))


cat("\n\n================ HHPanir =====================================\n")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
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
TF <- TF[Code==11428]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11428:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11428<-TF$Kilos*1000+TF$Grams
TF$PanirGram11428<- TF$PanirGram11428/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11428 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11428, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11428.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
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
TF <- TF[Code==11429]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11429:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11429<-TF$Kilos*1000+TF$Grams
TF$PanirGram11429<- TF$PanirGram11429/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11429 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11429, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11429.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
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
TF <- TF[Code==11430]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11430:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11430<-TF$Kilos*1000+TF$Grams
TF$PanirGram11430<- TF$PanirGram11430/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11430 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11430, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11430.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
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
TF <- TF[Code==11431]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11431:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11431<-TF$Kilos*1000+TF$Grams
TF$PanirGram11431<- TF$PanirGram11431/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11431 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11431, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11431.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHPanirs =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- PanirTables[Year=="95"]
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
TF <- TF[Code==11432]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11432:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$PanirGram11432<-TF$Kilos*1000+TF$Grams
TF$PanirGram11432<- TF$PanirGram11432/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
PanirData11432 <- TF[,lapply(.SD,sum),by=HHID]
save(PanirData11432, file = paste0(Settings$HEISProcessedPath,"Y","95","PanirData11432.rda"))
#}
endtime <- proc.time()

cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11428.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11429.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11430.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11431.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","PanirData11432.rda"))
Panir_Data<-merge(PanirData11428,PanirData11429,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11430,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11431,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-merge(PanirData11432,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data[is.na(Panir_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Panirs.rda"))
#Panir_Data<-merge(PanirData,Panir_Data,by =c("HHID"),all=TRUE)
Panir_Data<-Panir_Data[,Panirgram:=PanirGram11428+PanirGram11429+PanirGram11430+PanirGram11431+PanirGram11432]
Panir_Data<-Panir_Data[,PanirPrice:=(Price11428*PanirGram11428+Price11429*PanirGram11429+Price11430*PanirGram11430+Price11431*PanirGram11431+Price11432*PanirGram11432)/(PanirGram11428+PanirGram11429+PanirGram11430+PanirGram11431+PanirGram11432)]
save(Panir_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Panir_Data.rda"))


cat("\n\n================ HHRoghan =====================================\n")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))



#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11511]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11511:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11511<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11511<- TF$RoghanGram11511/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11511 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11511, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11511.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11512]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11512:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11512<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11512<- TF$RoghanGram11512/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11512 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11512, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11512.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11521]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11521:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11521<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11521<- TF$RoghanGram11521/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11521 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11521, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11521.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11522]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11522:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11522<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11522<- TF$RoghanGram11522/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11522 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11522, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11522.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11523]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11523:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11523<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11523<- TF$RoghanGram11523/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11523 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11523, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11523.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11531]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11531:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11531<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11531<- TF$RoghanGram11531/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11531 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11531, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11531.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11532]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11532:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11532<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11532<- TF$RoghanGram11532/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11532 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11532, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11532.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHRoghans =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

#for(year in (Settings$startyear:Settings$endyear)){
#cat(paste0("\n------------------------------\nYear:",year,"\n"))
load(file=paste0(Settings$HEISRawPath,"Y","95","Raw.rda"))
ft <- RoghanTables[Year=="95"]
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
TF <- TF[Code==11533]
if("95" %in% 84:94){
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
}
TF[,Price11533:=as.numeric(Price)]
TF[,Code:=NULL]
TF[is.na(TF)] <- 0
TF$RoghanGram11533<-TF$Kilos*1000+TF$Grams
TF$RoghanGram11533<- TF$RoghanGram11533/30
TF[,Grams:=NULL]
TF[,Kilos:=NULL]
TF[,Price:=NULL]
RoghanData11533 <- TF[,lapply(.SD,sum),by=HHID]
save(RoghanData11533, file = paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11533.rda"))
#}
endtime <- proc.time()
cat("\n\n============================\nIt took ")

load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11511.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11512.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11521.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11522.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11523.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11531.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","RoghanData11532.rda"))
Roghan_Data<-merge(RoghanData11511,RoghanData11512,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11521,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11522,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11523,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11531,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11532,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-merge(RoghanData11533,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data[is.na(Roghan_Data)] <- 0
#load(file=paste0(Settings$HEISProcessedPath,"Y","95","Roghans.rda"))
#Roghan_Data<-merge(RoghanData,Roghan_Data,by =c("HHID"),all=TRUE)
Roghan_Data<-Roghan_Data[,Roghangram:=RoghanGram11511+RoghanGram11512+RoghanGram11521+RoghanGram11522+RoghanGram11523+RoghanGram11531+RoghanGram11532+RoghanGram11533]
Roghan_Data<-Roghan_Data[,RoghanPrice:=(Price11511*RoghanGram11511+Price11512*RoghanGram11512+Price11521*RoghanGram11521+Price11522*RoghanGram11522+Price11523*RoghanGram11523+Price11531*RoghanGram11531+Price11532*RoghanGram11532+Price11533*RoghanGram11533)/(RoghanGram11511+RoghanGram11512+RoghanGram11521+RoghanGram11522+RoghanGram11523+RoghanGram11531+RoghanGram11532+RoghanGram11533)]
save(Roghan_Data, file = paste0(Settings$HEISProcessedPath,"Y","95","Roghan_Data.rda"))


cat("\n\n================ HHSabzi =====================================\n")

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


cat("\n\n================ HHShir =====================================\n")

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


cat("\n\n================ HHSibzamini =====================================\n")

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


cat("\n\n================ HHTokhmemorgh =====================================\n")

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

cat("\n\n================ HHTokhmemorghs =====================================\n")


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

endtime <- proc.time()
cat("\n\n============================\nIt took ")