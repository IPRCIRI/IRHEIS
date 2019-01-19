# Foods essential groups information
# 
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

################################################
################  Morgh  ########################
################################################
cat("\n\n================ Morgh =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","MorghExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,MorghExpenditure:=as.numeric(MorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,MorghKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  MorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(MorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Morghs.rda"))
}

################################################
################  Cow  ########################
################################################
cat("\n\n================ Cow =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Cow))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","CowExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,CowExpenditure:=as.numeric(CowExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,CowKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  CowData <- TF[,lapply(.SD,sum),by=HHID]
  save(CowData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Cows.rda"))
}

################################################
################  Sheep  ########################
################################################
cat("\n\n================ Sheep =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sheep))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","SheepExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,SheepExpenditure:=as.numeric(SheepExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,SheepKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  SheepData <- TF[,lapply(.SD,sum),by=HHID]
  save(SheepData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sheeps.rda"))
}

################################################
################  Berenj  ########################
################################################
cat("\n\n================ Berenj =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","BerenjExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,BerenjExpenditure:=as.numeric(BerenjExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,BerenjKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  BerenjData <- TF[,lapply(.SD,sum),by=HHID]
  save(BerenjData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Berenjs.rda"))
}

################################################
################  Roghan  ########################
################################################
cat("\n\n================ Roghan =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","RoghanExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode | Code==11511]
  if(year %in% 84:96){
    TF[,RoghanExpenditure:=as.numeric(RoghanExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,RoghanKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  RoghanData <- TF[,lapply(.SD,sum),by=HHID]
  save(RoghanData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Roghans.rda"))
}

################################################
#######  Load Additional Information  #############
################################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
HHBase<-HHBase[,.(HHID,Region,ProvinceCode)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
SMD<-SMD[,.(HHID,Decile,Weight,Dimension,EqSizeRevOECD)]

################################################
#######  Merge Information  #############
################################################

Total<-merge(HHBase,SMD,all = TRUE)
Total<-merge(Total,MorghData,all = TRUE)
Total<-merge(Total,SheepData,all = TRUE)
Total<-merge(Total,CowData,all = TRUE)
Total<-merge(Total,BerenjData,all = TRUE)
Total<-merge(Total,RoghanData,all = TRUE)

for (col in c("MorghExpenditure","MorghKG",
              "SheepExpenditure","SheepKG",
              "CowExpenditure","CowKG",
              "BerenjExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG")) Total[is.na(get(col)), (col) := 0]

Total<-Total[Decile %in% 1:10]

################################################
############  Results  #############
################################################
#Morgh
Total[,weighted.mean(MorghExpenditure,Weight),by=Decile][order(Decile)]
Total[,weighted.mean(MorghKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(MorghExpenditure,Weight),by=.(Decile,Region)][order(Region,Decile)]
Total[,weighted.mean(MorghKG,Weight),by=.(Decile,Region)][order(Region,Decile)]

#Cow
Total[,weighted.mean(CowExpenditure,Weight),by=Decile][order(Decile)]
Total[,weighted.mean(CowKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(CowExpenditure,Weight),by=.(Decile,Region)][order(Region,Decile)]
Total[,weighted.mean(CowKG,Weight),by=.(Decile,Region)][order(Region,Decile)]

#sheep
Total[,weighted.mean(SheepExpenditure,Weight),by=Decile][order(Decile)]
Total[,weighted.mean(SheepKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(SheepExpenditure,Weight),by=.(Decile,Region)][order(Region,Decile)]
Total[,weighted.mean(SheepKG,Weight),by=.(Decile,Region)][order(Region,Decile)]


#Roghan
Total[,weighted.mean(RoghanExpenditure,Weight),by=Decile][order(Decile)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(RoghanExpenditure,Weight),by=.(Decile,Region)][order(Region,Decile)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Decile,Region)][order(Region,Decile)]


#Berenj
Total[,weighted.mean(BerenjExpenditure,Weight),by=Decile][order(Decile)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(BerenjExpenditure,Weight),by=.(Decile,Region)][order(Region,Decile)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Decile,Region)][order(Region,Decile)]

#Dimension
Total[,weighted.mean(Dimension,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(Dimension,Weight),by=.(Decile,Region)][order(Region,Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
