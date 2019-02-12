# Calculation of EV and CV
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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","MorghPrice","MorghExpenditure"))
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

  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","CowPrice","CowExpenditure"))
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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","SheepPrice","SheepExpenditure"))
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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","BerenjPrice","BerenjExpenditure"))
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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","RoghanPrice","RoghanExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
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
################  Tokhmemorgh  ########################
################################################
cat("\n\n================ Tokhmemorgh =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))

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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","TokhmemorghPrice","TokhmemorghExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode | Code==11511]
  if(year %in% 84:96){
    TF[,TokhmemorghExpenditure:=as.numeric(TokhmemorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,TokhmemorghKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  TokhmemorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(TokhmemorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
}

################################################
#######  Load Additional Information  #############
################################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
HHBase<-HHBase[,.(HHID,Region,ProvinceCode)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
SMD<-MD[,.(HHID,Decile,Weight,Size,EqSizeRevOECD,
           Total_Exp_Month,Total_Exp_Month_Per,
           Total_Exp_Month_nondurable,Total_Exp_Month_Per_nondurable)]
           
################################################
#######  Merge Information  #############
################################################

Total<-merge(HHBase,SMD,all = TRUE)
Total<-merge(Total,MorghData,all = TRUE)
Total<-merge(Total,SheepData,all = TRUE)
Total<-merge(Total,CowData,all = TRUE)
Total<-merge(Total,BerenjData,all = TRUE)
Total<-merge(Total,RoghanData,all = TRUE)
Total<-merge(Total,TokhmemorghData,all = TRUE)

for (col in c("MorghExpenditure","MorghKG",
              "SheepExpenditure","SheepKG",
              "CowExpenditure","CowKG",
              "BerenjExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG",
              "TokhmemorghExpenditure","TokhmemorghKG")) Total[is.na(get(col)), (col) := 0]
#"MorghPrice","SheepPrice","CowPrice",
#"BerenjPrice","RoghanPrice","TokhmemorghPrice"

Total<-Total[Decile %in% 1:10]

#Price Indexes
Berenj_Esfand96<-147.7
Berenj_Azar97<-200
Morgh_Esfand96<-113.2
Morgh_Azar97<-200
Cow_Esfand96<-122.8
Cow_Azar97<-200
Sheep_Esfand96<-130.5
Sheep_Azar97<-200
Roghan_Esfand96<-111.1
Roghan_Azar97<-200
Tokhmemorgh_Esfand96<-191.4
Tokhmemorgh_Azar97<-200

#New Prices (Prices Doubled)
Total[,BerenjNewPrice:=BerenjPrice*(3-(Berenj_Azar97/Berenj_Esfand96))]
Total[,MorghNewPrice:=MorghPrice*(3-(Morgh_Azar97/Morgh_Esfand96))]
Total[,CowNewPrice:=CowPrice*(3-(Cow_Azar97/Cow_Esfand96))]
Total[,SheepNewPrice:=SheepPrice*(3-(Sheep_Azar97/Sheep_Esfand96))]
Total[,RoghanNewPrice:=RoghanPrice*(3-(Roghan_Azar97/Roghan_Esfand96))]
Total[,TokhmemorghNewPrice:=TokhmemorghPrice*(3-(Tokhmemorgh_Azar97/Tokhmemorgh_Esfand96))]

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

#Size
Total[,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(Size,Weight),by=.(Decile,Region)][order(Region,Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
