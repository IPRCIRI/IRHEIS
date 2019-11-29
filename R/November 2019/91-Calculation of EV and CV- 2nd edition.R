# 91-Calculation of EV and CV- 2nd edition
# 
#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)
options(warn=-1)

################################################
################  Shir  ########################
################################################
cat("\n\n================ Shir =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))

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
  
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","ShirPrice","ShirExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,ShirExpenditure:=as.numeric(ShirExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,ShirPrice:=as.numeric(ShirPrice)]
  TF[is.na(TF)] <- 0
  TF[,ShirKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  ShirData <- TF[,lapply(.SD,sum),by=HHID]
  save(ShirData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Shirs.rda"))
}

################################################
################  Mast  ########################
################################################
cat("\n\n================ Mast =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))

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
  
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","MastPrice","MastExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,MastExpenditure:=as.numeric(MastExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,MastPrice:=as.numeric(MastPrice)]
  TF[is.na(TF)] <- 0
  TF[,MastKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  MastData <- TF[,lapply(.SD,sum),by=HHID]
  save(MastData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Masts.rda"))
}

################################################
################  Panir  ########################
################################################
cat("\n\n================ Panir =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))

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
  
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","PanirPrice","PanirExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,PanirExpenditure:=as.numeric(PanirExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,PanirPrice:=as.numeric(PanirPrice)]
  TF[is.na(TF)] <- 0
  TF[,PanirKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  PanirData <- TF[,lapply(.SD,sum),by=HHID]
  save(PanirData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Panirs.rda"))
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
  TF[,CowPrice:=as.numeric(CowPrice)]
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
  TF[,SheepPrice:=as.numeric(Grams)]
  TF[,SheepPrice:=as.numeric(SheepPrice)]
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
  if(year %in% 84:97){
    TF[,MorghExpenditure:=as.numeric(MorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,MorghPrice:=as.numeric(MorghPrice)]
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
################  Ghand  ########################
################################################
cat("\n\n================ Ghand =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))

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
  
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","GhandPrice","GhandExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:97){
    TF[,GhandExpenditure:=as.numeric(GhandExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,GhandPrice:=as.numeric(GhandPrice)]
  TF[is.na(TF)] <- 0
  TF[,GhandKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  GhandData <- TF[,lapply(.SD,sum),by=HHID]
  save(GhandData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Ghands.rda"))
}

################################################
################  Shekar  ########################
################################################
cat("\n\n================ Shekar =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shekar))

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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","ShekarPrice","ShekarExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:97){
    TF[,ShekarExpenditure:=as.numeric(ShekarExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,ShekarPrice:=as.numeric(ShekarPrice)]
  TF[is.na(TF)] <- 0
  TF[,ShekarKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  ShekarData <- TF[,lapply(.SD,sum),by=HHID]
  save(ShekarData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Shekars.rda"))
}
################################################
################  Berenj  ########################
################################################
cat("\n\n================ Berenj =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_BerenjF))

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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","BerenjPrice","BerenjFExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:97){
    TF[,BerenjFExpenditure:=as.numeric(BerenjFExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,BerenjPrice:=as.numeric(BerenjPrice)]
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
  if(year %in% 84:97){
    TF[,RoghanExpenditure:=as.numeric(RoghanExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,RoghanPrice:=as.numeric(RoghanPrice)]
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
  if(year %in% 84:97){
    TF[,TokhmemorghExpenditure:=as.numeric(TokhmemorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,TokhmemorghPrice:=as.numeric(TokhmemorghPrice)]
  TF[is.na(TF)] <- 0
  TF[,TokhmemorghKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  TokhmemorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(TokhmemorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
}

cat("\n\n================ Merge =====================================\n")
################################################
#######  Load Additional Information  #############
################################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
HHBase<-HHBase[,.(HHID,Region,ProvinceCode,Month)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
SMD<-MD[,.(HHID,Decile,Weight,Size,EqSizeOECD,
           Total_Exp_Month,Total_Exp_Month_Per,TOriginalFoodExpenditure)]
#Total_Exp_Month_nondurable,Total_Exp_Month_Per_nondurable)]

################################################
#######  Merge Information  #############
################################################

Total<-merge(HHBase,SMD,all = TRUE)
Total<-merge(Total,MorghData,all = TRUE)
Total<-merge(Total,ShekarData,all = TRUE)
Total<-merge(Total,GhandData,all = TRUE)
Total<-merge(Total,BerenjData,all = TRUE)
Total<-merge(Total,RoghanData,all = TRUE)
Total<-merge(Total,TokhmemorghData,all = TRUE)
Total<-merge(Total,CowData,all = TRUE)
Total<-merge(Total,SheepData,all = TRUE)
Total<-merge(Total,ShirData,all = TRUE)
Total<-merge(Total,MastData,all = TRUE)
Total<-merge(Total,PanirData,all = TRUE)

for (col in c("MorghExpenditure","MorghKG",
              "ShekarExpenditure","ShekarKG",
              "GhandExpenditure","GhandKG",
              "BerenjFExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG",
              "TokhmemorghExpenditure","TokhmemorghKG",
              "CowExpenditure","CowKG",
              "ShirExpenditure","ShirKG",
              "MastExpenditure","MastKG",
              "PanirExpenditure","PanirKG",
              "SheepExpenditure","SheepKG")) Total[is.na(get(col)), (col) := 0]


Total<-Total[Decile %in% 1:10]

#Price Indexes
load(file="Index_Dataset97.rda")
Total<-merge(Total,Index_Dataset97,by=c("Region","Month"),all.x=TRUE)

Berenj_Esfand96<-114.4
Morgh_Esfand96<-122.1
Ghand_Esfand96<-108.4
Shekar_Esfand96<-108.4
Roghan_Esfand96<-113.7
Tokhmemorgh_Esfand96<-120.9 
Panir_Esfand96<-120.9 
Shir_Esfand96<-120.9 
Mast_Esfand96<-120.9 
Cow_Esfand96<-122.1
Sheep_Esfand96<-122.1

Berenj_Esfand97<-145.6
Morgh_Esfand97<-265.6
Ghand_Esfand97<-170.3
Shekar_Esfand97<-170.3
Roghan_Esfand97<-168
Tokhmemorgh_Esfand97<-175.3
Panir_Esfand97<-175.3
Shir_Esfand97<-175.3
Mast_Esfand97<-175.3
Cow_Esfand97<-265.6 
Sheep_Esfand97<-265.6



############New Prices###########
Total[,BerenjP0:=BerenjPrice*Berenj_Esfand96/BerenjIndex]
Total[,MorghP0:=MorghPrice*Morgh_Esfand96/MorghIndex]
Total[,GhandP0:=GhandPrice*Ghand_Esfand96/GhandIndex]
Total[,ShekarP0:=ShekarPrice*Ghand_Esfand96/GhandIndex]
Total[,RoghanP0:=RoghanPrice*Roghan_Esfand96/RoghanIndex]
Total[,TokhmemorghP0:=TokhmemorghPrice*Tokhmemorgh_Esfand96/EggIndex]
Total[,SheepP0:=SheepPrice*Sheep_Esfand96/SheepIndex]
Total[,CowP0:=CowPrice*Cow_Esfand96/CowIndex]
Total[,ShirP0:=ShirPrice*Shir_Esfand96/ShirIndex]
Total[,MastP0:=MastPrice*Mast_Esfand96/MastIndex]
Total[,PanirP0:=PanirPrice*Panir_Esfand96/PanirIndex]

Total[,BerenjP1:=BerenjPrice*Berenj_Esfand97/BerenjIndex]
Total[,MorghP1:=MorghPrice*Morgh_Esfand97/MorghIndex]
Total[,GhandP1:=GhandPrice*Ghand_Esfand97/GhandIndex]
Total[,ShekarP1:=ShekarPrice*Ghand_Esfand97/GhandIndex]
Total[,RoghanP1:=RoghanPrice*Roghan_Esfand97/RoghanIndex]
Total[,TokhmemorghP1:=TokhmemorghPrice*Tokhmemorgh_Esfand97/EggIndex]
Total[,PanirP1:=PanirPrice*Panir_Esfand97/PanirIndex]
Total[,MastP1:=MastPrice*Mast_Esfand97/MastIndex]
Total[,ShirP1:=ShirPrice*Shir_Esfand97/ShirIndex]
Total[,CowP1:=CowPrice*Cow_Esfand97/CowIndex]
Total[,SheepP1:=SheepPrice*Sheep_Esfand97/SheepIndex]

Total[,BerenjP2:=BerenjP0*2.57]
Total[,MorghP2:=MorghP0*2.57]
Total[,GhandP2:=GhandP0*2.57]
Total[,ShekarP2:=ShekarP0*2.57]
Total[,RoghanP2:=RoghanP0*2.57]
Total[,TokhmemorghP2:=TokhmemorghP0*2.57]
Total[,SheepP2:=SheepP0*2.57]
Total[,CowP2:=CowP0*2.57]
Total[,ShirP2:=ShirP0*2.57]
Total[,MastP2:=MastP0*2.57]
Total[,PanirP2:=PanirP0*2.57]

################################################
################  CV and EV  #################
################################################

Total<-Total[,GExpenditures:=BerenjFExpenditure+GhandExpenditure+
               ShekarExpenditure+MorghExpenditure+
               RoghanExpenditure+TokhmemorghExpenditure+
               ShirExpenditure+MastExpenditure+
               CowExpenditure+PanirExpenditure+
               SheepExpenditure]
GExpenditures<-Total[,weighted.mean(GExpenditures,Weight)]


Total<-Total[,BerenjShare:=BerenjFExpenditure/GExpenditures]
Total<-Total[,GhandShare:=GhandExpenditure/GExpenditures]
Total<-Total[,ShekarShare:=ShekarExpenditure/GExpenditures]
Total<-Total[,MorghShare:=MorghExpenditure/GExpenditures]
Total<-Total[,RoghanShare:=RoghanExpenditure/GExpenditures]
Total<-Total[,TokhmemorghShare:=TokhmemorghExpenditure/GExpenditures]
Total<-Total[,PanirShare:=PanirExpenditure/GExpenditures]
Total<-Total[,ShirShare:=ShirExpenditure/GExpenditures]
Total<-Total[,MastShare:=MastExpenditure/GExpenditures]
Total<-Total[,CowShare:=CowExpenditure/GExpenditures]
Total<-Total[,SheepShare:=SheepExpenditure/GExpenditures]


Total<-Total[,CV2:=GExpenditures*(1-((GhandP2/GhandP1)^GhandShare)*((ShekarP2/ShekarP1)^ShekarShare)*
                                    ((BerenjP2/BerenjP1)^BerenjShare)*((RoghanP2/RoghanP1)^RoghanShare)*
                                    ((MorghP2/MorghP1)^MorghShare)*((TokhmemorghP2/TokhmemorghP1)^TokhmemorghShare)*
                                    ((SheepP2/SheepP1)^SheepShare)*((CowP2/CowP1)^CowShare)*
                                    ((ShirP2/ShirP1)^ShirShare)*((MastP2/MastP1)^MastShare)*
                                    ((PanirP2/PanirP1)^PanirShare))]

Total[,weighted.mean(CV2,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]


Total<-Total[,CV3:=GExpenditures*(1-((GhandP2/GhandP0)^GhandShare)*((ShekarP2/ShekarP0)^ShekarShare)*
                                    ((BerenjP2/BerenjP0)^BerenjShare)*((RoghanP2/RoghanP0)^RoghanShare)*
                                    ((MorghP2/MorghP0)^MorghShare)*((TokhmemorghP2/TokhmemorghP0)^TokhmemorghShare)*
                                    ((SheepP2/SheepP0)^SheepShare)*((CowP2/CowP0)^CowShare)*
                                    ((ShirP2/ShirP0)^ShirShare)*((MastP2/MastP0)^MastShare)*
                                    ((PanirP2/PanirP0)^PanirShare))]

Total[,weighted.mean(CV3,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]


################################################
############  Other  #############
################################################
BerenjShare<-Total[,weighted.mean(BerenjFExpenditure,
                                  Weight)/weighted.mean(GExpenditures,Weight)]

GhandShare<-Total[,weighted.mean(GhandExpenditure,
                               Weight)/weighted.mean(GExpenditures,Weight)]

ShekarShare<-Total[,weighted.mean(ShekarExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

MorghShare<-Total[,weighted.mean(MorghExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

RoghanShare<-Total[,weighted.mean(RoghanExpenditure,
                                  Weight)/weighted.mean(GExpenditures,Weight)]

TokhmemorghShare<-Total[,weighted.mean(TokhmemorghExpenditure,
                                       Weight)/weighted.mean(GExpenditures,Weight)]


Total[,weighted.mean(Size,Weight)]
Total[,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]
Total[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]

Total[,weighted.mean(BerenjKG,Weight)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(RoghanKG,Weight)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(MorghKG,Weight)]
Total[,weighted.mean(MorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(MorghKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(GhandKG,Weight)]
Total[,weighted.mean(GhandKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(GhandKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(ShekarKG,Weight)]
Total[,weighted.mean(ShekarKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(ShekarKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(CowKG,Weight)]
Total[,weighted.mean(CowKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(CowKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(SheepKG,Weight)]
Total[,weighted.mean(SheepKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(SheepKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(TokhmemorghKG,Weight)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Decile)][order(Decile)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
