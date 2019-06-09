# 25-Food initial groups.R
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
  TF[,MorghPrice:=MorghPrice*3]
  TF[,MorghExpenditure:=MorghPrice*MorghKG]
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
  TF[,CowPrice:=CowPrice*3]
  TF[,CowExpenditure:=CowPrice*CowKG]
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
  TF[,SheepPrice:=SheepPrice*3]
  TF[,SheepExpenditure:=SheepPrice*SheepKG]
  SheepData <- TF[,lapply(.SD,sum),by=HHID]
  save(SheepData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sheeps.rda"))
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
  if(year %in% 84:96){
    TF[,BerenjFExpenditure:=as.numeric(BerenjFExpenditure)]
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
  TF[,BerenjPrice:=BerenjPrice*3]
  TF[,BerenjExpenditure:=BerenjPrice*BerenjKG]
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
  TF[,RoghanPrice:=RoghanPrice*3]
  TF[,RoghanExpenditure:=RoghanPrice*RoghanKG]
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
  TF[,TokhmemorghPrice:=TokhmemorghPrice*3]
  TF[,TokhmemorghExpenditure:=TokhmemorghPrice*TokhmemorghKG]
  TokhmemorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(TokhmemorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
}

################################################
#######  Load Additional Information  #############
################################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
HHBase<-HHBase[,.(HHID,Region,ProvinceCode,Month)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
SMD<-MD[,.(HHID,Decile,Weight,Size,EqSizeRevOECD,
           Total_Exp_Month,Total_Exp_Month_Per,TFoodExpenditure,
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
              "BerenjFExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG",
              "TokhmemorghExpenditure","TokhmemorghKG")) Total[is.na(get(col)), (col) := 0]
#"MorghPrice","SheepPrice","CowPrice",
#"BerenjPrice","RoghanPrice","TokhmemorghPrice"

Total<-Total[Decile %in% 1:10]

#Price Indexes
load(file="Index_Dataset.rda")
Total<-merge(Total,Index_Dataset,by=c("Month"),all.x=TRUE)

Berenj_Azar97<-185.5
Morgh_Azar97<-151.3
Cow_Azar97<-188.5
Sheep_Azar97<-182.1
Roghan_Azar97<-159.9
Tokhmemorgh_Azar97<-218.5


##########################################
w <- c("BerenjFExpenditure","CowExpenditure",
         "SheepExpenditure","MorghExpenditure",
         "RoghanExpenditure","TokhmemorghExpenditure")

Total[, Added_Food_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
Added_Food<-Total[,.(HHID,Added_Food_Exp_Month)]

save(Added_Food,file=paste0(Settings$HEISProcessedPath,"Y",year,"Added_Food.rda"))

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
