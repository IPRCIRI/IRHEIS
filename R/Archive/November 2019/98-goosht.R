# 21-HHFoods.R
# Builds the Food expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHFoods =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


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
  pcols <- intersect(names(TF),c("HHID","Code","MorghExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year >= 84){
    TF[,MorghExpenditure:=as.numeric(MorghExpenditure)]
  }
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  MorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(MorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Morghs.rda"))
}


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
  pcols <- intersect(names(TF),c("HHID","Code","CowExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year >= 84){
    TF[,CowExpenditure:=as.numeric(CowExpenditure)]
  }
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  CowData <- TF[,lapply(.SD,sum),by=HHID]
  save(CowData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Cows.rda"))
}



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
  pcols <- intersect(names(TF),c("HHID","Code","SheepExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year >= 84){
    TF[,SheepExpenditure:=as.numeric(SheepExpenditure)]
  }
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  SheepData <- TF[,lapply(.SD,sum),by=HHID]
  save(SheepData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sheeps.rda"))
}

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Final.rda")) 
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Foods.rda")) 

TTT<-merge(Final,FoodData,all = TRUE)
TTT<-merge(TTT,MorghData,all = TRUE)
TTT<-merge(TTT,SheepData,all = TRUE)
TTT<-merge(TTT,CowData,all = TRUE)

for (col in c("FoodExpenditure","MorghExpenditure",
              "SheepExpenditure","CowExpenditure")) 
  TTT[is.na(get(col)), (col) := 0]

TTT<-TTT[,R1:=MorghExpenditure/FoodExpenditure]
TTT<-TTT[,R2:=CowExpenditure/FoodExpenditure]
TTT<-TTT[,R3:=SheepExpenditure/FoodExpenditure]

#TTT[,weighted.mean(R1,Weight)]
TTT[FinalPoor==0 ,weighted.mean(R1,Weight)]
#TTT[,weighted.mean(R2,Weight)]
TTT[FinalPoor==0 ,weighted.mean(R2,Weight)]
#TTT[,weighted.mean(R3,Weight)]
TTT[FinalPoor==0 ,weighted.mean(R3,Weight)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
