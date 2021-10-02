# 167-Step7-Engel : Calculated Engel and modified Engel and Poverty Lines
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Engel =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

BigEngelTable <- data.table(Region=NA_character_,NewArea_Name=NA_integer_,
                            N=NA_integer_,Engel=NA_real_,
                            FPLine=NA_real_,Year=NA_integer_,ProvinceCode=NA_integer_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor2.rda"))
  
  MD<-MD[,EngelH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  EngelD <- MD[ TOriginalFoodExpenditure_Per>0.7*FPLine &
                   TOriginalFoodExpenditure_Per<1.3*FPLine,
                 .(.N,Engel=weighted.mean(EngelH,Weight),
                   FPLine=mean(FPLine))
               ,by=.(Region,NewArea_Name,ProvinceCode)]
  
  save(EngelD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngelD.rda"))

  EngelD<-EngelD[,Year:=year]
  BigEngelTable <- rbind(BigEngelTable,EngelD)
}

InflationData <- data.table(read_excel(path = Settings$InflationDataFilePath))
InflationData[,F1 := (1+D2FoodInf)/(1+D2Inf)]
InflationData<-InflationData[order(Year)]
InflationData[,l.F1:=data.table::shift(F1)]
InflationData[,F2 := F1*l.F1]


BigEngelTable<-merge(BigEngelTable,InflationData,by="Year")
BigEngelTable<-BigEngelTable[order(Year,NewArea_Name)]
BigEngelTable[,l.Engel:=data.table::shift(Engel),by=NewArea_Name]
BigEngelTable[,l2.Engel:=data.table::shift(Engel,2),by=NewArea_Name]

BigEngelTable[,EngelX:=l.Engel*F1]
BigEngelTable[,EngelX2:=l2.Engel*F2]


BigEngelTable[is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=Engel]
BigEngelTable[!is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=(Engel+EngelX)/2]
BigEngelTable[is.na(ModifiedEngel),ModifiedEngel:=(Engel+EngelX+EngelX2)/3]
BigEngelTable[,PovertyLine:=FPLine/ModifiedEngel]
BigEngelTable[,PovertyLine0:=FPLine/Engel]

save(BigEngelTable,file=paste0(Settings$HEISProcessedPath,"BigEngelTable2.rda"))




endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")