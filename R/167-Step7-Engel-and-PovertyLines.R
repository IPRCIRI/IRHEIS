# 167-Step7-Engel : Calculated Engel and modified Engel and Poverty Lines
# 
# Copyright © 2018-2022: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Engel =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

BigEngelTable <- data.table()

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  MD[is.na(Durable_Dep),Durable_Dep:=0]
  
  MD <- MD[,EngelH:=TOriginalFoodExpenditure/Total_Expenditure_Month]
  
  #Occasioanl Expenditure Ratio
  MD <- MD[,OEX:=Medical_Exp+Durable_Dep+Durable_NoDep+Durable_Emergency]
  MD <- MD[,OER_H:=OEX/Total_Expenditure_Month]
  
  #Durable Service Cost
  MD <- MD[,DSC_H:=OwnedDurableItemsDepreciation/Total_Expenditure_Month]
  
  EngelD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                   TOriginalFoodExpenditure_Per<1.2*FPLine,
                 .(.N,
                   Engel=weighted.mean(EngelH,Weight),
                   OER=weighted.mean(OER_H,Weight),
                   DSC=weighted.mean(DSC_H,Weight),
                   FPLine=mean(FPLine))
               ,by=.(Region,cluster3)]
  
  save(EngelD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngelD.rda"))
  
  EngelD <- EngelD[,Year:=year]

  BigEngelTable <- rbind(BigEngelTable,EngelD)

}

InflationData <- data.table(read_excel(path = Settings$InflationDataFilePath,
                                       sheet = Settings$InflationDataSheet))
InflationData[,F1 := (1+D2FoodInf)/(1+D2Inf)]
InflationData<-InflationData[order(Year)]
InflationData[,l.F1:=data.table::shift(F1)]
InflationData[,F2 := F1*l.F1]


BigEngelTable<-merge(BigEngelTable,InflationData,by="Year")
BigEngelTable<-BigEngelTable[order(Year,cluster3)]

BigEngelTable[,l.Engel:=data.table::shift(Engel),by=cluster3]
BigEngelTable[,l2.Engel:=data.table::shift(Engel,2),by=cluster3]

BigEngelTable[,EngelX:=l.Engel*F1]
BigEngelTable[,EngelX2:=l2.Engel*F2]

BigEngelTable[,l.OER:=data.table::shift(OER),by=cluster3]
BigEngelTable[,l2.OER:=data.table::shift(OER,2),by=cluster3]
BigEngelTable[,l.DSC:=data.table::shift(DSC),by=cluster3]
BigEngelTable[,l2.DSC:=data.table::shift(DSC),by=cluster3]

BigEngelTable[is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=Engel]
BigEngelTable[!is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=(Engel+EngelX)/2]
BigEngelTable[is.na(ModifiedEngel),ModifiedEngel:=(Engel+EngelX+EngelX2)/3]

BigEngelTable[,PovertyLine:=FPLine/ModifiedEngel]
#BigEngelTable[,PovertyLine0:=FPLine/Engel]

BigEngelTable[is.na(l.OER) & is.na(OER), ModOER:=OER]
BigEngelTable[!is.na(l.OER) & is.na(OER), ModOER:=(OER+l.OER)/2]
BigEngelTable[is.na(ModOER), ModOER:=(OER+l.OER+l2.OER)/3]

BigEngelTable[is.na(l.DSC) & is.na(DSC), ModDSC:=DSC]
BigEngelTable[!is.na(l.DSC) & is.na(DSC), ModDSC:=(DSC+l.DSC)/2]
BigEngelTable[is.na(ModDSC), ModDSC:=(DSC+l.DSC+l2.DSC)/3]

BigEngelTable[,CMPovLine:=PovertyLine*(1-ModOER+ModDSC)]

BigEngelTable[,.(PovertyLine,FPLine,CMPovLine)]

save(BigEngelTable,file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")
