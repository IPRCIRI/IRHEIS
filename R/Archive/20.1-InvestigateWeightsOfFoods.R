
# Copyright © 2016: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n===========================================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(readxl)

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Food))


for(year in (Settings$startyear:Settings$endyear))
{
  ty <- FoodTables[Year==year]
  if(!is.na(ty$Grams))
  {
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    

    tab <- ty$Table
    
    UTL <- Tables[[paste0("U",year,tab)]]
    RTL <- Tables[[paste0("R",year,tab)]]
    TL <- rbind(UTL,RTL)
    
    for(n in names(TL)){
      x <- which(ty==n)
      if(length(x)>0)
        setnames(TL,n,names(ty)[x])
    }
    
    
    TL[,Kilos:=as.numeric(Kilos)]
    TL[,Grams:=as.numeric(Grams)]
    TL[,Price:=as.numeric(Price)]
    TL[,Expenditure:=as.numeric(Expenditure)]
    
    TL[,GW:=Kilos+Grams/1000]
    TL[,GWxP:=GW*Price]
    TL[,GWxP_EX:=GWxP/Expenditure]
    TL2 <- TL[!is.na(GWxP)]
    cat(paste(year,
              max(TL$Grams,na.rm = TRUE),
              nrow(TL2),nrow(TL[is.na(GWxP)]),
              nrow(TL2[GWxP==Expenditure]),
              nrow(TL2[GWxP!=Expenditure]),
              mean(TL2$GWxP_EX,na.rm = TRUE),
              TL2[GWxP!=Expenditure,.(mean(GWxP_EX))],
              TL2[GWxP!=Expenditure,.(min(GWxP_EX))],
              TL2[GWxP!=Expenditure,.(max(GWxP_EX))]
              ,"\n"))
  }
  
}
  
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
