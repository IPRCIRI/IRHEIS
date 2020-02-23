#Free Durable Goods

rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)



cat("\n\n================ Free Durable Goods=====================================\n")
DurableTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Durable))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- DurableTables[Year==year]
  tab <- ct$Table
  if(is.na(tab))
    next
  UTC <- Tables[[paste0("U",year,tab)]]
  RTC <- Tables[[paste0("R",year,tab)]]
  TC <- rbind(UTC,RTC)
  for(n in names(TC)){
    x <- which(ct==n)
    if(length(x)>0)
      setnames(TC,n,names(ct)[x])
  }
  pcols <- intersect(names(TC),c("HHID","Code","BuyingMethod","Durable_Exp","Durable_Sale"))
  TC <- TC[,pcols,with=FALSE]
  TC<-TC[BuyingMethod==8]
  if(year %in% 84:97){
    TC[,FreeDurable_Exp:=as.numeric(Durable_Exp)]
    TC[,FreeDurable_Sale:=as.numeric(Durable_Sale)]
  }
  TC$FreeDurable_Exp<-TC$Durable_Exp/12
  TC$FreeDurable_Sale<-TC$Durable_Sale/12
  TC[,Code:=NULL]
  TC[,BuyingMethod:=NULL]
  TC[is.na(TC)] <- 0
  TC[,FreeDurable_Pure_Exp:=FreeDurable_Exp-FreeDurable_Sale]
  FreeDurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(FreeDurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"FreeDurableData.rda"))

  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  FreeDurableData<-merge(FreeDurableData,MD[,.(HHID,Region,ProvinceCode)])
  
  FreeD<-FreeDurableData[,.(.N),by=ProvinceCode]
  
  }




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
