# 141-Groups Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ NonFood Expenditures =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

sections_names <- c(#"Cigar","Cloth","Communication","Energy","Furniture",
                    #"Hygiene","Medical","Transportation","Communication",
                    #"Amusement","Education",
  "Hotel","Restaurant")
                # Other sections have their own code
for(section in sections_names){
section_sheet <- eval(parse(text = paste0("Settings$MDS_",section)))
cat("\n\n================",section,"=====================================\n")
SectionTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=section_sheet))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  st <- SectionTables[Year==year]
  tab <- st$Table
  if(is.na(tab))
    next
  UTS <- Tables[[paste0("U",year,tab)]]
  RTS <- Tables[[paste0("R",year,tab)]]
  TS <- rbind(UTS,RTS)
  for(n in names(TS)){
    x <- which(st==n)
    if(length(x)>0)
      setnames(TS,n,names(st)[x])
  }
  pcols <- intersect(names(TS),c("HHID","Code",paste0(section,"_Exp")))
  TS <- TS[,pcols,with=FALSE]
  if(!is.na(st$StartCode)){
    TS <- TS[Code %in% st$StartCode:st$EndCode]
  }
  
  TS[,(paste0(section,"_Exp")):=as.numeric(get(paste0(section,"_Exp")))]

  TS[,Code:=NULL]
  TS[is.na(TS)] <- 0
  
  eval(parse(text = paste0(section,"Data <- TS[,lapply(.SD,sum),by=HHID]")))
  eval(parse(text = paste0("save(",section,"Data, file =paste0(Settings$HEISProcessedPath,\"Y\",year,\"",section,"s.rda\"))")))
  eval(parse(text = paste0("cat(section,\":\",",section,"Data[,mean(",section,"_Exp)])")))
}


}



cat("\n\n================ Section4:HHHouse =====================================\n")

HouseTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_House))
p0<-0
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  ty <- HouseTables[Year==year][,1:7]
  tab <- ty$Table
  
  UTL <- Tables[[paste0("U",year,tab)]]
  RTL <- Tables[[paste0("R",year,tab)]]
  TL <- rbind(UTL,RTL)
  for(n in names(TL)){
    x <- which(ty==n)
    if(length(x)>0)
      setnames(TL,n,names(ty)[x])
  }
  pcols <- intersect(names(TL),c("HHID","Code","ServiceExp"))
  TL <- TL[,pcols,with=FALSE]
  TL <- TL[Code %in% ty$StartCode:ty$EndCode]
  TL[,ServiceExp:=as.numeric(ServiceExp)]
  
  TL[,Code:=NULL]
  
  
  TL[is.na(TL)] <- 0
  
  HouseData <- TL[,lapply(.SD,sum),by=HHID]
  

  # cat("\n",year,",",mean(HouseData$ServiceExp,na.rm = TRUE))
  ty <- HouseTables[Year==year]
  rt <- Tables[[paste0("R",year,ty$HATable)]]
  ut <- Tables[[paste0("U",year,ty$HATable)]]
  
  if(year <= 68){
    rt$New <- NA
    setnames(rt,"New",ty$HACode)
  }
  ns <- c(ty$HAHHID,ty$HRCode,ty$HACode)
  
  TRA <- rbind( ut[,ns,with=FALSE],rt[,ns,with=FALSE] )
  
  setnames(TRA,ty$HAHHID,"HHID")
  setnames(TRA,ty$HRCode,"Rooms")
  setnames(TRA,ty$HACode,"Area")
  
  TRA[Area==0,Area:=NA]
  
  HouseData <- merge(HouseData,TRA,by = "HHID", all = TRUE)
  HouseData$MetrPrice <-HouseData$ServiceExp/HouseData$Area
  HouseData <- HouseData[!is.na(MetrPrice)]
  save(HouseData, file = paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
  # cat(summary(HouseData[,ServiceExp/Area]))
 # cat(HouseData[,mean(MetrPrice)]/p0-1,"\n")
#  cat(HouseData[,median(MetrPrice)],"\n")
#  cat(HouseData[,mean(ServiceExp)],"\n")
#  cat(HouseData[,median(ServiceExp)],"\n")
  p0 <- HouseData[,mean(MetrPrice)]

}





cat("\n\n================ Section12:HHOther =====================================\n")

OtherTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Other))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- OtherTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Other_Exp"))
  TC <- TC[,pcols,with=FALSE]
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  if(year %in% 84:94){
    TC[,Other_Exp:=as.numeric(Other_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  OtherData <- TC[,lapply(.SD,sum),by=HHID]
  save(OtherData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Others.rda"))
  cat(OtherData[,mean(Other_Exp)])
  }



cat("\n\n================ Section13:HHDurable =====================================\n")
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
  
  #TC <- TC[Code %in% ct$StartCode:ct$EndCode]

  if(year %in% 84:98){
    TC[,Durable_Exp:=as.numeric(Durable_Exp)/12]
   TC[,Durable_Sale:=as.numeric(Durable_Sale)/12]
  }
  #TC<-TC[BuyingMethod==1]
  TC$Durable_Exp<-TC$Durable_Exp
  TC$Durable_Sale<-TC$Durable_Sale
  DurableDataCodes<-TC
  save(DurableDataCodes, file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableDataCodes.rda"))
  
  TC[,Code:=NULL]
  TC[,BuyingMethod:=NULL]
  TC[is.na(TC)] <- 0
  TC[,Durable_Pure_Exp:=Durable_Exp-Durable_Sale]
  DurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(DurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
  cat(DurableData[,mean(Durable_Exp)],"\t")
  cat(DurableData[,mean(Durable_Sale)],"\t")
  cat(DurableData[,mean(Durable_Pure_Exp)],"\t")
  }


cat("\n\n================ Section14:HHInvestment =====================================\n")

InvestmentTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Investment))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- InvestmentTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Investment_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 84:94){
    TC[,Investment_Exp:=as.numeric(Investment_Exp)]
  }
  TC$Investment_Exp<-TC$Investment_Exp/12
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  InvestmentData <- TC[,lapply(.SD,sum),by=HHID]
  save(InvestmentData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Investments.rda"))
  cat(InvestmentData[,mean(Investment_Exp)],"\t")
  }


endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")
