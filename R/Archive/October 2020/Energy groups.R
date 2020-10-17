# 141-Groups Expenditures.R
# Builds the Groups Expenditures data.table for households
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ NonFood Expenditures =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

Table1<-data.table(Year=NA_integer_,Sookht_Share=NA_real_,Ab_Share=NA_real_,Bargh_Share=NA_real_,
                   Gaz_Share=NA_real_,Energy_Share=NA_real_)[0]

Table2<-data.table(Year=NA_integer_,Sookht_Share=NA_real_,Ab_Share=NA_real_,Bargh_Share=NA_real_,
                   Gaz_Share=NA_real_,Energy_Share=NA_real_,Decile=NA_real_)[0]

Table3<-data.table(Year=NA_integer_,Sookht_Share=NA_real_,Ab_Share=NA_real_,Bargh_Share=NA_real_,
                   Gaz_Share=NA_real_,Energy_Share=NA_real_,ProvinceCode=NA_real_)[0]


cat("\n\n================ Energy =====================================\n")

EnergyTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Energy))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- EnergyTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Energy_Exp"))
  TC <- TC[,pcols,with=FALSE]
  
  if(year %in% 63:82){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  }
  
  if(year %in% 83:98){
    TC <- TC[Code %in% ct$StartCode:ct$EndCode]
    TC[,Energy_Exp:=as.numeric(Energy_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  EnergyData <- TC[,lapply(.SD,sum),by=HHID]
  save(EnergyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Energys.rda"))



cat("\n\n================ Bargh =====================================\n")

BarghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Bargh))


  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- BarghTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Bargh_Exp"))
  TC <- TC[,pcols,with=FALSE]
  TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  if(year %in% 84:94){
    TC[,Bargh_Exp:=as.numeric(Bargh_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  BarghData <- TC[,lapply(.SD,sum),by=HHID]
  save(BarghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Barghs.rda"))



cat("\n\n================ Sookht =====================================\n")

SookhtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sookht))


  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- SookhtTables[Year==year]
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
  pcols <- intersect(names(TC),c("HHID","Code","Sookht_Exp"))
  TC <- TC[,pcols,with=FALSE]
  TC <- TC[Code %in% ct$StartCode:ct$EndCode]
  if(year %in% 84:94){
    TC[,Sookht_Exp:=as.numeric(Sookht_Exp)]
  }
  TC[,Code:=NULL]
  TC[is.na(TC)] <- 0
  SookhtData <- TC[,lapply(.SD,sum),by=HHID]
  save(SookhtData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sookhts.rda"))



cat("\n\n================ Ab =====================================\n")

AbTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ab))


  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- AbTables[Year==year]
  tab <- mt$Table
  if(is.na(tab))
    next
  UTM <- Tables[[paste0("U",year,tab)]]
  RTM <- Tables[[paste0("R",year,tab)]]
  TM <- rbind(UTM,RTM)
  for(n in names(TM)){
    x <- which(mt==n)
    if(length(x)>0)
      setnames(TM,n,names(mt)[x])
  }
  pcols <- intersect(names(TM),c("HHID","Code","Ab_Exp"))
  TM <- TM[,pcols,with=FALSE]
  if (year<83){
    TM <- TM[Code %in% c(32110,32255)] 
  } else {
    TM <- TM[Code %in% mt$StartCode:mt$EndCode]  
  }

  if(year %in% 84:96){
    TM[,Ab_Exp:=as.numeric(Ab_Exp)]
  }
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  AbData <- TM[,lapply(.SD,sum),by=HHID]
  save(AbData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Abs.rda"))


cat("\n\n================ Gaz =====================================\n")

GazTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Gaz))


  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  mt <- GazTables[Year==year]
  tab <- mt$Table
  if(is.na(tab))
    next
  UTM <- Tables[[paste0("U",year,tab)]]
  RTM <- Tables[[paste0("R",year,tab)]]
  TM <- rbind(UTM,RTM)
  for(n in names(TM)){
    x <- which(mt==n)
    if(length(x)>0)
      setnames(TM,n,names(mt)[x])
  }
  pcols <- intersect(names(TM),c("HHID","Code","Gaz_Exp"))
  TM <- TM[,pcols,with=FALSE]
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  if(year %in% 84:96){
    TM[,Gaz_Exp:=as.numeric(Gaz_Exp)]
  }
  TM <- TM[Code %in% mt$StartCode:mt$EndCode]
  TM[,Code:=NULL]
  TM[is.na(TM)] <- 0
  GazData <- TM[,lapply(.SD,sum),by=HHID]
  save(GazData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Gazs.rda"))

  



Data<-merge(EnergyData,SookhtData,all.x=TRUE)
Data<-merge(Data,AbData,all.x=TRUE)
Data<-merge(Data,GazData,all.x=TRUE)
Data<-merge(Data,BarghData,all.x=TRUE)
Data[is.na(Data)]<-0

load(file = paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoors.rda"))
MD<-merge(MD,Data,all.x=TRUE,by="HHID")
MD[is.na(MD)]<-0

Final<-MD[,.(HHID,Weight,Energy_Exp,Sookht_Exp,Ab_Exp,Bargh_Exp,
             Gaz_Exp,Total_Exp_Month,Decile,ProvinceCode)]

Final[,weighted.mean(Sookht_Exp/Total_Exp_Month,Weight)]
Final[,weighted.mean(Ab_Exp/Total_Exp_Month,Weight)]
Final[,weighted.mean(Bargh_Exp/Total_Exp_Month,Weight)]

Final[as.numeric(Decile)==1,weighted.mean(Energy_Exp/Total_Exp_Month,Weight)]
Final[as.numeric(Decile)==10,weighted.mean(Energy_Exp/Total_Exp_Month,Weight)]

T1<-Final[,.(Sookht_Share=weighted.mean(Sookht_Exp/Total_Exp_Month,Weight),
             Ab_Share=weighted.mean(Ab_Exp/Total_Exp_Month,Weight),
             Gaz_Share=weighted.mean(Gaz_Exp/Total_Exp_Month,Weight),
             Energy_Share=weighted.mean(Energy_Exp/Total_Exp_Month,Weight),
             Bargh_Share=weighted.mean(Bargh_Exp/Total_Exp_Month,Weight))]
T1[,Year:=year]
Table1 <- rbind(Table1,T1)

Final[,weighted.mean(Bargh_Exp/Total_Exp_Month,Weight),by=Decile][order(Decile)]
Final[,weighted.mean(Gaz_Exp/Total_Exp_Month,Weight),by=Decile][order(Decile)]

T2<-Final[,.(Bargh_Share=weighted.mean(Bargh_Exp/Total_Exp_Month,Weight),
             Gaz_Share=weighted.mean(Gaz_Exp/Total_Exp_Month,Weight),
             Ab_Share=weighted.mean(Ab_Exp/Total_Exp_Month,Weight),
             Sookht_Share=weighted.mean(Sookht_Exp/Total_Exp_Month,Weight),
             Energy_Share=weighted.mean(Energy_Exp/Total_Exp_Month,Weight)),by=Decile]


T2[,Year:=year]
Table2 <- rbind(Table2,T2)
write.csv(Table2,"Table2.csv")

T3<-Final[,.(Bargh_Share=weighted.mean(Bargh_Exp/Total_Exp_Month,Weight),
             Gaz_Share=weighted.mean(Gaz_Exp/Total_Exp_Month,Weight),
             Ab_Share=weighted.mean(Ab_Exp/Total_Exp_Month,Weight),
             Sookht_Share=weighted.mean(Sookht_Exp/Total_Exp_Month,Weight),
             Energy_Share=weighted.mean(Energy_Exp/Total_Exp_Month,Weight)),by=ProvinceCode]


T3[,Year:=year]
Table3 <- rbind(Table3,T3)
write.csv(Table3,"Table3.csv")
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
