# 02-BuildHHBase.R
# As each table in the HEIS data cover different number of households, we create
# a Base dataset for households that will be used in merging. This dataset
# contains the basic information of household that are known before the 
# questionare is filled, such as the date (Year, Quarter, Month), and geographic
# information (Province, County).
#
# Copyright © 2015: Majid Einian
# Licence: GPL-3
# 


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ BuildHHBase =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

dir.create(Settings$HEISDataPath,showWarnings = FALSE)


library(RODBC)
library(foreign)
library(data.table)

# For Years 1363 to 1386 ----------------------------------------------
for(year in 63:86)
{
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  if(year %in% 63:75){
    UD <- data.table(read.dbf(paste0(Settings$HEISSummaryPath,"1363-1375/U",year,"_SUM.DBF",sep=""),as.is = TRUE))[,c("ZONE","OSTAN","HOUSEHOLD"),with=FALSE]
    RD <- data.table(read.dbf(paste("../DataSummary/1363-1375/R",year,"_SUM.DBF",sep=""),as.is = TRUE))[,c("ZONE","OSTAN","HOUSEHOLD"),with=FALSE]
  }else if(year %in% c(76:86)){
    if(year==76){
      Query <-"Select R_U, OSTAN, BLOCK, KHANEVAR from [Sum"
    }else if(year %in% 77:82){
      Query <-  "Select R_U, OSTAN, SHAHRESTAN, BLOCK, KHANEVAR from [Sum"
    }else if(year %in% 83:86){
      Query <- "Select Address from [Sum"
    }
    cnxu <- odbcConnectExcel2007(paste("../DataSummary/13",year,"/SumU",year,".xlsx",sep=""))
    UD <- data.table(sqlQuery(cnxu,query=paste(Query,"U",year,"$]",sep="")))
    close(cnxu)
    cnxr <- odbcConnectExcel2007(paste("../DataSummary/13",year,"/SumR",year,".xlsx",sep=""))
    RD <- data.table(sqlQuery(cnxr,query=paste(Query,"R",year,"$]",sep="")))
    close(cnxr)
    rm(cnxr,cnxu)
  }
  
  UD <- UD[, lapply(.SD, as.numeric)]
  RD <- RD[, lapply(.SD, as.numeric)]
  
  
  UD[,Region:=factor(x="Urban",levels=c("Urban","Rural"),
                     labels=c("Urban","Rural"))]
  RD[,Region:=factor(x="Rural",levels=c("Urban","Rural"),
                     labels=c("Urban","Rural"))]
  
  cat(nrow(UD),", ",nrow(RD))
  
  HHBase <- rbind(RD,UD)
  if(year %in% 63:75)
  {
    HHBase[,HHID:=as.numeric(paste(ZONE,sprintf("%02d", OSTAN),sprintf("%04d", HOUSEHOLD),sep=""))]
  }
  else if(year==76){
    HHBase[,HHID:=as.numeric(paste(R_U,sprintf("%02d", OSTAN), KHANEVAR %/% 10000, 
                                   sprintf("%03d",KHANEVAR %% 1000),sep=""))]
  }else if(year %in% 77:82){
    HHBase[,HHID:=as.numeric(paste(R_U,sprintf("%02d", OSTAN), sprintf("%02d", SHAHRESTAN),KHANEVAR %/% 10000, 
                                   sprintf("%03d",KHANEVAR %% 1000),sep=""))]
  }else if(year %in% 83:86){
    setnames(HHBase,"Address","HHID")
  }
  
  HHBase[,Year:=year]
  HHBase[,Quarter:=as.numeric(HHID %% 10000 %/% 1000)]
  if(year==74)
    HHBase[,Quarter:=as.numeric(HHID %% 100000 %/% 10000)]
  HHBase[,Month:=NA_integer_]
  
  HHBase <- HHBase[,c("HHID","Region","Year","Quarter","Month"),with=FALSE]
  
#  save(HHBase,year,file=paste("Data/Y",year,"HHBase.rda",sep=""))
}

# For Years 1387 to 1392 ----------------------------------------------
for (year in 87:92)
{
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  cns<-odbcConnectAccess2007(paste("../DataRAW/",year,".mdb",sep=""))
  U <- data.table(sqlQuery(cns,paste("Select Address, MahMorajeh from U",year,ifelse(year>=92,"Data",""),sep="")))
  R <- data.table(sqlQuery(cns,paste("Select Address, MahMorajeh from R",year,ifelse(year>=92,"Data",""),sep="")))
  close(cns)
  
  U <- U[, lapply(.SD, as.numeric)]
  R <- R[, lapply(.SD, as.numeric)]
  
  setnames(U,c("HHID","Month"))
  setnames(R,c("HHID","Month"))
  
  cnxu <- odbcConnectExcel2007(paste("../DataSummary/13",year,"/SumU",year,".xlsx",sep=""))
  UD <- data.table(sqlQuery(cnxu,query=paste("Select Address from [SumU",year,"$]",sep="")))
  close(cnxu)
  cnxr <- odbcConnectExcel2007(paste("../DataSummary/13",year,"/SumR",year,".xlsx",sep=""))
  RD <- data.table(sqlQuery(cnxr,query=paste("Select Address from [SumR",year,"$]",sep="")))
  close(cnxr)
  
  UD <- UD[, lapply(.SD, as.numeric)]
  RD <- RD[, lapply(.SD, as.numeric)]
  
  setnames(UD,"HHID")
  setnames(RD,"HHID")
  
  rm(cns,cnxr,cnxu)
  
  U <- U[HHID %in% UD$HHID,]
  R <- R[HHID %in% RD$HHID,]
  
  cat(nrow(UD),", ",nrow(RD))
  rm(UD,RD)
  
  U[,Region:=factor(x="Urban",levels=c("Urban","Rural"),
                    labels=c("Urban","Rural"))]
  R[,Region:=factor(x="Rural",levels=c("Urban","Rural"),
                    labels=c("Urban","Rural"))]
  
  
  HHBase <- rbind(U,R)
  rm(U,R)
  
  HHBase[,Year:=year]
  HHBase[,Month:=ifelse(Month==1,12,Month - 1)]
  HHBase[,Quarter:=(Month-1)%/%3+1]
  
  HHBase <- HHBase[,c("HHID","Region","Year","Quarter","Month"),with=FALSE]
  
  save(HHBase,year,file=paste("Data/Y",year,"HHBase.rda",sep=""))
}

endtime <- proc.time()

cat("\n\n============================\n============================\nIt took ")
cat(endtime-starttime)
