# 45-Loan-TS
# 
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Loans =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(XLConnect)

RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

BigD <- data.table(Region = character(0), HHID = numeric(0), Year = integer(0), 
                       Quarter = numeric(0), Month = numeric(0), ProvinceCode = integer(0), 
                       ServiceFee = numeric(0), HServiceFee = numeric(0),
                       Weight = numeric(0))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda")))
  { cat("next")
     next
  }
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Loans.rda"))
  
  if(year >=76)
    load(file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  else
    HHWeights <- data.table(Year=year)
  D <- merge(HHBase,LoansData,by="HHID", all.x = TRUE)
  
  W <- HHWeights
  W[,Year:=NULL]
  W_Rough <- RegionWeights[Year==year]
  W_Rough <- W_Rough[,list(Region,Weight)]
  if(nrow(W)==0){
    D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
  }else{
    D <- merge(D,W,by="HHID", all.x = TRUE)
  }
  
  D <- D[,list(HHID, Region, Year, Quarter, Month, ProvinceCode, ServiceFee, HServiceFee, Weight)]
  
  D[is.na(Quarter), Quarter:=4]
  
  BigD <- rbind(BigD,D)
  
#  cat("\n",year,",",sum(D$ServiceFee*D$Weight, na.rm = TRUE))
}

BigD <- BigD[is.na(ServiceFee),ServiceFee:=0]
BigD <- BigD[is.na(HServiceFee),HServiceFee:=0]
BigD <- BigD[!is.na(Weight)]

S.Q <- BigD[,.(SF=sum(ServiceFee*Weight, na.rm = TRUE),
                  HSF=sum(HServiceFee*Weight, na.rm = TRUE),
                  LP=weighted.mean(ifelse(ServiceFee>0,1,0),Weight, na.rm = TRUE),
                  HLP=weighted.mean(ifelse(HServiceFee>0,1,0),Weight, na.rm = TRUE)),
            by=.(Year,Quarter)]
S.Y <- BigD[,.(SF=sum(ServiceFee*Weight, na.rm = TRUE),
                  HSF=sum(HServiceFee*Weight, na.rm = TRUE),
                  LP=weighted.mean(ifelse(ServiceFee>0,1,0),Weight, na.rm = TRUE),
                  HLP=weighted.mean(ifelse(HServiceFee>0,1,0),Weight, na.rm = TRUE)),
            by=Year]

writeWorksheetToFile(data = S.Q, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),
                     sheet = "Loan.Q")
writeWorksheetToFile(data = S.Y, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),
                     sheet = "Loan.Y")


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)