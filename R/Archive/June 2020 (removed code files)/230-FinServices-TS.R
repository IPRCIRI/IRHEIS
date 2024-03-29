# 230-FinServices-TS ##
# 
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ FinServices =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(XLConnect)

RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

BigD <- data.table(HHID = integer(0), Region = character(0),  Year = integer(0), 
                   Quarter = integer(0), Month = integer(0), ProvinceCode = integer(0), 
                   Size = numeric(0), FinServicesExp=numeric(0), Weight = numeric(0))


for(year in (Settings$startyear:Settings$endyear)){
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"FinServices.rda")))
    next
  
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinServices.rda"))
  load(file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  
  D <- merge(HHBase,HHI,by="HHID",all.x = TRUE)
  D <- merge(D,FinServicesData,by="HHID", all.x = TRUE)
  
  
  W <- data.table(HHWeights)
  W[,Year:=NULL]
  W[,HHID:=as.numeric(HHID)]
  W_Rough <- RegionWeights[Year==year]
  W_Rough <- W_Rough[,list(Region,Weight)]
  if(nrow(W)==0){
    D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
  }else{
    D <- merge(D,W,by="HHID", all.x = TRUE)
  }
  
  D <- D[,c("HHID","Region", "Year", "Quarter", "Month", "ProvinceCode",
            "CountyCode","County_Name",
            "Size", "FinServicesExp", "Weight"), with=FALSE]
  
  D[is.na(Quarter), Quarter:=4]
  D <- D[!is.na(Size)]
  
  BigD <- rbind(BigD,D)
}


BigD[,HasFinService:=ifelse(is.na(FinServicesExp),0,1)]
S.Q.P <- BigD[,.(SS=.N,
                 Pop = sum(Size*Weight),
               FinServicesExp = sum(FinServicesExp*Weight, na.rm = TRUE),
               HasFinService = sum(HasFinService*Weight)),
          by = .(Year,Quarter,ProvinceCode)][order(ProvinceCode,Year,Quarter)]
S.Y.P <- S.Q.P <- BigD[,.(SS=.N,
                          Pop = sum(Size*Weight),
                          FinServicesExp = sum(FinServicesExp*Weight, na.rm = TRUE),
                          HasFinService = sum(HasFinService*Weight)),
                       by = .(Year,ProvinceCode)][order(ProvinceCode,Year)]

S.Q.P[,PctFinService := HasFinService / Pop * 100]
S.Q.P[,PcpFinService := FinServicesExp / Pop * 100]
S.Y.P[,PctFinService := HasFinService / Pop * 100]
S.Y.P[,PcpFinService := FinServicesExp / Pop * 100]

writeWorksheetToFile(data = S.Q.P, file = paste0(Settings$HEISResultsPath,"Panel.xlsx"),
                     sheet = "FinServices.Q")
writeWorksheetToFile(data = S.Y.P, file = paste0(Settings$HEISResultsPath,"Panel.xlsx"),
                                                                sheet = "FinServices.Y")

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)



