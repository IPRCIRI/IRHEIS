# 47-Insurance-TS ##
# 
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Insurance =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(XLConnect)

RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

BigD <- data.table(HHID = integer(0), Region = character(0),  Year = integer(0), 
                   Quarter = integer(0), Month = integer(0), ProvinceCode = integer(0), 
                   Size = numeric(0), InsuredCount = numeric(0), Weight = numeric(0))


for(year in (Settings$startyear:Settings$endyear)){
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"Insurance.rda")))
    next
  
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Insurance.rda"))
  load(file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  
  D <- merge(HHBase,HHI,by="HHID",all.x = TRUE)
  D <- merge(D,InsuranceData,by="HHID", all.x = TRUE)
  
  
  W <- HHWeights
  W[,Year:=NULL]
  W_Rough <- RegionWeights[Year==year]
  W_Rough <- W_Rough[,list(Region,Weight)]
  if(nrow(W)==0){
    D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
  }else{
    D <- merge(D,W,by="HHID", all.x = TRUE)
  }
  
  D <- D[,c("HHID","Region", "Year", "Quarter", "Month", "ProvinceCode",
            "Size", "InsuredCount", "THI", "Weight"), with=FALSE]
  
  D[is.na(Quarter), Quarter:=4]
  D <- D[!is.na(Size)]
  
  D[THI>0,InsuredCount:=Size]
  D[InsuredCount>Size,InsuredCount:=Size]
  D[,THI:=NULL]
  D[is.na(InsuredCount),InsuredCount:=0]
  BigD <- rbind(BigD,D)
}

S.Q <- BigD[,.(Pop = sum(Size*Weight), Insured = sum(InsuredCount*Weight, na.rm = TRUE)),
          by = .(Year,Quarter)]
S.Y <- BigD[,.(Pop = sum(Size*Weight), Insured = sum(InsuredCount*Weight, na.rm = TRUE)),
            by = .(Year)]
S.Q[,PctInsured := Insured / Pop * 100]
S.Y[,PctInsured := Insured / Pop * 100]

writeWorksheetToFile(data = S.Q, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),
                     sheet = "Insurance.Q")
writeWorksheetToFile(data = S.Y, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),
                                                                sheet = "Insurance.Y")

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)



