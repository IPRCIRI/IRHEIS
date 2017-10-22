# 27-Cigar-TS
# 
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Cigar =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(XLConnect)

load(Settings$weightsFile)
RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

BigD <- data.table(Region = character(0), HHID = numeric(0), Year = integer(0), 
                   Quarter = numeric(0), Month = numeric(0), ProvinceCode = integer(0), 
                   Cigar_Exp = numeric(0), 
                   Weight = numeric(0))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  if(!file.exists(paste0(Settings$HEISProcessedPath,"Y",year,"Cigars.rda")))
  { cat("next")
    next
  }
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Cigars.rda"))
  
  D <- merge(HHBase,CigarData,by="HHID", all.x = TRUE)
  
  W <- AllWeights[Year==year]
  W[,Year:=NULL]
  W_Rough <- RegionWeights[Year==year]
  W_Rough <- W_Rough[,list(Region,Weight)]
  if(nrow(W)==0){
    D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
  }else{
    D <- merge(D,W,by="HHID", all.x = TRUE)
  }
  
  D <- D[,list(HHID, Region, Year, Quarter, Month, ProvinceCode, Cigar_Exp, Weight)]
  
  D[is.na(Quarter), Quarter:=4]
  BigD[is.na(Cigar_Exp),Cigar_Exp:=0]
  BigD <- BigD[!is.na(Weight)]
  BigD <- rbind(BigD,D)
  
  #  cat("\n",year,",",sum(D$FoodExpenditure*D$Weight))
}




Cigar.Q <- BigD[,.(SM=sum(Cigar_Exp*Weight, na.rm = TRUE)),by=.(Year,Quarter)]

Cigar.Y <- BigD[,.(SM=sum(Cigar_Exp*Weight, na.rm = TRUE)),by=Year]


writeWorksheetToFile(data = Cigar.Q, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),sheet = "Cigar.Q")

writeWorksheetToFile(data = Cigar.Y, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),sheet = "Cigar.Y")


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)