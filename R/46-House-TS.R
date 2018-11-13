# 46-House-TS
# 
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ House =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(XLConnect)

load(Settings$weightsFile)
RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

BigD <- data.table(Region = character(0), HHID = integer(0), Year = integer(0), 
                   Quarter = integer(0), Month = integer(0), ProvinceCode = integer(0), 
                   ServiceExp = numeric(0), Weight = numeric(0))


for(year in (Settings$startyear:Settings$endyear)){
   cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"House.rda"))
  
  D <- merge(HHBase,HouseData,by="HHID", all.x = TRUE)
  

  W <- AllWeights[Year==year]
  W[,Year:=NULL]
  W_Rough <- RegionWeights[Year==year]
  W_Rough <- W_Rough[,list(Region,Weight)]
  if(nrow(W)==0){
    D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
  }else{
    D <- merge(D,W,by="HHID", all.x = TRUE)
  }
  D <- D[,list(HHID, Region, Year, Quarter, Month, ProvinceCode, ServiceExp, Weight)]
 
  D[is.na(Quarter), Quarter:=4]

  BigD <- rbind(BigD,D)
  
    # cat("\n",year,",",sum(D$ServiceExp, na.rm = TRUE),
    #     ",",sum(D$Weight, na.rm = TRUE),
    #     ",",sum(D$ServiceExp*D$Weight, na.rm = TRUE))
}
if(year %in% 63:69){
  BigD[,ServiceExp:=as.numeric(ServiceExp)]
  BigD[,ServiceExp=0]
}
S <- BigD[,list(SX=sum(ServiceExp*Weight, na.rm = TRUE)), by=list(Year,Quarter)]
         

writeWorksheetToFile(data = S, file = paste0(Settings$HEISResultsPath,"Timeseries.xlsx"),
                     sheet = "House")

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)