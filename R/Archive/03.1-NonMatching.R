# 03.1-NonMatching
# Print the info on non-matching HH weights
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ NonMatching ===================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

load(Settings$weightsFile)
RegionWeights <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Rough_Weights))

for(year in (Settings$startyear:Settings$endyear)){

    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
    
    D <- copy(HHBase)
    
    W <- AllWeights[Year==year]
    W[,Year:=NULL]
    W_Rough <- RegionWeights[Year==year]
    W_Rough <- W_Rough[,list(Region,Weight)]
    if(nrow(W)==0){
      D <- merge(D,W_Rough,by="Region", all.x = TRUE)  
    }else{
      D <- merge(D,W,by="HHID", all.x = TRUE)
    }
    
    cat("\n",year,":",nrow(D),
        ",",nrow(D[!is.na(Weight)]),
        ",",nrow(D[is.na(Weight)]))
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
