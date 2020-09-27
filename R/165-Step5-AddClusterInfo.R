#165-Step 5- Clustering.R ----- Clusters are predefined now, not calculated here
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Saving Cluster Info===============================\n")
library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

ClusterInfo <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_GeoX))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

  MD<-merge(MD,ClusterInfo,by=c("NewArea","NewArea_Name","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,
                      "InitialPoorClustered.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"],"seconds.")