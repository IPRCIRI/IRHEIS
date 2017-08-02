# 04.1-SampleSize.R
# Print the Urban/Rural Sample Size for all years
#
# Copyright © 2017: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ SampleSize ===================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

# BigD <- data.table(HHID=NA,Region=NA,Year=NA,Quarter=NA,Month=NA,ProvinceCode=NA)[0]


for(year in (Settings$startyear:Settings$endyear))
{
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  D <- merge(HHBase,HHI, by="HHID")
  
  cat("\n",year,",",nrow(D),
      ",",nrow(D[Region=="Rural"]),
      ",",nrow(D[Region=="Urban"]),
      ",",sum(D$Size),
      ",",sum(D[Region=="Rural"]$Size),
      ",",sum(D[Region=="Urban"]$Size))
 
  #rm(HHBase)
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
