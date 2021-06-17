# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Bime =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)
library(ggplot2)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy,G125,G1253,G1251)],by="HHID")
  MD[,Decile:=NULL]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  if (year==98){
    names(Deciles)<-c("HHID","Decile","Percentile")
  }
  
  MD<-merge(MD,Deciles,by="HHID")
  
  
}

x<-MD[,weighted.mean(G1251>0,Weight),by=Decile]



endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")