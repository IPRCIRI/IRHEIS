# House price.R
# 
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ House price =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)


PriceResults <- data.table(Year=NA_integer_,MeterPrice=NA_real_,
                           cluster3=NA_integer_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  MD<-MD[FinalPoor==1]
  #MD<-MD[Region=="Rural"]
  
  
  
  ################Cluster##################
  X1 <- MD[,.(MeterPrice=weighted.mean(MetrPrice,Weight,na.rm = TRUE)),by=cluster3]
  
  X1[,Year:=year]
  
  PriceResults <- rbind(PriceResults,X1)
  
  
}

save(PriceResults,file=paste0(Settings$HEISProcessedPath,"PriceResults.rda"))


ggplot(PriceResults)+
  geom_line(mapping = aes(x=Year,y=MeterPrice,col=factor(cluster3)))

PriceResults<-PriceResults[cluster3>7]

ggplot(PriceResults, aes(fill=cluster3==13, y=MeterPrice, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),)


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")