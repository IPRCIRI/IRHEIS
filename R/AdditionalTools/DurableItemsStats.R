# DurableItemsStats (calculations of scrap value & depretiations)
# Run after 133-HHDurableExpenditures
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Durable Items Stats ==============================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItems))
year <- 83
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_NetDetail.rda"))
  
  load(file=paste0(Settings$HEISWeightsPath,
                   Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  DurableData_Detail<-merge(DD,HHWeights,by="HHID")
  DurableData_Detail[is.na(Item),Item:="Other"]
  table(DurableData_Detail$Item,useNA = "always")
  AVX <- DurableData_Detail[Durable_Exp>0,
                            .(Exp=weighted.mean(Durable_Exp,Weight)),
                            by=Item]
  AVS <- DurableData_Detail[Durable_Sale>0,
                            .(Sale=weighted.mean(Durable_Sale,Weight)),
                            by=Item]
  
  Boughtm <- DurableData_Detail[,.(Bought=.SD[Durable_Exp>0,sum(Weight)]/sum(HHWeights$Weight)),by=Item]
  Soldm <- DurableData_Detail[,.(Sold=.SD[Durable_Sale>0,sum(Weight)]/sum(HHWeights$Weight)),by=Item]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                                    "OwnsDurableItems.rda"))
  
  OwnsDurableItems <- merge(OwnsDurableItems,HHWeights,by="HHID")
  Has <- OwnsDurableItems[,lapply(.SD, weighted.mean,Weight),
                          .SDcols=2:(ncol(OwnsDurableItems)-1)]
  Hasm <- melt(Has,measure.vars = names(Has),
               variable.name = "Item",value.name = "Has")
  
  print(setdiff(AVX$Item,AVS$Item))
  print(setdiff(AVX$Item,AVS$Item))
  DurableItemsStats <- merge(AVX,AVS,by="Item",all = TRUE)
  DurableItemsStats <- merge(DurableItemsStats,Boughtm,all = TRUE)
  DurableItemsStats <- merge(DurableItemsStats,Soldm,all = TRUE)
  DurableItemsStats <- merge(DurableItemsStats,Hasm,all = TRUE)
  save(DurableItemsStats, file=paste0(Settings$HEISProcessedPath,"Y",year,
                                      "DurableItemsStats.rda"))
}
DurableItemsStats[,Year:=NA_integer_]
BigDT <- DurableItemsStats[0]
for(year in (Settings$startyear:Settings$endyear)){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"DurableItemsStats.rda"))
  DurableItemsStats[,Year:=year]
  BigDT <- rbind(BigDT,DurableItemsStats)
}

BigDT[,LifeTime:=Has/Bought]
BigDT[,ScrapShare:=Sale/Exp]
ADT <- BigDT[,.(HasMin=min(Has), HasMax=max(Has), 
                HasMean=mean(Has),HasMedian=median(Has),
                BoughtMin=min(Bought), BoughtMax=max(Bought),
                BoughtMean=mean(Bought), BoughtMedian=median(Bought)),by=Item]
ADT[,ltmin:=HasMin/BoughtMin]
ADT[,ltmax:=HasMax/BoughtMax]
ADT[,ltmean:=HasMean/BoughtMean]
ADT[,ltmedian:=HasMedian/BoughtMedian]

View(ADT)
#View(BigDT)
XDT <- (BigDT[Has>0,.(LifeTimeAverage=mean(LifeTime,na.rm=TRUE),
              LifeTimeMedian=median(LifeTime,na.rm=TRUE),
              ScrapShareAverage=mean(ScrapShare,na.rm=TRUE),
              ScrapShareMedian=median(ScrapShare,na.rm=TRUE)),by=Item])
View(XDT)
endTime <- proc.time()
cat("\n\n=========================\nIt took",(endTime-startTime)[3], "seconds.")
