# 143-DurableItemsStats (to be used in calculations of scrap value)
# 
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ Durable Items Stats =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_DurableItems))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableData_Detail.rda"))
  
  DurableData_Detail[is.na(DurableData_Detail)] <- 0
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  DurableData_Detail <- merge(DurableData_Detail,DurableItems,by="Code")
  DurableData_Detail<-merge(DurableData_Detail,HHWeights,by="HHID")
  
  AVX <- DurableData_Detail[Durable_Exp>0,.(Exp=weighted.mean(Durable_Exp,Weight)),
                            by=Item]
  AVS <- DurableData_Detail[Durable_Sale>0,.(Sale=weighted.mean(Durable_Sale,Weight)),
                            by=Item]
  
  DurableBought <- dcast(DurableData_Detail[Durable_Exp>0],HHID ~ Item,value.var = "Durable_Exp",fun.aggregate = length)
  DurableSold <- dcast(DurableData_Detail[Durable_Sale>0],HHID ~ Item,value.var = "Durable_Sale",fun.aggregate = length)
  Bought <- merge(DurableBought,HHWeights,all.y = TRUE)
  Bought[is.na(Bought)] <- 0
  BoughtStats <- Bought[,lapply(.SD, weighted.mean,Weight),.SDcols=2:(ncol(Bought)-1)]
  Sold <- merge(DurableSold,HHWeights,all.y = TRUE)
  Sold[is.na(Sold)] <- 0
  SoldStats <- Sold[,lapply(.SD, weighted.mean,Weight),.SDcols=2:(ncol(Sold)-1)]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  HP <- HHHouseProperties[,.(HHID,
                             Cellphone=cellphone*1,
                             Washer=(washer|dishwasher)*1,
                             Car_Repair=car*1,
                             Car_Tire=car*1,
                             Oven=oven*1,
                             TV=(tvbw |tvcr)*1,
                             Refrigrator=(refrigerator|frez_refrig|freezer)*1,
                             Car=car*1,
                             Computer=computer*1,
                             AC=(cooler_gas |cooler_gas_movable)*1,
                             Car_Motor=car*1)]
  HP <- merge(HP,HHWeights,by="HHID")
  Has <- HP[,lapply(.SD, weighted.mean,Weight),.SDcols=2:(ncol(HP)-1)]
  Hasm <- melt(Has,measure.vars = names(Has),variable.name = "Item",value.name = "Has")
  Boughtm <- melt(BoughtStats,measure.vars = names(BoughtStats),variable.name = "Item",value.name = "Bought")
  Soldm <- melt(SoldStats,measure.vars = names(SoldStats),variable.name = "Item",value.name = "Sold")
  
  DurableItemsStats <- merge(AVX,AVS)
  DurableItemsStats <- merge(DurableItemsStats,Boughtm)
  DurableItemsStats <- merge(DurableItemsStats,Soldm)
  DurableItemsStats <- merge(DurableItemsStats,Hasm)
  save(DurableItemsStats, file=paste0(Settings$HEISProcessedPath,"Y",year,"DurableItemsStats.rda"))
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
ADT <- BigDT[,.(HasMin=min(Has),
                HasMax=max(Has),
                HasMean=mean(Has),
                HasMedian=median(Has),
                BoughtMin=min(Bought),
                BoughtMax=max(Bought),
                BoughtMean=mean(Bought),
                BoughtMedian=median(Bought)),by=Item]
ADT[,ltmin:=HasMin/BoughtMin]
ADT[,ltmax:=HasMax/BoughtMax]
ADT[,ltmean:=HasMean/BoughtMean]
ADT[,ltmedian:=HasMedian/BoughtMedian]

View(ADT)
View(BigDT)
View(BigDT[,.(LifeTimeAverage=mean(LifeTime)),by=Item])

ggplot(BigDT,aes(x=LifeTime)) +
  geom_histogram(position = "identity")+
  facet_grid(.~Item)

endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")
