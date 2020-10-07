# 166-Step6-FoodPoor.R: Calculate base year basket cost in current year prices 
#                  for each cluster. and find FoodPoor
#
# Copyright © 2019-2020: Arin Shahbazian & Majid Einian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(data.table)
library(spatstat)
year<-Settings$baseBundleyear

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))

MD[,Selected_Group:=ifelse((Region=="Urban" & (Decile==2)) |
                             (Region=="Rural" & (Decile==3)),1,0)]

Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
Bfd2 <- merge(Bfd2,MD[,.(HHID,Region,Weight,Size,
                         EqSizeCalory,Selected_Group)],by="HHID")
Bfd2[is.na(Bfd2)]<-0
Bfd2[Price<0.1,Price:=NA]

BaseYearBasket <- Bfd2[Selected_Group==1,
                       .(FGramspc=weighted.mean(FGrams/EqSizeCalory,Weight*Size),
                         FKCalspc=weighted.mean(FoodKCalories/EqSizeCalory,Weight*Size)),
                       by=.(FoodType,Region)]
BaseYearBasket[,BasketCals:=sum(FKCalspc),by=Region]
BaseYearBasket[,StandardFGramspc:=FGramspc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  
  MD[,Selected_Group:=ifelse((Decile==1) | (Decile==2) |
                               (Decile==3) | (Decile==4),1,0)]
  
  Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
  Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
  Bfd2 <- merge(Bfd2,MD[,.(HHID,Region,Weight,Size,NewArea_Name,
                           EqSizeCalory,Selected_Group)],by="HHID")
  Bfd2[is.na(Bfd2)]<-0
  Bfd2[Price<0.1,Price:=NA]
  # Basket <- Bfd2[Selected_Group==1,
  #                .(FGramspc=weighted.mean(FGrams/EqSizeCalory,Weight*Size)),
  #                by=.(FoodType,NewArea_Name)]
  
  BasketPrice <- Bfd2[Selected_Group==1 & !is.na(Price),
                      .(Price=weighted.median(Price,Weight*Size,na.rm = TRUE)),
                      by=.(FoodType,Region,NewArea_Name)]
  
  BasketCost <- merge(BaseYearBasket,BasketPrice,by=c("FoodType","Region"))
  BasketCost[,Cost:=(StandardFGramspc/1000)*Price]
  FPLineBasket <- BasketCost[,.(FPLine=sum(Cost)),by=c("Region","NewArea_Name")]
  
  MD <- merge(MD,FPLineBasket,all.x=TRUE,by=c("Region","NewArea_Name"))
  
  MD[,FoodPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]


  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor2.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")