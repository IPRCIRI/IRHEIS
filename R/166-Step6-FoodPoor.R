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
library(spatstat)
library(data.table)
BigsdTable <- data.table()
year<-Settings$baseBundleyear

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))

MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==4) |
                             (Region=="Rural" & Decile==3),1,0)]

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
  
  MD[,Selected_Group:=ifelse((Decile==5) | (Decile==2) |
                               (Decile==3) | (Decile==4),1,0)]
  
  #Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
  #Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
  Bfd2 <- merge(BigFData,MD[,.(HHID,Region,Weight,Size,cluster3,
                           EqSizeCalory,Selected_Group)],by="HHID")
  Bfd2[is.na(Bfd2)]<-0
  Bfd2[Price<0.1,Price:=NA]
  Bfd3 <- Bfd2[Selected_Group==1 & !is.na(Price),
               .(MedPrice=weighted.median(Price,Weight*Size*FGrams,na.rm = TRUE),
                 MeanPrice=weighted.mean(Price,Weight*Size*FGrams,na.rm = TRUE)),
               by=.(FoodType,Region,cluster3)]
  BasketPrice <- Bfd3[!is.na(MeanPrice),
                      .(Price=min(MeanPrice)),
                      by=.(FoodType,Region,cluster3)]
  
  BasketCost <- merge(BaseYearBasket,BasketPrice,by=c("FoodType","Region"))
  BasketCost[,Cost:=(StandardFGramspc/1000)*Price]
  FPLineBasket <- BasketCost[,.(FPLine=sum(Cost)),by=cluster3]
  
  MD <- merge(MD,FPLineBasket,all.x=TRUE,by="cluster3")
  sd <- MD
  sd <- sd[,FPLineBasketyear:=weighted.mean(FPLine)]
    sd <-sd[,Year:=year]
  sd <- unique(sd[,.(Year,FPLineBasketyear)])
  BigsdTable <- rbind(BigsdTable,sd)
  
  
  MD[,FoodPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]

  cat(unlist(MD[cluster3==13,.(FPLine,weighted.mean(FoodPoor))][1]))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  
  Meat<-BigFData[FoodType=="Meat",.(HHID,FoodType,FGrams)]
  Meat2 <- Meat[,lapply(.SD,sum),by=HHID,.SDcols=c("FGrams")]
  Meat2<-merge(MD[,.(HHID,Size,EqSizeCalory,Weight)],Meat2,all.x=TRUE)
  Meat2[is.na(Meat2)]<-0
  cat(Meat2[,weighted.mean(FGrams/EqSizeCalory,Weight*Size)])
  cat(Meat2[,weighted.median(FGrams/EqSizeCalory,Weight*Size)])
}

library(writexl)

#write_xlsx(BigsdTable,"E:/FPLinebasket.xlsx")

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")
