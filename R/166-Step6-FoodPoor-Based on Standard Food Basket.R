# 166-Step6-FoodPoor.R: Calculate Food Poor based on standard food basket (25 percent deviate from food basket)
#
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(spatstat)
library(data.table)
library(isotone)
BigsdTable <- data.table()

BaseYearBasket <- data.table(read_excel(paste0(Settings$HEISResultsPath,"/FoodBasketStatsList.xlsx"),sheet = "25"))


#Reweighting
Pop_U84 <- 47096
Pop_U85 <- 48260
Pop_U86 <- 49287.8
Pop_U87 <- 50339.7
Pop_U88 <- 51416.4
Pop_U89 <- 52518.5
Pop_U90 <- 53647
Pop_U91 <- 55419
Pop_U92 <- 56329
Pop_U93 <- 57252
Pop_U94 <- 58192
Pop_U95 <- 59147
Pop_U96 <- 60281
Pop_U97 <- 61329
Pop_U98 <- 62367
Pop_U99 <- 63390
Pop_U100 <- 63876
Pop_U101 <- 64655
Pop_U102 <- 65427

Pop_R84 <- 22294
Pop_R85 <- 22235.8
Pop_R86 <- 22078.4
Pop_R87 <- 21926.5
Pop_R88 <- 21780
Pop_R89 <- 21638.8
Pop_R90 <- 21503
Pop_R91<- 20656
Pop_R92<- 20687
Pop_R93<- 20718
Pop_R94<- 20748
Pop_R95<- 20779
Pop_R96<- 20789
Pop_R97<- 20754
Pop_R98<- 20708
Pop_R99<- 20648
Pop_R100 <- 20179
Pop_R101 <- 20045
Pop_R102 <- 19902

BasketCostSereis <- data.table()
#year <- 100
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  
  
  if (year==90){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U90/Weight_U,Weight*Pop_R90/Weight_R)]  
  }
  
  if (year==91){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U91/Weight_U,Weight*Pop_R91/Weight_R)] 
  }
  
  if (year==92){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U92/Weight_U,Weight*Pop_R92/Weight_R)]  
  }
  
  if (year==93){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U93/Weight_U,Weight*Pop_R93/Weight_R)]  
  }
  
  if (year==94){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U94/Weight_U,Weight*Pop_R94/Weight_R)]  
  }
  
  if (year==95){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U95/Weight_U,Weight*Pop_R95/Weight_R)]  
  }
  
  if (year==96){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U96/Weight_U,Weight*Pop_R96/Weight_R)] 
  }
  
  if (year==97){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U97/Weight_U,Weight*Pop_R97/Weight_R)]  
  }
  
  if (year==98){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U98/Weight_U,Weight*Pop_R98/Weight_R)]   
  }
  
  if (year==99){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U99/Weight_U,Weight*Pop_R99/Weight_R)]   
  }
  
  if (year==100){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U100/Weight_U,Weight*Pop_R100/Weight_R)]   
  }

  if (year==101){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U101/Weight_U,Weight*Pop_R101/Weight_R)]   
  }
  
  if (year==102){
    Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
    Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
    MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U102/Weight_U,Weight*Pop_R102/Weight_R)]   
  }
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFDataTotalNutrition.rda"))
  
  MD[,Selected_Group:=ifelse(Dcil_IP_Cons_PAdj %in% 2:5,1,0)]
  
  Bfd2 <- merge(BigFData,MD[,.(HHID,Region,Weight,Size,cluster3,
                           EqSizeCalory,Selected_Group)],by="HHID")
  Bfd2[is.na(Bfd2)]<-0
  Bfd2[Price<0.1,Price:=NA]
  Bfd2 <- Bfd2[,
               .(Price=weighted.mean(Price,Weight*Size*FGrams,na.rm = TRUE),
                 FGrams=sum(FGrams),
                 cluster3=first(cluster3),
                 Region=first(Region), Weight=first(Weight),
                 Size=first(Size),Selected_Group=first(Selected_Group)),
               by=.(HHID,FoodType)]
  Bfd3 <- Bfd2[Selected_Group==1 & !is.na(Price),
               .(MedPrice=weighted.median(Price,Weight*Size*FGrams),
                 MeanPrice=weighted.mean(Price,Weight*Size*FGrams,na.rm = TRUE)),
               by=.(FoodType,Region,cluster3)]
  BasketPrice <- Bfd3[!is.na(MeanPrice),
                      .(Price=min(MeanPrice)),
                      by=.(FoodType,Region,cluster3)]
  
 
  BasketCost <- merge(BaseYearBasket,BasketPrice,by=c("FoodType"))
  BasketCost[,Cost:=(StandardFGramspc/1000)*Price]
  # BasketCost[,Year:=year]
  # BasketCostSereis <- rbind(BasketCostSereis,BasketCost)
  #   

  
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
  
}

