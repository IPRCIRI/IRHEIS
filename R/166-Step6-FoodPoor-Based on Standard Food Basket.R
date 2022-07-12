# 166-Step6-FoodPoor.R: Calculate Food Poor based on standard food basket (25 percent deviate from food basket)
#
# Licence: GPL-3

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


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  
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



endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")

