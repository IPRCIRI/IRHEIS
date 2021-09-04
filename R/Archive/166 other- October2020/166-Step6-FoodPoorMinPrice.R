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
library(tidyr)
year<-Settings$baseBundleyear

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))

MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==3) |
                             (Region=="Rural" & Decile==2),1,0)]

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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  
  MD<-merge(MD,FoodPrices,all.x=TRUE,by="HHID")
  
  MD[,NewPoor:=InitialPoor]
  MD[,OldPoor:=1]
  
  
  #i <- 0
  #while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
  #    cat(nrow(MD[NewPoor==1]))
  # i <- i + 1
  MD[,ThisIterationPoor:=NewPoor]
  #  MD[,FPLine:=NULL]
  MD[,Selected_Group:=ifelse((Decile==1) | (Decile==2) | (Decile==3) | (Decile==4),1,0)]
  MDP <- MD [Selected_Group==1,
             .(Bread=min(weighted.mean(LavashPrice,Weight,na.rm = TRUE),weighted.mean(BarbariPrice,Weight,na.rm = TRUE),weighted.mean(TaftoonPrice,Weight,na.rm = TRUE),weighted.mean(Bread_FantasyPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Rice=min(weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),weighted.mean(Rice_AshPrice,Weight,na.rm = TRUE),weighted.mean(Rice_Khareji2Price,Weight,na.rm = TRUE),weighted.mean(Rice_DomsiahPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Spaghetti=min(weighted.mean(MacaroniPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Beans=min(weighted.mean(AdasPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_ChitiPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_GhermezPrice,Weight,na.rm = TRUE),weighted.mean(NokhodPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Vegetables=min(weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),weighted.mean(Sabzi_AshPrice,Weight,na.rm = TRUE),weighted.mean(KahooPrice,Weight,na.rm = TRUE),weighted.mean(KhiarPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Potato=min(weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Fruit=min(weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),weighted.mean(CherryPrice,Weight,na.rm = TRUE),weighted.mean(OrangePrice,Weight,na.rm = TRUE),weighted.mean(ApplePrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Meat=min(weighted.mean(CowMeatPrice,Weight,na.rm = TRUE),weighted.mean(SheepMeatPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Chicken=min(weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Fish=min(weighted.mean(Fish_North_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_South_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_ConservedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Egg=min(weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Milk=min(weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Yogurt=min(weighted.mean(Yogurt_PasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Cheese=min(weighted.mean(Cheese_PasturizedPrice,Weight,na.rm = TRUE),weighted.mean(Cheese_NonPasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Oil=min(weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),weighted.mean(Oil_AnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_NonAnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_Animal_PasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Nuts=min(weighted.mean(KeshmeshPrice,Weight,na.rm = TRUE),weighted.mean(TokhmePrice,Weight,na.rm = TRUE),weighted.mean(PistachioPrice,Weight,na.rm = TRUE),weighted.mean(NokhodchiPrice,Weight,na.rm = TRUE),na.rm = TRUE),
               Sugar=min(weighted.mean(GhandPrice,Weight,na.rm = TRUE),weighted.mean(ShekarPrice,Weight,na.rm = TRUE),na.rm = TRUE)),
             by=.(cluster3,Region)]
  
  #Bfd2[is.na(Bfd2)]<-0
  #Bfd2[Price<0.1,Price:=NA]
  # Basket <- Bfd2[Selected_Group==1,
  #                .(FGramspc=weighted.mean(FGrams/EqSizeCalory,Weight*Size)),
  #                by=.(FoodType,cluster3)]
  MDP2 <- gather(MDP, FoodType, Price, Bread:Sugar, factor_key=TRUE)
  
  BasketCost <- merge(BaseYearBasket,MDP2,by=c("FoodType","Region"),all.x = T)
  BasketCost[,Cost:=(StandardFGramspc/1000)*Price]
  FPLineBasket <- BasketCost[,.(FPLine=sum(Cost)),by=cluster3]
  
  MD <- merge(MD,FPLineBasket,all.x=TRUE,by="cluster3")
  
  MD[,FoodPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]
  
  cat(unlist(MD[cluster3==1,.(FPLine,weighted.mean(FoodPoor))][1]))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")