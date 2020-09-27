#163- Step 3.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Food Prices =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodNonDurable.rda"))

 FoodPrices<-TotalFoodNonDurable[,.(HHID,
                                    LavashPrice,BarbariPrice,TaftoonPrice, Bread_FantasyPrice,
                                    Rice_TaromPrice,Rice_Khareji2Price,Rice_DomsiahPrice, Rice_AshPrice,
                                    MacaroniPrice,
                                    AdasPrice, Loobia_ChitiPrice,  Loobia_GhermezPrice, NokhodPrice,
                                    SibzaminiPrice,
                                    Sabzi_KhordanPrice, Sabzi_AshPrice,  KahooPrice,KhiarPrice,
                                    Banana_CoconutPrice,CherryPrice,ApplePrice,OrangePrice,
                                    CowMeatPrice,SheepMeatPrice,
                                    PoultryMeat_MPrice,
                                    Fish_North_FreshPrice,Fish_South_FreshPrice,Fish_ConservedPrice,
                                    Egg_MashinPrice,
                                    Milk_PasteurizedPrice, Yogurt_PasturizedPrice,CreamPrice,
                                    Cheese_PasturizedPrice,Cheese_NonPasturizedPrice,
                                    Oil_NabatiPrice,Oil_AnimalPrice,Butter_Animal_PasturizedPrice,Butter_NonAnimalPrice,
                                    GhandPrice,ShekarPrice,
                                    KeshmeshPrice,TokhmePrice,NokhodchiPrice,PistachioPrice
                                    )]

 FoodPrices[FoodPrices == 0] <- NA
 
save(FoodPrices,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))

}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)