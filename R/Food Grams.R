#163- Step 3.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Food Grams =====================================\n")

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
  
  FoodGrams<-TotalFoodNonDurable[,.(HHID,BreadGrams,
                                     #BarbariPrice,
                                     GrainGrams,
                                    Rice_Khareji1Gram,
                                    Rice_Khareji2Gram,
                                     #Rice_DomsiahPrice,
                                     MacaroniGram,
                                     AdasGram,Loobia_ChitiGram,NokhodGram,
                                     #Loobia_ChitiPrice,
                                     SibzaminiGram,
                                     VegetableShrubsGrams,
                                     #KhiarPrice,
                                     TreeFruitsGrams,
                                     #CherryPrice,
                                     LivestockGrams,
                                    CowMeatGram,
                                    SheepMeatGram,
                                     #SheepMeatPrice,
                                     PoultryMeat_MGram,
                                     #Fish_North_FreshPrice,
                                     Egg_MashinGram,
                                     MilkGrams,MilkproductsGrams,
                                     #Cheese_PasturizedPrice,
                                     Oil_NabatiGram,
                                     #Oil_AnimalPrice,
                                     GhandGram)]
  
  #FoodGrams[FoodGrams == 0] <- NA
  
  save(FoodGrams,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodGrams.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)