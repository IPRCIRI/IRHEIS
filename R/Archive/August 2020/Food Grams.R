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
  
  
  #FoodGrams[FoodGrams == 0] <- NA
  FoodGrams<-TotalFoodNonDurable[,.(HHID,LavashGram,TaftoonGram,BarbariGram,
                            Bread_FantasyGram,Rice_TaromGram,
                            Rice_AshGram,Rice_Khareji2Gram,
                            Rice_DomsiahGram,AdasGram,Loobia_ChitiGram,
                            Loobia_GhermezGram,NokhodGram,Sabzi_KhordanGram,
                            Sabzi_AshGram,KahooGram,KhiarGram,
                            Banana_CoconutGram,OrangeGram,CherryGram,
                            AppleGram,CowMeatGram,SheepMeatGram,
                            Fish_North_FreshGram,Fish_South_FreshGram,
                            Fish_ConservedGram,Cheese_PasturizedGram,
                            Cheese_NonPasturizedGram,Oil_AnimalGram,
                            Oil_NabatiGram,Butter_Animal_PasturizedGram,
                            Butter_NonAnimalGram,KeshmeshGram,
                            NokhodchiGram,PistachioGram,TokhmeGram,
                            GhandGram,ShekarGram)]
  save(FoodGrams,file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodGrams.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)