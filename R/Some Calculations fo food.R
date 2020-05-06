rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
library(dplyr)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
year<-97

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoors.rda"))

MD<-merge(MD,Total[,.(HHID,`011164`,MacaroniGram,`011231`,`011232`,PoultryMeat_MGram,
                       PoultryMeat_NMGram,`011411`,MilkGrams,`011428`,`011429`,
                       Cheese_PasturizedGram,Cheese_NonPasturizedGram,
                       `011921`,Rob_GojeGram,`011441`,`011442`,Egg_MashinGram,
                       Egg_NonMashinGram,`011731`,SibzaminiGram,
                       `011732`,PiazGram)],by="HHID")

m<-MD[,.(weighted.mean(`011164`/Total_Exp_Month_nondurable,Weight),
         weighted.mean((`011231`+`011232`)/Total_Exp_Month_nondurable,Weight),
         weighted.mean(`011411`/Total_Exp_Month_nondurable,Weight),
         weighted.mean((`011428`+`011429`)/Total_Exp_Month_nondurable,Weight),
         weighted.mean(`011921`/Total_Exp_Month_nondurable,Weight),
         weighted.mean((`011441`+`011442`)/Total_Exp_Month_nondurable,Weight),
         weighted.mean(`011731`/Total_Exp_Month_nondurable,Weight),
         weighted.mean(`011732`/Total_Exp_Month_nondurable,Weight)),by=Decile][order(Decile)]

z<-MD[,.(weighted.mean(`011164`/FoodExpenditure,Weight),
         weighted.mean((`011231`+`011232`)/FoodExpenditure,Weight),
         weighted.mean(`011411`/FoodExpenditure,Weight),
         weighted.mean((`011428`+`011429`)/FoodExpenditure,Weight),
         weighted.mean(`011921`/FoodExpenditure,Weight),
         weighted.mean((`011441`+`011442`)/FoodExpenditure,Weight),
         weighted.mean(`011731`/FoodExpenditure,Weight),
         weighted.mean(`011732`/FoodExpenditure,Weight)),by=Decile][order(Decile)]

y<-MD[,.(weighted.mean(`011164`,Weight),
         weighted.mean(`011231`+`011232`,Weight),
         weighted.mean(`011411`,Weight),
         weighted.mean(`011428`+`011429`,Weight),
         weighted.mean(`011921`,Weight),
         weighted.mean(`011441`+`011442`,Weight),
         weighted.mean(`011731`,Weight),
         weighted.mean(`011732`,Weight)),by=Decile][order(Decile)]


x<-MD[,.(weighted.mean(MacaroniGram,Weight),
      weighted.mean(PoultryMeat_MGram+PoultryMeat_NMGram,Weight),
      weighted.mean(MilkGrams,Weight),
      weighted.mean(Cheese_PasturizedGram+Cheese_NonPasturizedGram,Weight),
      weighted.mean(Rob_GojeGram,Weight),
      weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight),
      weighted.mean(SibzaminiGram,Weight),
      weighted.mean(PiazGram,Weight)),by=Decile][order(Decile)]

MD[,weighted.mean(Size,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(FoodKCaloriesHH_Per,Weight),by=Decile][order(Decile)]


