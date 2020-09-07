rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
library(dplyr)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
#year<-97

TS <- data.table(Year=NA_integer_,Berenj=NA_real_,
                  Gav=NA_real_,Goosfand=NA_real_,
                 Shir=NA_real_,Panir=NA_real_,
                 Tokhmemorgh=NA_real_,
                Morgh=NA_real_,MacaroniGram=NA_real_,
                 Rob_GojeGram=NA_real_,PiazGram=NA_real_,
                SibzaminiGram=NA_real_,
                 Decile=NA_real_)[0]


for (year in (Settings$startyear:Settings$endyear)){
   cat(paste0("\n------------------------------\nYear:", year, "\n"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoors.rda"))

MD<-merge(MD,Total[,.(HHID,`011164`,MacaroniGram,`011231`,`011232`,PoultryMeat_MGram,
                       PoultryMeat_NMGram,`011411`,MilkGrams,`011428`,`011429`,
                       Cheese_PasturizedGram,Cheese_NonPasturizedGram,
                       `011921`,Rob_GojeGram,`011441`,`011442`,Egg_MashinGram,
                       Egg_NonMashinGram,`011731`,SibzaminiGram,
                       `011732`,PiazGram,`011211`,`011212`,
                      CowMeatGram,SheepMeatGram,
                      Rice_TaromGram,Rice_DomsiahGram)],by="HHID")

A<-MD[,.(Morgh=weighted.mean(PoultryMeat_MGram,Weight),
         Shir= weighted.mean(MilkGrams,Weight),
         Panir=weighted.mean(Cheese_PasturizedGram,Weight),
         Tokhmemorgh=weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight),
         Berenj=weighted.mean(Rice_TaromGram+Rice_DomsiahGram,Weight),
         Gav=weighted.mean(CowMeatGram,Weight),
         Goosfand=weighted.mean(SheepMeatGram,Weight),
         Rob_GojeGram=weighted.mean(Rob_GojeGram,Weight),
         SibzaminiGram=weighted.mean(SibzaminiGram,Weight),
         PiazGram=weighted.mean(PiazGram,Weight),
         MacaroniGram=weighted.mean(MacaroniGram,Weight)),by=Decile][order(Decile)]
A[,Year:=year]
TS <- rbind(TS,A)


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


x<-MD[,.(MacaroniGram=weighted.mean(MacaroniGram,Weight),
         PoultryMeat_Gram=weighted.mean(PoultryMeat_MGram+PoultryMeat_NMGram,Weight),
         MilkGrams=weighted.mean(MilkGrams,Weight),
         CheeseGram=weighted.mean(Cheese_PasturizedGram+Cheese_NonPasturizedGram,Weight),
         Rob_GojeGram=weighted.mean(Rob_GojeGram,Weight),
         Egg_Gram=weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight),
         SibzaminiGram=weighted.mean(SibzaminiGram,Weight),
         PiazGram=weighted.mean(PiazGram,Weight),
         CowMeatGram=weighted.mean(CowMeatGram,Weight),
         SheepMeatGram=weighted.mean(SheepMeatGram,Weight)),by=Decile][order(Decile)]

MD[,weighted.mean(Size,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(FoodKCaloriesHH_Per,Weight),by=Decile][order(Decile)]




}

#TS<-TS[as.numeric(Decile)<7]

#ggplot(TS, aes( y=Morgh, x=Year,fill=Decile)) + 
 #  geom_bar(position="dodge", stat="identity") + theme_bw() +
  # theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
