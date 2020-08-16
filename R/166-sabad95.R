#703-Bundle 2100
# Builds the Food Groups data.table for households
#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(spatstat)
library(writexl)
library(tidyr)

year<-Settings$baseBundleyear

load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))

goosht<-BigFData[FoodType=="Goosht"]
goosht<-goosht[,Goosht_Grams:=FGrams*30 ]
morgh<-BigFData[FoodType=="Morgh"]
morgh<-morgh[,Morgh_Grams:=FGrams*30 ]
mive<-BigFData[FoodType=="Mive"]
mive<-mive[,Mive_Grams:=FGrams*30 ]
nan<-BigFData[FoodType=="Nan"]
nan<-nan[,Nan_Grams:=FGrams*30 ]
sibzamini<-BigFData[FoodType=="Sibzamini"]
sibzamini<-sibzamini[,Sibzamini_Grams:=FGrams*30 ]
makarooni<-BigFData[FoodType=="Makarooni"]
makarooni<-makarooni[,Makarooni_Grams:=FGrams*30 ]
berenj<-BigFData[FoodType=="Berenj"]
berenj<-berenj[,Berenj_Grams:=FGrams*30 ]
hoboobat<-BigFData[FoodType=="Hoboobat"]
hoboobat<-hoboobat[,Hoboobat_Grams:=FGrams*30 ]
sabzi<-BigFData[FoodType=="Sabzi"]
sabzi<-sabzi[,Sabzi_Grams:=FGrams*30 ]
roghan<-BigFData[FoodType=="Roghan"]
roghan<-roghan[,Roghan_Grams:=FGrams*30 ]
ghand<-BigFData[FoodType=="Ghand"]
ghand<-ghand[,Ghand_Grams:=FGrams*30 ]
shir<-BigFData[FoodType=="Shir"]
shir<-shir[,Shir_Grams:=FGrams*30]
panir<-BigFData[FoodType=="Panir"]
panir<-panir[,Panir_Grams:=FGrams*30]
mast<-BigFData[FoodType=="Mast"]
mast<-mast[,Mast_Grams:=FGrams*30]
tokhmemorgh<-BigFData[FoodType=="Tokhmemorgh"]
tokhmemorgh<-tokhmemorgh[,Tokhmemorgh_Grams:=FGrams*30 ]
mahi<-BigFData[FoodType=="Mahi"]
mahi<-mahi[,Mahi_Grams:=FGrams*30 ]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))

MD<-merge(MD,goosht[,.(HHID,Goosht_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,morgh[,.(HHID,Morgh_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mive[,.(HHID,Mive_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,nan[,.(HHID,Nan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sibzamini[,.(HHID,Sibzamini_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,makarooni[,.(HHID,Makarooni_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,berenj[,.(HHID,Berenj_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,hoboobat[,.(HHID,Hoboobat_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sabzi[,.(HHID,Sabzi_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,roghan[,.(HHID,Roghan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,ghand[,.(HHID,Ghand_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,shir[,.(HHID,Shir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,panir[,.(HHID,Panir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mast[,.(HHID,Mast_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,tokhmemorgh[,.(HHID,Tokhmemorgh_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mahi[,.(HHID,Mahi_Grams)],by=c("HHID"),all.x = T)

MD<-MD[,Labaniat_Grams:=Shir_Grams+Mast_Grams]
MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==3) |
                             (Region=="Rural" & Decile==2),1,0)]

gram <-MD [Selected_Group==1,
           .(Nan_Grams= weighted.mean(Nan_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Berenj_Grams= weighted.mean(Berenj_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Makarooni_Grams=weighted.mean(Makarooni_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Hoboobat_Grams=weighted.mean(Hoboobat_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Sibzamini_Grams=weighted.mean(Sibzamini_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Sabzi_Grams=weighted.mean(Sabzi_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Mive_Grams=weighted.mean(Mive_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Goosht_Grams=weighted.mean(Goosht_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Morgh_Grams=weighted.mean(Morgh_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Tokhmemorgh_Grams=weighted.mean(Tokhmemorgh_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Labaniat_Grams=weighted.mean(Labaniat_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Roghan_Grams=weighted.mean(Roghan_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Ghand_Grams=weighted.mean(Ghand_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Mahi_Grams=weighted.mean(Mahi_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE),
             Panir_Grams=weighted.mean(Panir_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE)),by=c("Region")]

year<-90

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  
  MD<-merge(MD,FoodPrices,all.x=TRUE,by="HHID")
  
  MD[,NewPoor:=InitialPoor]
  MD[,OldPoor:=1]
  
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
    #    cat(nrow(MD[NewPoor==1]))
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]
    MD[,Selected_Group:=ifelse((Decile==1) | (Decile==2) | (Decile==3) | (Decile==4),1,0)]
    MDP <- MD [Selected_Group==1,
               .(Nan_Price=min(weighted.mean(LavashPrice,Weight,na.rm = TRUE),weighted.mean(BarbariPrice,Weight,na.rm = TRUE),weighted.mean(TaftoonPrice,Weight,na.rm = TRUE),weighted.mean(Bread_FantasyPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Berenj_Price=min(weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),weighted.mean(Rice_AshPrice,Weight,na.rm = TRUE),weighted.mean(Rice_Khareji2Price,Weight,na.rm = TRUE),weighted.mean(Rice_DomsiahPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Makarooni_Price=min(weighted.mean(MacaroniPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Hoboobat_Price=min(weighted.mean(AdasPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_ChitiPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_GhermezPrice,Weight,na.rm = TRUE),weighted.mean(NokhodPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Sabzi_Price=min(weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),weighted.mean(Sabzi_AshPrice,Weight,na.rm = TRUE),weighted.mean(KahooPrice,Weight,na.rm = TRUE),weighted.mean(KhiarPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Sibzamini_Price=min(weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Mive_Price=min(weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),weighted.mean(CherryPrice,Weight,na.rm = TRUE),weighted.mean(OrangePrice,Weight,na.rm = TRUE),weighted.mean(ApplePrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Goosht_Price=min(weighted.mean(CowMeatPrice,Weight,na.rm = TRUE),weighted.mean(SheepMeatPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Morgh_Price=min(weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Mahi_Price=min(weighted.mean(Fish_North_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_South_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_ConservedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Tokhmemorgh_Price=min(weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Labaniat_Price=min(weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),weighted.mean(Yogurt_PasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Panir_Price=min(weighted.mean(Cheese_PasturizedPrice,Weight,na.rm = TRUE),weighted.mean(Cheese_NonPasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Roghan_Price=min(weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),weighted.mean(Oil_AnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_NonAnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_Animal_PasturizedPrice,Weight,na.rm = TRUE),na.rm = TRUE),
                 Ghand_Price=min(weighted.mean(GhandPrice,Weight,na.rm = TRUE),weighted.mean(ShekarPrice,Weight,na.rm = TRUE),na.rm = TRUE)),
               by=.(cluster3,Region)]
    
    MDP[mapply(is.infinite, MDP)] <- NA
    MDP<- MDP %>% mutate(Nan_Price = ifelse(is.na(Nan_Price), mean(Nan_Price, na.rm = T), Nan_Price))
    MDP<- MDP %>% mutate(Berenj_Price = ifelse(is.na(Berenj_Price), mean(Berenj_Price, na.rm = T), Berenj_Price))
    MDP<- MDP %>% mutate(Makarooni_Price = ifelse(is.na(Makarooni_Price), mean(Makarooni_Price, na.rm = T), Makarooni_Price))
    MDP<- MDP %>% mutate(Hoboobat_Price = ifelse(is.na(Hoboobat_Price), mean(Hoboobat_Price, na.rm = T), Hoboobat_Price))
    MDP<- MDP %>% mutate(Sabzi_Price = ifelse(is.na(Sabzi_Price), mean(Sabzi_Price, na.rm = T), Sabzi_Price))
    MDP<- MDP %>% mutate(Sibzamini_Price = ifelse(is.na(Sibzamini_Price), mean(Sibzamini_Price, na.rm = T), Sibzamini_Price))
    MDP<- MDP %>% mutate(Mive_Price = ifelse(is.na(Mive_Price), mean(Mive_Price, na.rm = T), Mive_Price))
    MDP<- MDP %>% mutate(Goosht_Price = ifelse(is.na(Goosht_Price), mean(Goosht_Price, na.rm = T), Goosht_Price))
    MDP<- MDP %>% mutate(Morgh_Price = ifelse(is.na(Morgh_Price), mean(Morgh_Price, na.rm = T), Morgh_Price))
    MDP<- MDP %>% mutate(Mahi_Price = ifelse(is.na(Mahi_Price), mean(Mahi_Price, na.rm = T), Mahi_Price))
    MDP<- MDP %>% mutate(Tokhmemorgh_Price = ifelse(is.na(Tokhmemorgh_Price), mean(Tokhmemorgh_Price, na.rm = T), Tokhmemorgh_Price))
    MDP<- MDP %>% mutate(Labaniat_Price = ifelse(is.na(Labaniat_Price), mean(Labaniat_Price, na.rm = T), Labaniat_Price))
    MDP<- MDP %>% mutate(Panir_Price = ifelse(is.na(Panir_Price), mean(Panir_Price, na.rm = T), Panir_Price))
    MDP<- MDP %>% mutate(Roghan_Price = ifelse(is.na(Roghan_Price), mean(Roghan_Price, na.rm = T), Roghan_Price))
    MDP<- MDP %>% mutate(Ghand_Price = ifelse(is.na(Ghand_Price), mean(Ghand_Price, na.rm = T), Ghand_Price))
    MDP<-merge(MDP,gram,by=c("Region"),all.x=TRUE)
    
    MDP<- MDP %>% mutate(Nan_Price = ifelse(is.infinite(Nan_Price), min(Nan_Price, na.rm = T), Nan_Price))
    MDP<- MDP %>% mutate(Berenj_Price = ifelse(is.infinite(Berenj_Price), min(Berenj_Price, na.rm = T), Berenj_Price))
    MDP<- MDP %>% mutate(Makarooni_Price = ifelse(is.infinite(Makarooni_Price), min(Makarooni_Price, na.rm = T), Makarooni_Price))
    MDP<- MDP %>% mutate(Hoboobat_Price = ifelse(is.infinite(Hoboobat_Price), min(Hoboobat_Price, na.rm = T), Hoboobat_Price))
    MDP<- MDP %>% mutate(Sabzi_Price = ifelse(is.infinite(Sabzi_Price), min(Sabzi_Price, na.rm = T), Sabzi_Price))
    MDP<- MDP %>% mutate(Sibzamini_Price = ifelse(is.infinite(Sibzamini_Price), min(Sibzamini_Price, na.rm = T), Sibzamini_Price))
    MDP<- MDP %>% mutate(Mive_Price = ifelse(is.infinite(Mive_Price), min(Mive_Price, na.rm = T), Mive_Price))
    MDP<- MDP %>% mutate(Goosht_Price = ifelse(is.infinite(Goosht_Price), min(Goosht_Price, na.rm = T), Goosht_Price))
    MDP<- MDP %>% mutate(Morgh_Price = ifelse(is.infinite(Morgh_Price), min(Morgh_Price, na.rm = T), Morgh_Price))
    MDP<- MDP %>% mutate(Mahi_Price = ifelse(is.infinite(Mahi_Price), min(Mahi_Price, na.rm = T), Mahi_Price))
    MDP<- MDP %>% mutate(Tokhmemorgh_Price = ifelse(is.infinite(Tokhmemorgh_Price), min(Tokhmemorgh_Price, na.rm = T), Tokhmemorgh_Price))
    MDP<- MDP %>% mutate(Labaniat_Price = ifelse(is.infinite(Labaniat_Price), min(v, na.rm = T), Labaniat_Price))
    MDP<- MDP %>% mutate(Panir_Price = ifelse(is.infinite(Panir_Price), min(v, na.rm = T), Panir_Price))
    MDP<- MDP %>% mutate(Roghan_Price = ifelse(is.infinite(Roghan_Price), min(Roghan_Price, na.rm = T), Roghan_Price))
    MDP<- MDP %>% mutate(Ghand_Price = ifelse(is.infinite(Ghand_Price), min(Ghand_Price, na.rm = T), Ghand_Price))
    MDP<-as.data.table(MDP)
    MDP <- MDP[,FPLine:=0.001*((Berenj_Price*Berenj_Grams)+
                                 (Nan_Price*Nan_Grams)+
                                 (Makarooni_Price*Makarooni_Grams)+
                                 (Hoboobat_Price*Hoboobat_Grams)+
                                 (Sabzi_Price*Sabzi_Grams)+
                                 (Sibzamini_Price*Sibzamini_Grams)+
                                 (Mive_Price*Mive_Grams)+
                                 (Goosht_Price*Goosht_Grams)+
                                 (Morgh_Price*Morgh_Grams)+
                                 (Mahi_Price*Mahi_Grams)+
                                 (Tokhmemorgh_Price*Tokhmemorgh_Grams)+
                                 (Labaniat_Price*Labaniat_Grams)+
                                 (Panir_Price*Panir_Grams)+
                                 (Roghan_Price*Roghan_Grams)+
                                 (Ghand_Price*Ghand_Grams))]
    
    MD <- merge(MD,MDP[,.(Region,cluster3,FPLine)],all.x=TRUE,by=c("Region","cluster3"))
    #    print(MDP)
    #x<-MD[,.(NewArea,Region,FPLine,InitialPoor)]
    MD[,NewPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]
    #    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }
  
  MD[,FinalFoodPoor:=OldPoor]
  
  # MD <- MD[,.(HHID,HIndivNo,Region,NewArea,NewArea_Name,cluster3,ProvinceCode,Size,HAge,HSex,Month,ServiceExp,
  #    HLiterate,HEduLevel0,HActivityState,Area,Rooms,MetrPrice,Total_Exp_Month_nondurable,
  #   Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
  #   OriginalFoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor,
  #   Total_Exp_Month_Per,TFoodKCaloriesHH_Per,TOriginalFoodExpenditure,Total_Exp_Month,
  #  TFoodExpenditure2,Total_Exp_Month_nondurable2,Total_Exp_Month2,
  # Total_Exp_Month_Per2,
  #   EqSizeOECD,EqSizeCalory,Decile,Bundle_Value)]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  MD[,weighted.mean(FinalFoodPoor,Weight)]
  # MDFinalfood<-MD[,.(HHID,Region,NewArea,cluster3,Percentile,FinalFoodPoor)]
  # UrbanFinalfood<-MDFinalfood[Region=="Urban"]
  # RuralFinalfood<-MDFinalfood[Region=="Rural"]
  # save(UrbanFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanFinalfood.rda"))
  # save(RuralFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralFinalfood.rda"))
  # 
  MD[,weighted.mean(FinalFoodPoor,Weight),by=c("Region","ProvinceCode")][order(Region,ProvinceCode)]
  MD[,weighted.mean(FinalFoodPoor,Weight),by=cluster3][order(cluster3)]
  #  cat(MD[,weighted.mean(FPLine,Weight)])
  # cat(MD[cluster3==13,weighted.mean(Calory_Price,Weight)])
  #cat(MD[cluster3==1,weighted.mean(TOriginalFoodExpenditure_Per,Weight)])
  x<-MD[,weighted.mean(FPLine,Weight),by="cluster3"]
  
  
  #  cat(MD[,weighted.mean(TOriginalFoodExpenditure_Per,Weight)],"\n")
  #  cat(MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE)],"\n")
  #  cat(MD[,weighted.mean(Calory_Price,Weight,na.rm = TRUE)],"\n")
  # cat(MD[cluster3==1,weighted.mean(FPLine,Weight,na.rm = TRUE)],"\n")
  cat(MD[,weighted.mean(FPLine,Weight*Size,na.rm = TRUE)],"\n")
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")