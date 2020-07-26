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

library(data.table)
library(stringr)
library(readxl)
library(spatstat)
library(writexl)
library(tidyr)

year<-95

load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))

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
khoshkbar<-BigFData[FoodType=="Khoshkbar"]
khoshkbar<-khoshkbar[,Khoshkbar_Grams:=FGrams*30 ]

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
MD<-merge(MD,khoshkbar[,.(HHID,Khoshkbar_Grams)],by=c("HHID"),all.x = T)
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
             Khoshkbar_Grams=weighted.mean(Khoshkbar_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE),
             Mahi_Grams=weighted.mean(Mahi_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE),
             Panir_Grams=weighted.mean(Panir_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE))]
             



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodGrams.rda"))
  
  MD<-merge(MD,FoodPrices,all.x=TRUE,by="HHID")
  MD<-merge(MD,FoodGrams,all.x=TRUE,by="HHID")
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  MetaData <- read_excel(Settings$MetaDataFilePath, Settings$MDS_FoodGroups)
  MetaData<-as.data.table(MetaData) 
  MetaData<-MetaData[,SheetName:=NULL]
  FoodNames<-MetaData
  save(FoodNames,file ="FoodNames.rda" )
  load(file = "FoodNames.rda")
  
  # y<-MD[,weighted.mean(FoodKCaloriesHH_Per,Weight),by=c("Region","Decile")]
  
  
  MD[,NewPoor:=InitialPoor]
  MD[,OldPoor:=1]
  load(file=paste0(Settings$HEISProcessedPath,"Gram.rda"))
  MD<- merge(MD,gram, by=c("cluster3","Region"),all.x = TRUE)
  
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
    #    cat(nrow(MD[NewPoor==1]))
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]
    MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==3) |
                                 (Region=="Rural" & Decile==2),1,0)]
    MDP <- MD [Selected_Group==1,
                   .(Nan_Price=min(weighted.mean(LavashPrice,Weight,na.rm = TRUE),weighted.mean(BarbariPrice,Weight,na.rm = TRUE),weighted.mean(TaftoonPrice,Weight,na.rm = TRUE),weighted.mean(Bread_FantasyPrice,Weight,na.rm = TRUE)),
                      Berenj_Price=min(weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),weighted.mean(Rice_AshPrice,Weight,na.rm = TRUE),weighted.mean(Rice_Khareji2Price,Weight,na.rm = TRUE),weighted.mean(Rice_DomsiahPrice,Weight,na.rm = TRUE)),
                      Makarooni_Price=weighted.mean(MacaroniPrice,Weight,na.rm = TRUE),
                      Hoboobat_Price=min(weighted.mean(AdasPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_ChitiPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_GhermezPrice,Weight,na.rm = TRUE),weighted.mean(NokhodPrice,Weight,na.rm = TRUE)),
                      Sabzi_Price=min(weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),weighted.mean(Sabzi_AshPrice,Weight,na.rm = TRUE),weighted.mean(KahooPrice,Weight,na.rm = TRUE),weighted.mean(KhiarPrice,Weight,na.rm = TRUE)),
                      Sibzamini_Price=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),
                      Mive_Price=min(weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),weighted.mean(CherryPrice,Weight,na.rm = TRUE),weighted.mean(OrangePrice,Weight,na.rm = TRUE),weighted.mean(ApplePrice,Weight,na.rm = TRUE)),
                      Goosht_Price=min(weighted.mean(CowMeatPrice,Weight,na.rm = TRUE),weighted.mean(SheepMeatPrice,Weight,na.rm = TRUE)),
                      Morgh_Price=weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE),
                      Mahi_Price=min(weighted.mean(Fish_North_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_South_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_ConservedPrice,Weight,na.rm = TRUE)),
                      weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE),
                      ShirMast_Price=min(weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),weighted.mean(Yogurt_PasturizedPrice,Weight,na.rm = TRUE)),
                      Roghan_Price=min(weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),weighted.mean(Oil_AnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_NonAnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_Animal_PasturizedPrice,Weight,na.rm = TRUE)),
                      Ghand_Price=min(weighted.mean(GhandPrice,Weight,na.rm = TRUE),weighted.mean(ShekarPrice,Weight,na.rm = TRUE))),
               by=.(cluster3,Region)]
    for (col in c("Berenj_Price")) 
      MDP[is.na(get(col)),(col):=min(col)]
    
      MDP <- MD [Selected_Group==1,
               .(FPLine=0.001*
                    (min(weighted.mean(LavashPrice,Weight,na.rm = TRUE),weighted.mean(BarbariPrice,Weight,na.rm = TRUE),weighted.mean(TaftoonPrice,Weight,na.rm = TRUE),weighted.mean(Bread_FantasyPrice,Weight,na.rm = TRUE))*gram$Nan_Grams+
                    min(weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),weighted.mean(Rice_AshPrice,Weight,na.rm = TRUE),weighted.mean(Rice_Khareji2Price,Weight,na.rm = TRUE),weighted.mean(Rice_DomsiahPrice,Weight,na.rm = TRUE))*gram$Berenj_Grams+
                      weighted.mean(MacaroniPrice,Weight,na.rm = TRUE)*gram$Makarooni_Grams+
                    min(weighted.mean(AdasPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_ChitiPrice,Weight,na.rm = TRUE),weighted.mean(Loobia_GhermezPrice,Weight,na.rm = TRUE),weighted.mean(NokhodPrice,Weight,na.rm = TRUE))*gram$Hoboobat_Grams+
                    min(weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),weighted.mean(Sabzi_AshPrice,Weight,na.rm = TRUE),weighted.mean(KahooPrice,Weight,na.rm = TRUE),weighted.mean(KhiarPrice,Weight,na.rm = TRUE))*gram$Sabzi_Grams+
                      weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*gram$Sibzamini_Grams+
                    min(weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),weighted.mean(CherryPrice,Weight,na.rm = TRUE),weighted.mean(OrangePrice,Weight,na.rm = TRUE),weighted.mean(ApplePrice,Weight,na.rm = TRUE))*gram$Mive_Grams+
                    min(weighted.mean(CowMeatPrice,Weight,na.rm = TRUE),weighted.mean(SheepMeatPrice,Weight,na.rm = TRUE))*(gram$Goosht_Grams)+
                      weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE)*gram$Morgh_Grams+
                    min(weighted.mean(Fish_North_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_South_FreshPrice,Weight,na.rm = TRUE),weighted.mean(Fish_ConservedPrice,Weight,na.rm = TRUE))*gram$Mahi_Grams+
                      weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE)*gram$Tokhmemorgh_Grams+
                    min(weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),weighted.mean(Yogurt_PasturizedPrice,Weight,na.rm = TRUE))*(gram$Labaniat_Grams)+
                    min(weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),weighted.mean(Oil_AnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_NonAnimalPrice,Weight,na.rm = TRUE),weighted.mean(Butter_Animal_PasturizedPrice,Weight,na.rm = TRUE))*gram$Roghan_Grams+
                    min(weighted.mean(GhandPrice,Weight,na.rm = TRUE),weighted.mean(ShekarPrice,Weight,na.rm = TRUE))*gram$Ghand_Grams+
                    min(weighted.mean(KeshmeshPrice,Weight,na.rm = TRUE),weighted.mean(NokhodchiPrice,Weight,na.rm = TRUE),weighted.mean(TokhmePrice,Weight,na.rm = TRUE),weighted.mean(GhandPrice,PistachioPrice,na.rm = TRUE))*gram$Khoshkbar_Grams)),
               by=.(cluster3,Region)]


    MDP<-MDP[,lapply(.SD,sum),by=c("cluster3","Region")]
    price<- MD[Selected_Group==1,
               .(LavashPrice=weighted.mean(LavashPrice,Weight,na.rm = TRUE),
                 Rice_TaromPrice=weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),
                 MacaroniPrice=weighted.mean(MacaroniPrice,Weight,na.rm = TRUE),
                 HobubatPrice=weighted.mean(AdasPrice,Weight,na.rm = TRUE),
                 SibzaminiPrice=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),
                 Sabzi_KhordanPrice=weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),
                 Banana_CoconutPrice=weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),
                 CowMeatPrice=weighted.mean(CowMeatPrice,Weight,na.rm = TRUE),
                 PoultryMeat_MPrice=weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE),
                 Egg_MashinPrice=weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE),
                 Milk_PasteurizedPrice=weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),
                 Oil_NabatiPrice=weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),
                 GhandPrice=weighted.mean(GhandPrice,Weight,na.rm = TRUE),
                 number=.N)]
               #by=.(cluster3,Region)]
    
     
    MDP[is.na(MDP)] <- 0
    min<-MDP[FPLine>0,min(FPLine)]
    MDP[,FPLine:=ifelse(FPLine==0,min,FPLine)]
    
    MD <- merge(MD,MDP,all.x=TRUE,by=c("Region","cluster3"))
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
  cat(MD[,weighted.mean(FPLine,Weight,na.rm = TRUE)],"\n")
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")

