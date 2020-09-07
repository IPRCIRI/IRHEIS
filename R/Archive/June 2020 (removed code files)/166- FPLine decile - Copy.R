#166-Step 6- FoodBasicNeeds.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodGrams.rda"))
  
  MD<-merge(MD,FoodPrices,all.x=TRUE,by="HHID")
  MD<-merge(MD,FoodGrams,all.x=TRUE,by="HHID")
  
  # y<-MD[,weighted.mean(FoodKCaloriesHH_Per,Weight),by=c("Region","Decile")]
  
  
  MD[,NewPoor:=InitialPoor]
  MD[,OldPoor:=1]
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
    #    cat(nrow(MD[NewPoor==1]))
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]
    MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==3) |
                                 (Region=="Rural" & Decile==2),1,0)]
    MDP <- MD[Selected_Group==1,
              .(FPLine=0.001*
                  (weighted.mean(LavashPrice,Weight,na.rm = TRUE)*weighted.mean(BreadGrams,Weight,na.rm = TRUE)+
                     weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE)*weighted.mean(GrainGrams,Weight,na.rm = TRUE)+
                     weighted.mean(MacaroniPrice,Weight,na.rm = TRUE)*weighted.mean(MacaroniGram,Weight,na.rm = TRUE)+
                     weighted.mean(AdasPrice,Weight,na.rm = TRUE)*weighted.mean(AdasGram+Loobia_ChitiGram+NokhodGram,Weight,na.rm = TRUE)+
                     weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*weighted.mean(SibzaminiGram,Weight,na.rm = TRUE)+
                     weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE)*weighted.mean(VegetableShrubsGrams,Weight,na.rm = TRUE)+
                     weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE)*weighted.mean(TreeFruitsGrams,Weight,na.rm = TRUE)+
                     weighted.mean(LivestockGrams,Weight,na.rm = TRUE)*weighted.mean(LivestockGrams,Weight,na.rm = TRUE)+
                     weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE)*weighted.mean(PoultryMeat_MGram,Weight,na.rm = TRUE)+
                     weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE)*weighted.mean(Egg_MashinGram,Weight,na.rm = TRUE)+
                     weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE)*weighted.mean(MilkproductsGrams+MilkGrams,Weight,na.rm = TRUE)+
                     weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE)*weighted.mean(Oil_NabatiGram,Weight,na.rm = TRUE)+
                     weighted.mean(GhandPrice,Weight,na.rm = TRUE)*weighted.mean(GhandGram,Weight,na.rm = TRUE))),
              by=.(cluster3,Region)]
    MDP[is.na(MDP)] <- 0
    min<-MDP[FPLine>0,min(FPLine)]
    MDP[,FPLine:=ifelse(FPLine==0,min,FPLine)]
    
    Bundle <- MD[Selected_Group==1,
              .( BreadGrams=weighted.mean(BreadGrams/EqSizeCalory,Weight,na.rm = TRUE),
                  BerenjKhareji= weighted.mean((Rice_Khareji1Gram+Rice_Khareji2Gram)/EqSizeCalory,Weight,na.rm = TRUE),
                  MacaroniGram=  weighted.mean(MacaroniGram/EqSizeCalory,Weight,na.rm = TRUE),
                   HoboobatGram= weighted.mean((AdasGram+Loobia_ChitiGram+NokhodGram)/EqSizeCalory,Weight,na.rm = TRUE),
                SibzaminiGram=  weighted.mean(SibzaminiGram/EqSizeCalory,Weight,na.rm = TRUE),
                VegetableShrubsGrams= weighted.mean(VegetableShrubsGrams/EqSizeCalory,Weight,na.rm = TRUE),
                TreeFruitsGrams=   weighted.mean(TreeFruitsGrams/EqSizeCalory,Weight,na.rm = TRUE),
                CowMeatGram= weighted.mean(CowMeatGram/EqSizeCalory,Weight,na.rm = TRUE),
                SheepGrams= weighted.mean(SheepMeatGram/EqSizeCalory,Weight,na.rm = TRUE),
                PoultryMeat_MGram=  weighted.mean(PoultryMeat_MGram/EqSizeCalory,Weight,na.rm = TRUE),
                Egg_MashinGram=  weighted.mean(Egg_MashinGram/EqSizeCalory,Weight,na.rm = TRUE),
                MilkproductsGrams=  weighted.mean((MilkproductsGrams+MilkGrams)/EqSizeCalory,Weight,na.rm = TRUE),
                Oil_NabatiGram=  weighted.mean(Oil_NabatiGram/EqSizeCalory,Weight,na.rm = TRUE),
                GhandGram= weighted.mean((GhandGram+ShekarGram)/EqSizeCalory,Weight,na.rm = TRUE))
              ,by="Region"
              ]
    
    MD <- merge(MD,MDP,by=c("Region","cluster3"))
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