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
                  (weighted.mean(LavashPrice,Weight,na.rm = TRUE)*weighted.mean(BreadGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE)*weighted.mean(GrainGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(MacaroniPrice,Weight,na.rm = TRUE)*weighted.mean(MacaroniGram/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(AdasPrice,Weight,na.rm = TRUE)*weighted.mean((AdasGram+Loobia_ChitiGram+NokhodGram)/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*weighted.mean(SibzaminiGram/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE)*weighted.mean(VegetableShrubsGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE)*weighted.mean(TreeFruitsGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(LivestockGrams,Weight,na.rm = TRUE)*weighted.mean(LivestockGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE)*weighted.mean(PoultryMeat_MGram/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE)*weighted.mean(Egg_MashinGram/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE)*weighted.mean((MilkproductsGrams+MilkGrams)/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE)*weighted.mean(Oil_NabatiGram/(EqSizeCalory),Weight*Size,na.rm = TRUE)+
                     weighted.mean(GhandPrice,Weight,na.rm = TRUE)*weighted.mean(GhandGram/(EqSizeCalory),Weight*Size,na.rm = TRUE))),
              by=.(cluster3,Region)]
   price<- MD[Selected_Group==1,
       .(LavashPrice=weighted.mean(LavashPrice,Weight,na.rm = TRUE),
         Rice_TaromPrice=weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE),
         MacaroniPrice=weighted.mean(MacaroniPrice,Weight,na.rm = TRUE),
         HobubatPrice=weighted.mean(AdasPrice,Weight,na.rm = TRUE),
         SibzaminiPrice=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),
         Sabzi_KhordanPrice=weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE),
         Banana_CoconutPrice=weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE),
         LivestockGrams=weighted.mean(LivestockGrams,Weight,na.rm = TRUE),
         PoultryMeat_MPrice=weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE),
         Egg_MashinPrice=weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE),
         Milk_PasteurizedPrice=weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE),
         Oil_NabatiPrice=weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE),
         GhandPrice=weighted.mean(GhandPrice,Weight,na.rm = TRUE))]
       #by=.(cluster3,Region)]
   gram <- MD[Selected_Group==1,
             .(BreadGrams=weighted.mean(BreadGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               GrainGrams=weighted.mean(GrainGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               MacaroniGram=weighted.mean(MacaroniGram/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               HobubatGrams=weighted.mean((AdasGram+Loobia_ChitiGram+NokhodGram)/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               SibzaminiGram=weighted.mean(SibzaminiGram/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               VegetableShrubsGrams=weighted.mean(VegetableShrubsGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               TreeFruitsGrams=weighted.mean(TreeFruitsGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               LivestockGrams=weighted.mean(LivestockGrams/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               PoultryMeat_MGram=weighted.mean(PoultryMeat_MGram/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               Egg_MashinGram=weighted.mean(Egg_MashinGram/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               MilkproductsGrams=weighted.mean((MilkproductsGrams+MilkGrams)/(EqSizeCalory),Weight*Size,na.rm = TRUE),
               Oil_NabatiGram=weighted.mean(Oil_NabatiGram/(EqSizeCalory),Weight,na.rm = TRUE),
               GhandGram=weighted.mean(GhandGram/(EqSizeCalory),Weight*Size,na.rm = TRUE))]
            # by=.(cluster3,Region)]
   gram<-t(gram)
    MDP[is.na(MDP)] <- 0
    min<-MDP[FPLine>0,min(FPLine)]
    MDP[,FPLine:=ifelse(FPLine==0,min,FPLine)]
    
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