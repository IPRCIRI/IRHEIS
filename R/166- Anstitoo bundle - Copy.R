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
  
  MD<-merge(MD,FoodPrices,all.x=TRUE,by="HHID")

  
  MD[,NewPoor:=InitialPoor]
  MD[,OldPoor:=1]
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
    #    cat(nrow(MD[NewPoor==1]))
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]    
    MDP <- MD[ThisIterationPoor==1,
              .(FPLine=0.001*30*0.5*(310*weighted.mean(LavashPrice,Weight,na.rm = TRUE)+
                  95*weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE)+
                  20*weighted.mean(MacaroniPrice,Weight,na.rm = TRUE)+
                  26*weighted.mean(AdasPrice,Weight,na.rm = TRUE)+
                  70*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)+
                  300*weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE)+
                  280*weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE)+
                  38*weighted.mean(CowMeatPrice,Weight,na.rm = TRUE)+
                  64*weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE)+
                  35*weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE)+
                  250*weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE)+
                  35*weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE)+
                  40*weighted.mean(GhandPrice,Weight,na.rm = TRUE))),
              by=.(cluster3,Region)]
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
  x<-MD[,weighted.mean(OriginalFoodExpenditure,Weight),by="cluster3"]
  
  
#  cat(MD[,weighted.mean(TOriginalFoodExpenditure_Per,Weight)],"\n")
#  cat(MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE)],"\n")
#  cat(MD[,weighted.mean(Calory_Price,Weight,na.rm = TRUE)],"\n")
  cat(MD[cluster3==1,weighted.mean(FPLine,Weight,na.rm = TRUE)],"\n")
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")