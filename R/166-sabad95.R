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
year<-95
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
               .(FPLine=0.001*
                   (weighted.mean(LavashPrice,Weight,na.rm = TRUE)*mean(Nan_Grams,na.rm = TRUE)+
                      weighted.mean(Rice_TaromPrice,Weight,na.rm = TRUE)*mean(Berenj_Grams)+
                      weighted.mean(MacaroniPrice,Weight,na.rm = TRUE)*mean(Makarooni_Grams)+
                      weighted.mean(AdasPrice,Weight,na.rm = TRUE)*mean(Hoboobat_Grams)+
                      weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*mean(Sibzamini_Grams)+
                      weighted.mean(Sabzi_KhordanPrice,Weight,na.rm = TRUE)*mean(Sabzi_Grams)+
                      weighted.mean(Banana_CoconutPrice,Weight,na.rm = TRUE)*mean(Mive_Grams)+
                      weighted.mean(CowMeatPrice,Weight,na.rm = TRUE)*mean(Goosht_Grams)+
                      weighted.mean(PoultryMeat_MPrice,Weight,na.rm = TRUE)*mean(Morgh_Grams)+
                      weighted.mean(Egg_MashinPrice,Weight,na.rm = TRUE)*mean(Tokhmemorgh_Grams)+
                      weighted.mean(Milk_PasteurizedPrice,Weight,na.rm = TRUE)*mean(Labaniat_Grams)+
                      weighted.mean(Oil_NabatiPrice,Weight,na.rm = TRUE)*mean(Roghan_Grams)+
                      weighted.mean(GhandPrice,Weight,na.rm = TRUE)*mean(Ghand_Grams))),
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
                 number=.N),
               by=.(cluster3,Region)]
    
    #by=.(cluster3,Region)]
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
                 number=.N),
               by=.(cluster3,Region)]
    
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

