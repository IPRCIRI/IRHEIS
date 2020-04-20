# 177- Step 7,8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

FinalCountryResults <- data.table(Year=NA_integer_,PovertyLine=NA_real_,
                                  Engle=NA_real_,Bundle_Value=NA_real_,
                                  Total_Exp_Month_Per=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalRegionResults <- data.table(Year=NA_integer_,Region=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]




for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor98.rda"))
  
  #MD<-MD[Region=="Rural"]
  #MD<-MD[cluster3==13]

  
 
  EngleD<- MD[TOriginalFoodExpenditure_Per>0.8*FPLine &
                TOriginalFoodExpenditure_Per<1.2*FPLine,
               .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region)]


  
  EngleD[,PovertyLine:=FPLine/Engel]
  MD <- merge(MD,EngleD[,.(Region,PovertyLine,Engel)],by=c("Region"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS98.rda"))
  

  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per)/PovertyLine)^2]

  
  ################Country##################

  X1 <- MD[,.(PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Engle=weighted.mean(HHEngle,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight))]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size))]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by="Year")
  FinalCountryResults <- rbind(FinalCountryResults,X)
  
  ################Region##################
  X1 <- MD[,.(PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=Region]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=Region]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","Region"))
  FinalRegionResults <- rbind(FinalRegionResults,X)

  

  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)],"\t")
  cat(MD[, weighted.mean(PovertyLine,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FPLine,Weight*Size)],"\t")
  cat(MD[, sum(Weight*Size)],"\t")
  cat(MD[, weighted.mean(Engel,Weight*Size)],"\t")

  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
  
  Poors<-MD[FinalPoor==1]
  
MD[,weighted.mean(Bundle_Value,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Region)][order(Region)]
MD[,weighted.mean(Bundle_Value,Weight)]

MD[,weighted.mean(FPLine,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(FPLine,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(FPLine,Weight),by=.(Region)][order(Region)]
MD[,weighted.mean(FPLine,Weight)]

MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)][order(Region)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight)]

MD[as.numeric(Decile)==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)][order(Region)]
MD[as.numeric(Decile)==1,weighted.mean(TFoodKCaloriesHH_Per,Weight)]

MD[,weighted.mean(OriginalFoodExpenditure_Per,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(OriginalFoodExpenditure_Per,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(OriginalFoodExpenditure_Per,Weight),by=.(Region)][order(Region)]
MD[,weighted.mean(OriginalFoodExpenditure_Per,Weight)]

MD[FinalPoor==1,weighted.mean(OriginalFoodExpenditure_Per,Weight),by=.(Region)][order(Region)]
MD[FinalPoor==1,weighted.mean(OriginalFoodExpenditure_Per,Weight)]


load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodCon.rda"))
MD<-merge(MD,TotalFoodCon,by="HHID")

load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
MD<-merge(MD,TotalFoodExp,by="HHID")


MD[,weighted.mean(G0111,Weight.x)]
MD[,weighted.mean(G01181,Weight.x)]
MD[,weighted.mean(G01121+SheepMeatGram,Weight.x)]
MD[,weighted.mean(G01176,Weight.x)]
MD[,weighted.mean(G01131,Weight.x)]
MD[,weighted.mean(`011164`,Weight.x)]
MD[,weighted.mean(`011424`,Weight.x)]
MD[,weighted.mean(G0116,Weight.x)]
MD[,weighted.mean(G01123,Weight.x)]
MD[,weighted.mean(G01114,Weight.x)]
MD[,weighted.mean(`011428`,Weight.x)]
MD[,weighted.mean(G01153+G01152+G01151,Weight.x)]
MD[,weighted.mean(G0117,Weight.x)]
MD[,weighted.mean(G01141,Weight.x)]
MD[,weighted.mean(`011731`,Weight.x)]
MD[,weighted.mean(G01144,Weight.x)]
MD[,weighted.mean(G01165+G01166,Weight.x)]

MD[FinalPoor==1,weighted.mean(G0111,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01181,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01121+SheepMeatGram,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01176,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01131,Weight.x)]
MD[FinalPoor==1,weighted.mean(`011164`,Weight.x)]
MD[FinalPoor==1,weighted.mean(`011424`,Weight.x)]
MD[FinalPoor==1,weighted.mean(G0116,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01123,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01114,Weight.x)]
MD[FinalPoor==1,weighted.mean(`011428`,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01153+G01152+G01151,Weight.x)]
MD[FinalPoor==1,weighted.mean(G0117,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01141,Weight.x)]
MD[FinalPoor==1,weighted.mean(`011731`,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01144,Weight.x)]
MD[FinalPoor==1,weighted.mean(G01165+G01166,Weight.x)]

MD[,weighted.mean(G0111,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01181,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01121+SheepMeatGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01176,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01131,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(`011164`,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(`011424`,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G0116,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01123,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01114,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(`011428`,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01153+G01152+G01151,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G0117,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01141,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(`011731`,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01144,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(G01165+G01166,Weight.x), by=Decile][order(Decile)]


MD[,weighted.mean(GrainGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(GhandGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(CowMeatGram+SheepMeatGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(BeansGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(FishandShrimpsGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(MacaroniGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(Yogurt_PasturizedGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(TreeFruitsGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(PoultryMeat_MGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(BreadGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(Cheese_PasturizedGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(Oil_NabatiGram+Oil_AnimalGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(Sabzi_AshGram+Sabzi_KhordanGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(MilkGrams,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(SibzaminiGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight), by=Decile][order(Decile)]
MD[,weighted.mean(NutsGrams,Weight), by=Decile][order(Decile)]

MD[,weighted.mean(GrainGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(GhandGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(CowMeatGram+SheepMeatGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(BeansGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(FishandShrimpsGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(MacaroniGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(Yogurt_PasturizedGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(TreeFruitsGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(PoultryMeat_MGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(BreadGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(Cheese_PasturizedGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(Oil_NabatiGram+Oil_AnimalGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(Sabzi_AshGram+Sabzi_KhordanGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(MilkGrams,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(SibzaminiGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight.x), by=Decile][order(Decile)]
MD[,weighted.mean(NutsGrams,Weight.x), by=Decile][order(Decile)]


MD[FinalPoor==1,weighted.mean(GrainGrams,Weight)]
MD[FinalPoor==1,weighted.mean(GhandGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(CowMeatGram+SheepMeatGram,Weight)]
MD[FinalPoor==1,weighted.mean(BeansGrams,Weight)]
MD[FinalPoor==1,weighted.mean(FishandShrimpsGrams,Weight)]
MD[FinalPoor==1,weighted.mean(MacaroniGram,Weight)]
MD[FinalPoor==1,weighted.mean(Yogurt_PasturizedGram,Weight)]
MD[FinalPoor==1,weighted.mean(TreeFruitsGrams,Weight)]
MD[FinalPoor==1,weighted.mean(PoultryMeat_MGram,Weight)]
MD[FinalPoor==1,weighted.mean(BreadGrams,Weight)]
MD[FinalPoor==1,weighted.mean(Cheese_PasturizedGram,Weight)]
MD[FinalPoor==1,weighted.mean(Oil_NabatiGram+Oil_AnimalGram,Weight)]
MD[FinalPoor==1,weighted.mean(Sabzi_AshGram+Sabzi_KhordanGram,Weight)]
MD[FinalPoor==1,weighted.mean(MilkGrams,Weight)]
MD[FinalPoor==1,weighted.mean(SibzaminiGram,Weight)]
MD[FinalPoor==1,weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight)]
MD[FinalPoor==1,weighted.mean(NutsGrams,Weight)]


MD[as.numeric(Decile)==1,weighted.mean(GrainGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(GhandGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Cow_LiveGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(BeansGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(FishandShrimpsGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(MacaroniGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Yogurt_PasturizedGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(TreeFruitsGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(PoultryMeat_MGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(BreadGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Cheese_PasturizedGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Oil_NabatiGram+Oil_AnimalGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Sabzi_AshGram+Sabzi_KhordanGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(MilkGrams,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(SibzaminiGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(Egg_MashinGram+Egg_NonMashinGram,Weight)]
MD[as.numeric(Decile)==1,weighted.mean(NutsGrams,Weight)]






MD[as.numeric(Decile)<2,weighted.mean(FinalPoor,Weight)]
    
    MD5<-MD[ProvinceCode!=11]
    MD5<-MD5[,Group:=ifelse(FinalPoor==0 & FinalFoodPoor==0,"NoPoor",
                          ifelse(FinalPoor==1 & FinalFoodPoor==0,"OnlyFinalPoor",
                           ifelse(FinalPoor==0 & FinalFoodPoor==1,"OnlyFoodPoor","Both")))]
    a<-ggplot(MD5,aes(x=HHEngle, fill=factor(Group))) +
      geom_density(alpha=0.25)+
      xlim(0,1)+ylim(0,5)+
      ggtitle(year)
    plot(a)

    
    
    MDD<-MD[,.(Population=sum(Weight*Size)), by=.(Decile,ProvinceName)][order(ProvinceName,Decile)]
      ggplot(MDD, aes(fill=factor(Decile), y=Population, x=ProvinceName)) + 
      geom_bar(position="fill", stat="identity") + theme_bw() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

    
    
    Decile1<-MD[as.numeric(Decile)<2,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile1Poors<-Decile1[,.(Decile1Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile1Poors, aes( y=Decile1Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    Decile2<-MD[as.numeric(Decile)<3,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile2Poors<-Decile2[,.(Decile2Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile2Poors, aes( y=Decile2Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    Decile5<-MD[as.numeric(Decile)>3,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile5Poors<-Decile5[,.(Decile5Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile5Poors, aes( y=Decile5Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
 
    

    MD[,weighted.mean(FinalPoor,Weight),by=ProvinceCode][order(ProvinceCode)]
}

MD[,sum(Weight),by=Decile][order(Decile)]
MD[,sum(Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,sum(Weight)]
MD[,sum(Weight*Size)]

MD<-MD[,Total:=OriginalFoodExpenditure+Cigar_Exp+Cloth_Exp+
         HouseandEnergy_Exp+Furniture_Exp+Hygiene_Exp+
         Transportation_Exp+Communication_Exp+Communication_Exp+
         Amusement_Exp+Education_Exp+HotelRestaurant_Exp+
         Other_Exp+Durable_Exp]
m<-MD[,.(Total,Total_Exp_Month)]

Share<-MD[,.(Foodshare=weighted.mean(OriginalFoodExpenditure/Total,Weight),
             CigarShare=weighted.mean(Cigar_Exp/Total,Weight),
             ClothShare=weighted.mean(Cloth_Exp/Total,Weight),
             HouseandEnergyShare=weighted.mean(HouseandEnergy_Exp/Total,Weight),
             FurnitureShare=weighted.mean(Furniture_Exp/Total,Weight),
             HygieneShare=weighted.mean(Hygiene_Exp/Total,Weight),
             TransportationShare=weighted.mean(Transportation_Exp/Total,Weight),
             CommunicationShare=weighted.mean(Communication_Exp/Total,Weight),
             AmusementShare=weighted.mean(Amusement_Exp/Total,Weight),
             EducationShare=weighted.mean(Education_Exp/Total,Weight),
             HotelShare=weighted.mean(HotelRestaurant_Exp/Total,Weight),
             OtherShare=weighted.mean(Other_Exp/Total,Weight),
             DurableShare=weighted.mean(Durable_Exp/Total,Weight)),by=Decile]



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")