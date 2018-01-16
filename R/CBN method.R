#CBN Method.R
# 
# Copyright © 2018:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

#for(year in (Settings$startyear:Settings$endyear)){
 # cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHBase.rda"))
  HHBase[,IndivNo:=NULL]
  HHBase[,Relationship:=NULL]
  HHBase[,Sex:=NULL]
  HHBase[,Age:=NULL]
  HHBase[,Literate:=NULL]
  HHBase[,Student:=NULL]
  HHBase[,EduCode:=NULL]
  HHBase[,EduYears:=NULL]
  HHBase[,EduLevel:=NULL]
  HHBase[,EduLevel0:=NULL]
  HHBase[,ActivityState:=NULL]
  HHBase[,MarritalState:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Ghand_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Hoboobat_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Roghan_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Berenj_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Nan_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Goosht_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Morgh_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mahi_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Shir_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mast_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Panir_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Tokhmemorgh_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Mive_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sabzi_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Makarooni_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Sibzamini_Data.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Weights.rda"))
  
  Ghand_Data<-Ghand_Data[,.(HHID,Ghandgram,GhandPrice)]
  Hoboobat_Data<-Hoboobat_Data[,.(HHID,Hoboobatgram,HoboobatPrice)]
  Roghan_Data<-Roghan_Data[,.(HHID,Roghangram,RoghanPrice)]
  Berenj_Data<-Berenj_Data[,.(HHID,Berenjgram,BerenjPrice)]
  Nan_Data<-Nan_Data[,.(HHID,Nangram,NanPrice)]
  Goosht_Data<-Goosht_Data[,.(HHID,Gooshtgram,GooshtPrice)]
  Morgh_Data<-Morgh_Data[,.(HHID,Morghgram,MorghPrice)]
  Mahi_Data<-Mahi_Data[,.(HHID,Mahigram,MahiPrice)]
  Shir_Data<-Shir_Data[,.(HHID,Shirgram,ShirPrice)]
  Mast_Data<-Mast_Data[,.(HHID,Mastgram,MastPrice)]
  Panir_Data<-Panir_Data[,.(HHID,Panirgram,PanirPrice)]
  Tokhmemorgh_Data<-Tokhmemorgh_Data[,.(HHID,Tokhmemorghgram,TokhmemorghPrice)]
  Mive_Data<-Mive_Data[,.(HHID,Mivegram,MivePrice)]
  Sabzi_Data<-Sabzi_Data[,.(HHID,Sabzigram,SabziPrice)]
  Makarooni_Data<-Makarooni_Data[,.(HHID,Makaroonigram,MakarooniPrice)]
  Sibzamini_Data<-Sibzamini_Data[,.(HHID,Sibzaminigram,SibzaminiPrice)]
  
  Food<-merge(HHBase,Ghand_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Hoboobat_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Roghan_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Berenj_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Nan_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Goosht_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Morgh_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Mahi_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Shir_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Mast_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Panir_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Tokhmemorgh_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Mive_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Sabzi_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Makarooni_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Sibzamini_Data,by =c("HHID"),all.x=TRUE)
  Food<-merge(Food,Weights,by =c("HHID"),all.x=TRUE)
  Food[is.na(Food)] <- 0
  

  #load Expenditure groups
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Foods.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Cigars.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Cloths.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Amusements.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Communications.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Durables.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Education.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Energy.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Furnitures.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Hotels.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","House.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Medicals.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Behdashts.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Transportations.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Others.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Investments.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Weights.rda"))
  
  #merge Expenditure groups
  CBN<-merge(Food,HHI ,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FoodData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,CigarData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,ClothData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,AmusementData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,CommunicationData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,EducData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,EnergyData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FurnitureData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,HotelData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,HouseData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,BehdashtData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,TransportationData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,OtherData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,InvestmentData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,MedicalData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,DurableData,by =c("HHID"),all=TRUE)
  CBN[is.na(CBN)] <- 0
  CBN<-CBN[Size!=0]

  
  #Calculate Per_Total Expenditures Monthly
  CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(62:74,76:77)][] 
  CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=62:74][] 

  CBN$Total_Exp_Month_Per<-CBN$Total_Exp_Month/CBN$EqSizeRevOECD
  CBN$Total_Exp_Month_Per_nondurable<-CBN$Total_Exp_Month_nondurable/CBN$EqSizeRevOECD
  CBN$FoodExpenditure_Per<-CBN$FoodExpenditure/CBN$EqSizeRevOECD
  
  #Sort Expenditure data
  CBN<- CBN[order(Total_Exp_Month_Per_nondurable)]

  #Calculate cumulative weights
  sum(CBN$Weight)
  CBN$cumweight <- cumsum(CBN$Weight)
  tx <- max(CBN$cumweight)
  
  #Calculate deciles by weights
  CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
  CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]


  CBN[,Poor:=ifelse(Decile %in% 1:2,1,0)]
  CBNPoor<-CBN[Poor==1]
  test<-CBNPoor[,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice,Region,ProvinceCode,Weight)]
  dt2 <- test[,lapply(.SD,weighted.mean,w=Weight),by=.(Region,ProvinceCode)]
  dt2<- dt2[order(ProvinceCode,Region)]
  dt <- dt2 [,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice)]
   
   pca <- princomp(dt, cor=T) 
    PRICE <- pca$scores
    PRICE1 <- -1*PRICE[,1] 
    PRICE2 <- -1*PRICE[,2] 
    PRICE3 <- -1*PRICE[,3] 
    PRICE4 <- -1*PRICE[,4] 
    PRICE5 <- -1*PRICE[,5]
    PRICE6 <- -1*PRICE[,6] 
    PRICE7 <- -1*PRICE[,7] 
    PRICE8 <- -1*PRICE[,8] 
    PRICE9 <- -1*PRICE[,9] 
    PRICE10 <- -1*PRICE[,10] 
    PRICE11 <- -1*PRICE[,11] 
    PRICE12 <- -1*PRICE[,12] 
    PRICE13 <- -1*PRICE[,13]
    PRICE14 <- -1*PRICE[,14] 
    PRICE15 <- -1*PRICE[,15] 
    PRICE16 <- -1*PRICE[,16] 
    
    #X12 <- cbind(PRICE1, PRICE2)
    cl <- kmeans(PRICE,5)
    cl$cluster
    dt2 <- dt2[,cluster:=data.table(cl$cluster)]
    dt2<-dt2[,.(ProvinceCode,Region,cluster)]
    #plot(PRICE1, PRICE2,col=cl$cluster)
    #points(cl$centers, pch=20)
    CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode","Region"),all.x = TRUE)
    C2<-CBNPoor[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]

  CBNPoor<-CBNPoor[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
  CBNPoor<-CBNPoor[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]
  
  CBNPoor<-CBNPoor[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
  CBNPoor<-CBNPoor[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor<-CBNPoor[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]
  
  
  #CBN[,weighted.mean(GhandPrice,Weight,na.rm = TRUE),GhandPrice!=0]
  #CBN[,weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),HoboobatPrice!=0]
  #CBN[,weighted.mean(RoghanPrice,Weight,na.rm = TRUE),RoghanPrice!=0]
  #CBN[,weighted.mean(BerenjPrice,Weight,na.rm = TRUE),BerenjPrice!=0]
  #CBN[,weighted.mean(NanPrice,Weight,na.rm = TRUE),NanPrice!=0]
  #CBN[,weighted.mean(GooshtPrice,Weight,na.rm = TRUE),GooshtPrice!=0]
  #CBN[,weighted.mean(MorghPrice,Weight,na.rm = TRUE),MorghPrice!=0]
  #CBN[,weighted.mean(MahiPrice,Weight,na.rm = TRUE),MahiPrice!=0]
  #CBN[,weighted.mean(ShirPrice,Weight,na.rm = TRUE),ShirPrice!=0]
  #CBN[,weighted.mean(MastPrice,Weight,na.rm = TRUE),MastPrice!=0]
  #CBN[,weighted.mean(PanirPrice,Weight,na.rm = TRUE),PanirPrice!=0]
  #CBN[,weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),TokhmemorghPrice!=0]
  #CBN[,weighted.mean(MivePrice,Weight,na.rm = TRUE),MivePrice!=0]
  #CBN[,weighted.mean(SabziPrice,Weight,na.rm = TRUE),SabziPrice!=0]
  #CBN[,weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),MakarooniPrice!=0]
  #CBN[,weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),SibzaminiPrice!=0]

  CBNPoor[,GhandPoor:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,HoboobatPoor:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,RoghanPoor:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,BerenjPoor:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,NanPoor:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,GooshtPoor:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MorghPoor:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MahiPoor:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,ShirPoor:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MastPoor:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,PanirPoor:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,TokhmemorghPoor:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MivePoor:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,SabziPoor:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MakarooniPoor:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,SibzaminiPoor:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[, Poors_Expenditures := Reduce(`+`, .SD), .SDcols=121:136][] 
  
  
  # Food Calories
  CBNPoor$Ghand_Calory<- CBNPoor$Ghandgram *4
  CBNPoor$Hoboobat_Calory<- CBNPoor$Hoboobatgram *3
  CBNPoor$Nan_Calory<- CBNPoor$Nangram *2.5
  CBNPoor$Berenj_Calory<- CBNPoor$Berenjgram *1.2
  CBNPoor$Roghan_Calory<- CBNPoor$Roghangram *8
  CBNPoor$Goosht_Calory<- CBNPoor$Gooshtgram *2.5
  CBNPoor$Morgh_Calory<- CBNPoor$Morghgram *2
  CBNPoor$Mahi_Calory<- CBNPoor$Mahigram *1
  CBNPoor$Shir_Calory<- CBNPoor$Shirgram *2.5
  CBNPoor$Mast_Calory<- CBNPoor$Mastgram *1.5
  CBNPoor$Panir_Calory<- CBNPoor$Mastgram *2.5
  CBNPoor$Tokhmemorgh_Calory<- CBNPoor$Tokhmemorghgram *1.4
  CBNPoor$Mive_Calory<- CBNPoor$Mivegram *0.5
  CBNPoor$Sabzi_Calory<- CBNPoor$Sabzigram *0.5
  CBNPoor$Makarooni_Calory<- CBNPoor$Makaroonigram *3.6
  CBNPoor$Sibzamini_Calory<- CBNPoor$Sibzaminigram *0.9
  utils::View(CBNPoor)
  
  CBNPoor[, Daily_Calories := Reduce(`+`, .SD), .SDcols=138:153][] 
  CBNPoor <- CBNPoor[Daily_Calories<100000] # arbitrary removal of outliers
  CBNPoor[,Daily_Calories_cluster:=weighted.mean(Daily_Calories,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Ghand_cluster:=weighted.mean(Ghand_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Nan_cluster:=weighted.mean(Nan_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Berenj_cluster:=weighted.mean(Berenj_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Roghan_cluster:=weighted.mean(Roghan_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Goosht_cluster:=weighted.mean(Goosht_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Morgh_cluster:=weighted.mean(Morgh_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mahi_cluster:=weighted.mean(Mahi_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Shir_cluster:=weighted.mean(Shir_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mast_cluster:=weighted.mean(Mast_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Panir_cluster:=weighted.mean(Panir_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mive_cluster:=weighted.mean(Mive_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[, Daily_Calories2 := Reduce(`+`, .SD), .SDcols=c(156:170,172:172)][] 
  
  CBNPoor[,Daily2_Ghand_cluster:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Hoboobat_cluster:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Nan_cluster:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Berenj_cluster:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Roghan_cluster:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Goosht_cluster:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Morgh_cluster:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Mahi_cluster:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Shir_cluster:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Mast_cluster:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Panir_cluster:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Tokhmemorgh_cluster:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Mive_cluster:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Sabzi_cluster:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Makarooni_cluster:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster)]
  CBNPoor[,Daily2_Sibzamini_cluster:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster)]
  
  utils::View(CBNPoor)
  
  CBNPoor[,Year:=NULL]
  CBNPoor[,Quarter:=NULL]
  # CBNPoor[,Month:=NULL]
  # CBNPoor[,ProvinceCode:=NULL]
  CBNPoor[,Dimension:=NULL]
  CBNPoor[,CBNPoorExpenditure:=NULL]
  
#CalculatePer_calories
  CBN[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
  CBN[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
  
  CBN$Ghandgram<-CBN$Ghandgram/CBN$EqSizeCalory
  CBN$Hoboobatgram<-CBN$Hoboobatgram/CBN$EqSizeCalory
  CBN$Roghangram<-CBN$Roghangram/CBN$EqSizeCalory
  CBN$Berenjgram<-CBN$Berenjgram/CBN$EqSizeCalory
  CBN$Nangram<-CBN$Nangram/CBN$EqSizeCalory
  CBN$Gooshtgram<-CBN$Gooshtgram/CBN$EqSizeCalory
  CBN$Morghgram<-CBN$Morghgram/CBN$EqSizeCalory
  CBN$Mahigram<-CBN$Mahigram/CBN$EqSizeCalory
  CBN$Shirgram<-CBN$Shirgram/CBN$EqSizeCalory
  CBN$Mastgram<-CBN$Mastgram/CBN$EqSizeCalory
  CBN$Panirgram<-CBN$Panirgram/CBN$EqSizeCalory
  CBN$Tokhmemorghgram<-CBN$Tokhmemorghgram/CBN$EqSizeCalory
  CBN$Mivegram<-CBN$Mivegram/CBN$EqSizeCalory
  CBN$Sabzigram<-CBN$Sabzigram/CBN$EqSizeCalory
  CBN$Makaroonigram<-CBN$Makaroonigram/CBN$EqSizeCalory
  CBN$Sibzaminigram<-CBN$Sibzaminigram/CBN$EqSizeCalory
  
  
  CBN[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=Poor]
  exp1<-2965629
  CBN[,Poor:=ifelse(Total_Exp_Month_Per_nondurable < 2965629,1,0)]
  CBN[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=Poor]
  exp1<-2240735
  CBN[,Poor:=ifelse(Total_Exp_Month_Per_nondurable < 2240735,1,0)]
  CBN[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=Poor]
  exp1<-1723594
  CBN[,Poor:=ifelse(Total_Exp_Month_Per_nondurable < 1723594,1,0)]
  CBN[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=Poor]
  exp1<-1723594
  
  FoodRural<-MyFood[(Food$Region=="Rural"),]
  FoodUrban<-MyFood[(Food$Region=="Urban"),]
  FoodRural[,Region:=NULL]
  FoodUrban[,Region:=NULL]
  save(Food, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food.rda"))
  save(FoodRural, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Rural.rda"))
  save(FoodUrban, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Urban.rda"))
#}

# save(MyFood, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
# save(MyFoodRural, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Rural.rda"))
# save(MyFoodUrban, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories_Urban.rda"))

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

