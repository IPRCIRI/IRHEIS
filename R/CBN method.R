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
  for (col in c("Ghandgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Hoboobat_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Hoboobatgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Roghan_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Roghangram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Berenj_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Berenjgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Nan_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Nangram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Goosht_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Gooshtgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Morgh_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Morghgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Mahi_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Mahigram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Shir_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Shirgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Mast_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Mastgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Panir_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Panirgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Tokhmemorgh_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Tokhmemorghgram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Mive_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Mivegram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Sabzi_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Sabzigram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Makarooni_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Makaroonigram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Sibzamini_Data,by =c("HHID"),all.x=TRUE)
  for (col in c("Sibzaminigram")) Food[is.na(get(col)), (col) := 0]
  Food<-merge(Food,Weights,by =c("HHID"),all.x=TRUE)

  

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
  for (col in c("FoodExpenditure")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,CigarData,by =c("HHID"),all=TRUE)
  for (col in c("Cigar_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,ClothData,by =c("HHID"),all=TRUE)
  for (col in c("Cloth_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,AmusementData,by =c("HHID"),all=TRUE)
  for (col in c("Amusement_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,CommunicationData,by =c("HHID"),all=TRUE)
  for (col in c("Communication_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,EducData,by =c("HHID"),all=TRUE)
  for (col in c("EducExpenditure")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,EnergyData,by =c("HHID"),all=TRUE)
  for (col in c("Energy_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,FurnitureData,by =c("HHID"),all=TRUE)
  for (col in c("Furniture_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,HotelData,by =c("HHID"),all=TRUE)
  for (col in c("Hotel_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,HouseData,by =c("HHID"),all=TRUE)
  for (col in c("ServiceExp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,BehdashtData,by =c("HHID"),all=TRUE)
  for (col in c("Behdasht_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,TransportationData,by =c("HHID"),all=TRUE)
  for (col in c("Transportation_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,OtherData,by =c("HHID"),all=TRUE)
  for (col in c("Other_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("Investment_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,MedicalData,by =c("HHID"),all=TRUE)
  for (col in c("Medical_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-merge(CBN,DurableData,by =c("HHID"),all=TRUE)
  for (col in c("Durable_Exp")) CBN[is.na(get(col)), (col) := 0]
  CBN<-CBN[Size!=0]

  
  #Calculate Per_Total Expenditures Monthly
  CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(65:77,79:80)][] 
  CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=65:77][] 

  CBN$Total_Exp_Month_Per<-CBN$Total_Exp_Month/CBN$EqSizeRevOECD
  CBN$Total_Exp_Month_Per_nondurable<-CBN$Total_Exp_Month_nondurable/CBN$EqSizeRevOECD
  
  #Calculate Per_Food Expenditures Monthly
  CBN$FoodExpenditure_Per<-CBN$FoodExpenditure/CBN$EqSizeRevOECD
  
  #Calculate Per_Food Expenditures Daily
  CBN$FoodExpenditure_Per_day<-CBN$FoodExpenditure_Per/30
  
  CBN$Ghandgram_Per_day<-CBN$Ghandgram/(30*CBN$EqSizeRevOECD)
  CBN$Hoboobatgram_Per_day<-CBN$Hoboobatgram/(30*CBN$EqSizeRevOECD)
  CBN$Berenjgram_Per_day<-CBN$Berenjgram/(30*CBN$EqSizeRevOECD)
  CBN$Nangram_Per_day<-CBN$Nangram/(30*CBN$EqSizeRevOECD)
  CBN$Roghangram_Per_day<-CBN$Roghangram/(30*CBN$EqSizeRevOECD)
  CBN$Gooshtgram_Per_day<-CBN$Gooshtgram/(30*CBN$EqSizeRevOECD)
  CBN$Morghgram_Per_day<-CBN$Morghgram/(30*CBN$EqSizeRevOECD)
  CBN$Mahigram_Per_day<-CBN$Mahigram/(30*CBN$EqSizeRevOECD)
  CBN$Shirgram_Per_day<-CBN$Shirgram/(30*CBN$EqSizeRevOECD)
  CBN$Mastgram_Per_day<-CBN$Mastgram/(30*CBN$EqSizeRevOECD)
  CBN$Panirgram_Per_day<-CBN$Panirgram/(30*CBN$EqSizeRevOECD)
  CBN$Tokhmemorghgram_Per_day<-CBN$Tokhmemorghgram/(30*CBN$EqSizeRevOECD)
  CBN$Mivegram_Per_day<-CBN$Mivegram/(30*CBN$EqSizeRevOECD)
  CBN$Sabzigram_Per_day<-CBN$Sabzigram/(30*CBN$EqSizeRevOECD)
  CBN$Makaroonigram_Per_day<-CBN$Makaroonigram/(30*CBN$EqSizeRevOECD)
  CBN$Sibzaminigram_Per_day<-CBN$Sibzaminigram/(30*CBN$EqSizeRevOECD)
  
  
  #Sort Expenditure data
  CBN<- CBN[order(Total_Exp_Month_Per_nondurable)]

  #Calculate cumulative weights
  sum(CBN$Weight)
  CBN$cumweight <- cumsum(CBN$Weight)
  tx <- max(CBN$cumweight)
  
  #Calculate deciles by weights
  CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
  CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

  #Assume that deciles 1 and 2 are poor
  CBN[,Poor:=ifelse(Decile %in% 1:2,1,0)]
  CBNPoor<-CBN[Poor==1]
  
  #K-means algorithm for clustering by prices
  test<-CBNPoor[,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice,Region,ProvinceCode,Weight)]
  dt2 <- test[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,ProvinceCode)]
  dt2<- dt2[order(ProvinceCode,Region)]
  for (col in c("MahiPrice")) dt2[is.nan(get(col)), (col) := 200000]
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
######################################################################    
    #K-means algorithm for clustering by consumption
    test3<-CBNPoor[,.(Ghandgram,Hoboobatgram,Roghangram,Berenjgram,Nangram,Gooshtgram,Morghgram,Mahigram,Shirgram,Mastgram,Panirgram,Tokhmemorghgram,Mivegram,Sabzigram,Makaroonigram,Sibzaminigram,Region,ProvinceCode,Weight)]
    dt3 <- test3[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,ProvinceCode)]
    dt3<- dt3[order(ProvinceCode,Region)]
    #for (col in c("MahiPrice")) dt3[is.nan(get(col)), (col) := 200000]
    dtt <- dt3 [,.(Ghandgram,Hoboobatgram,Roghangram,Berenjgram,Nangram,Gooshtgram,Morghgram,Mahigram,Shirgram,Mastgram,Panirgram,Tokhmemorghgram,Mivegram,Sabzigram,Makaroonigram,Sibzaminigram)]
    
    pca2 <- princomp(dtt, cor=T)
    Gram <- pca2$scores
    Gram1 <- -1*Gram[,1] 
    Gram2 <- -1*Gram[,2] 
    Gram3 <- -1*Gram[,3] 
    Gram4 <- -1*Gram[,4] 
    Gram5 <- -1*Gram[,5]
    Gram6 <- -1*Gram[,6] 
    Gram7 <- -1*Gram[,7] 
    Gram8 <- -1*Gram[,8] 
    Gram9 <- -1*Gram[,9] 
    Gram10 <- -1*Gram[,10] 
    Gram11 <- -1*Gram[,11] 
    Gram12 <- -1*Gram[,12] 
    Gram13 <- -1*Gram[,13]
    Gram14 <- -1*Gram[,14] 
    Gram15 <- -1*Gram[,15] 
    Gram16 <- -1*Gram[,16] 
    
    #X12 <- cbind(Gram1, Gram2)
    cl2 <- kmeans(Gram,5)
    cl2$cluster
    dt3 <- dt3[,cluster:=data.table(cl$cluster)]
    dt3<-dt3[,.(ProvinceCode,Region,cluster)]
    #plot(Gram1, Gram2,col=cl$cluster)
    #points(cl$centers, pch=20)
    CBNPoor8<-merge(CBNPoor,dt3,by=c("ProvinceCode","Region"),all.x = TRUE)
    C28<-CBNPoor8[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]
##########################################################
    #weighted consumption in each cluster
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
  
  #weighted prices in each cluster
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
  
  #weighted food expenditures in each cluster
  CBNPoor[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=140:155][] 
  utils::View(CBNPoor)
  
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
  
  #CalculatePer_calories
  CBNPoor[, Daily_Calories := Reduce(`+`, .SD), .SDcols=157:172][] 
  CBNPoor[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
  CBNPoor[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
  CBNPoor <- CBNPoor[Daily_Calories<100000] # arbitrary removal of outliers
  CBNPoor[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
  
  CBNPoor$Ghand_per_Calory<- CBNPoor$Ghandgram *4/CBNPoor$EqSizeCalory
  CBNPoor$Hoboobat_per_Calory<- CBNPoor$Hoboobatgram *3/CBNPoor$EqSizeCalory
  CBNPoor$Nan_per_Calory<- CBNPoor$Nangram *2.5/CBNPoor$EqSizeCalory
  CBNPoor$Berenj_per_Calory<- CBNPoor$Berenjgram *1.2/CBNPoor$EqSizeCalory
  CBNPoor$Roghan_per_Calory<- CBNPoor$Roghangram *8/CBNPoor$EqSizeCalory
  CBNPoor$Goosht_per_Calory<- CBNPoor$Gooshtgram *2.5/CBNPoor$EqSizeCalory
  CBNPoor$Morgh_per_Calory<- CBNPoor$Morghgram *2/CBNPoor$EqSizeCalory
  CBNPoor$Mahi_per_Calory<- CBNPoor$Mahigram *1/CBNPoor$EqSizeCalory
  CBNPoor$Shir_per_Calory<- CBNPoor$Shirgram *2.5/CBNPoor$EqSizeCalory
  CBNPoor$Mast_per_Calory<- CBNPoor$Mastgram *1.5/CBNPoor$EqSizeCalory
  CBNPoor$Panir_per_Calory<- CBNPoor$Mastgram *2.5/CBNPoor$EqSizeCalory
  CBNPoor$Tokhmemorgh_per_Calory<- CBNPoor$Tokhmemorghgram *1.4/CBNPoor$EqSizeCalory
  CBNPoor$Mive_per_Calory<- CBNPoor$Mivegram *0.5/CBNPoor$EqSizeCalory
  CBNPoor$Sabzi_per_Calory<- CBNPoor$Sabzigram *0.5/CBNPoor$EqSizeCalory
  CBNPoor$Makarooni_per_Calory<- CBNPoor$Makaroonigram *3.6/CBNPoor$EqSizeCalory
  CBNPoor$Sibzamini_per_Calory<- CBNPoor$Sibzaminigram *0.9/CBNPoor$EqSizeCalory
  utils::View(CBNPoor)

  #CalculatePer_calories in clusters
  CBNPoor[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(193:208)][] 
  utils::View(CBNPoor)
  
  CBNPoor[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(210:225)][] 

  utils::View(CBNPoor)
  
  # Food grams from Calories2
  CBNPoor$Ghandgram2  <- CBNPoor$Daily2_Ghand /4
  CBNPoor$Hoboobatgram2 <- CBNPoor$Daily2_Hoboobat /3
  CBNPoor$Nangram2 <- CBNPoor$Daily2_Nan  /2.5
  CBNPoor$Berenjgram2 <- CBNPoor$Daily2_Berenj  /1.2
  CBNPoor$Roghangram2 <- CBNPoor$Daily2_Roghan /8
  CBNPoor$Gooshtgram2 <- CBNPoor$Daily2_Goosht  /2.5
  CBNPoor$Morghgram2 <- CBNPoor$Daily2_Morgh  /2
  CBNPoor$Mahigram2 <- CBNPoor$Daily2_Mahi  /1
  CBNPoor$Shirgram2 <- CBNPoor$Daily2_Shir  /2.5
  CBNPoor$Mastgram2 <- CBNPoor$Daily2_Mast  /1.5
  CBNPoor$Panirgram2 <- CBNPoor$Daily2_Panir  /2.5
  CBNPoor$Tokhmemorghgram2 <- CBNPoor$Daily2_Tokhmemorgh /1.4
  CBNPoor$Mivegram2 <- CBNPoor$Daily2_Mive  /0.5
  CBNPoor$Sabzigram2 <- CBNPoor$Daily2_Sabzi  /0.5
  CBNPoor$Makaroonigram2 <- CBNPoor$Daily2_Makarooni  /3.6
  CBNPoor$Sibzaminigram2 <- CBNPoor$Daily2_Sibzamini /0.9
  utils::View(CBNPoor)
  
  # real prices
  CBNPoor<-CBNPoor[,GhandPrice:=ifelse(ProvinceCode==11,GhandPrice*257/279,GhandPrice)]
  CBNPoor<-CBNPoor[,HoboobatPrice:=ifelse(ProvinceCode==11,HoboobatPrice*257/236,HoboobatPrice)]
  CBNPoor<-CBNPoor[,RoghanPrice:=ifelse(ProvinceCode==11,RoghanPrice*256/238,RoghanPrice)]
  CBNPoor<-CBNPoor[,BerenjPrice:=ifelse(ProvinceCode==11,BerenjPrice*291/277,BerenjPrice)]
  CBNPoor<-CBNPoor[,NanPrice:=ifelse(ProvinceCode==11,NanPrice*292/277,NanPrice)]
  CBNPoor<-CBNPoor[,GooshtPrice:=ifelse(ProvinceCode==11,GooshtPrice*245/231,GooshtPrice)]
  CBNPoor<-CBNPoor[,MorghPrice:=ifelse(ProvinceCode==11,MorghPrice*245/231,MorghPrice)]
  CBNPoor<-CBNPoor[,MahiPrice:=ifelse(ProvinceCode==11,MahiPrice*283/281,MahiPrice)]
  CBNPoor<-CBNPoor[,ShirPrice:=ifelse(ProvinceCode==11,ShirPrice*254/290,ShirPrice)]
  CBNPoor<-CBNPoor[,MastPrice:=ifelse(ProvinceCode==11,MastPrice*254/290,MastPrice)]
  CBNPoor<-CBNPoor[,PanirPrice:=ifelse(ProvinceCode==11,PanirPrice*254/290,PanirPrice)]
  CBNPoor<-CBNPoor[,TokhmemorghPrice:=ifelse(ProvinceCode==11,TokhmemorghPrice*254/290,TokhmemorghPrice)]
  CBNPoor<-CBNPoor[,MivePrice:=ifelse(ProvinceCode==11,MivePrice*231/279,MivePrice)]
  CBNPoor<-CBNPoor[,SabziPrice:=ifelse(ProvinceCode==11,SabziPrice*257/236,SabziPrice)]
  CBNPoor<-CBNPoor[,MakarooniPrice:=ifelse(ProvinceCode==11,MakarooniPrice*292/277,MakarooniPrice)]
  CBNPoor<-CBNPoor[,SibzaminiPrice:=ifelse(ProvinceCode==11,SibzaminiPrice*257/236,SibzaminiPrice)]
  
  # New bundle
  CBNPoor[,GhandExp2:=GhandPrice*Ghandgram2*0.001]
  for (col in c("GhandExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,HoboobatExp2:=HoboobatPrice*Hoboobatgram2*0.001]
  for (col in c("HoboobatExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,NanExp2:=NanPrice*Nangram2*0.001]
  for (col in c("NanExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,BerenjExp2:=BerenjPrice*Berenjgram2*0.001]
  for (col in c("BerenjExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,RoghanExp2:=RoghanPrice*Roghangram2*0.001]
  for (col in c("RoghanExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,GooshtExp2:=GooshtPrice*Gooshtgram2*0.001]
  for (col in c("GooshtExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,MorghExp2:=MorghPrice*Morghgram2*0.001]
  for (col in c("MorghExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,MahiExp2:=MahiPrice*Mahigram2*0.001]
  for (col in c("MahiExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,ShirExp2:=ShirPrice*Shirgram2*0.001]
  for (col in c("ShirExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,MastExp2:=MastPrice*Mastgram2*0.001]
  for (col in c("MastExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,PanirExp2:=PanirPrice*Panirgram2*0.001]
  for (col in c("PanirExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,TokhmemorghExp2:=TokhmemorghPrice*Tokhmemorghgram2*0.001]
  for (col in c("TokhmemorghExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,MiveExp2:=MivePrice*Mivegram2*0.001]
  for (col in c("MiveExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,SabziExp2:=SabziPrice*Sabzigram2*0.001]
  for (col in c("SabziExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,MakarooniExp2:=MakarooniPrice*Makaroonigram2*0.001]
  for (col in c("MakarooniExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[,SibzaminiExp2:=SibzaminiPrice*Sibzaminigram2*0.001]
  for (col in c("SibzaminiExp2")) CBNPoor[is.na(get(col)), (col) := 0]
  CBNPoor[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(243:258)][] 
  utils::View(CBNPoor)
  a<-CBNPoor[,.(FoodExpenditure_Per_day,FoodExpenditure_Per_day2)]


  model <- lm(FoodExpenditure_Per ~ Total_Exp_Month_Per_nondurable , weights = Weight, data=CBNPoor)
  summary(model)
  Engel<-0.386
  Engel_Reverse<-1/Engel
  CBNPoor$Total_Exp_Month_Per2<-Engel_Reverse*CBNPoor$FoodExpenditure_Per
  Povertyline<-Engel_Reverse*weighted.mean(CBNPoor$FoodExpenditure_Per,CBNPoor$Weight,na.rm = TRUE)
  b<-CBNPoor[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month_Per2)]
  m<-CBNPoor[,.(HHID,Total_Exp_Month_Per2, cluster)]
  CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
  utils::View(CBN)
  for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
  c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month_Per2,Poor,Decile,Weight)]

    #Sort Food Expenditure data
  CBN<- CBN[order(Total_Exp_Month_Per2)]
  c<-  c[order(Total_Exp_Month_Per2)]
  
  #Indicate new poors
  #Calculate cumulative weights
  sum(CBN$Weight)
  CBN$cumweight2 <- cumsum(CBN$Weight)
  c$cumweight2 <- cumsum(CBN$Weight)
  tx <- max(CBN$cumweight2)
  
  #Calculate deciles by weights
  CBN[,Decile2:=cut(cumweight2,breaks = seq(0,tx,tx/10),labels = 1:10)]
  c[,Decile2:=cut(cumweight2,breaks = seq(0,tx,tx/10),labels = 1:10)]

  #CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline ,1,0)]
  #c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline ,1,0)]
  CBN[,Poor2:=ifelse(Decile2 %in% 1:2,1,0)]
  c[,Poor2:=ifelse(Decile2 %in% 1:2,1,0)]
  c[,sum(Poor),Poor2==1]
  c[,sum(Poor2),Poor==1]
  c[,sum(Poor2),Poor==1 & Poor2==1]
  CBNPoor2<-CBN[Poor2==1]
  c<-CBN[Poor2==1]
  
  
  #weighted consumption in each cluster
  CBNPoor2<-CBNPoor2[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
  CBNPoor2<-CBNPoor2[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]
  
  #weighted prices in each cluster
  CBNPoor2<-CBNPoor2[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
  CBNPoor2<-CBNPoor2[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2<-CBNPoor2[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]
  
  #weighted food expenditures in each cluster
  CBNPoor2[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
  CBNPoor2[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=140:155][] 
  utils::View(CBNPoor2)
  
  # Food Calories
  CBNPoor2$Ghand_Calory<- CBNPoor2$Ghandgram *4
  CBNPoor2$Hoboobat_Calory<- CBNPoor2$Hoboobatgram *3
  CBNPoor2$Nan_Calory<- CBNPoor2$Nangram *2.5
  CBNPoor2$Berenj_Calory<- CBNPoor2$Berenjgram *1.2
  CBNPoor2$Roghan_Calory<- CBNPoor2$Roghangram *8
  CBNPoor2$Goosht_Calory<- CBNPoor2$Gooshtgram *2.5
  CBNPoor2$Morgh_Calory<- CBNPoor2$Morghgram *2
  CBNPoor2$Mahi_Calory<- CBNPoor2$Mahigram *1
  CBNPoor2$Shir_Calory<- CBNPoor2$Shirgram *2.5
  CBNPoor2$Mast_Calory<- CBNPoor2$Mastgram *1.5
  CBNPoor2$Panir_Calory<- CBNPoor2$Mastgram *2.5
  CBNPoor2$Tokhmemorgh_Calory<- CBNPoor2$Tokhmemorghgram *1.4
  CBNPoor2$Mive_Calory<- CBNPoor2$Mivegram *0.5
  CBNPoor2$Sabzi_Calory<- CBNPoor2$Sabzigram *0.5
  CBNPoor2$Makarooni_Calory<- CBNPoor2$Makaroonigram *3.6
  CBNPoor2$Sibzamini_Calory<- CBNPoor2$Sibzaminigram *0.9
  utils::View(CBNPoor2)
  
  #CalculatePer_calories
  CBNPoor2[, Daily_Calories := Reduce(`+`, .SD), .SDcols=161:176][] 
  CBNPoor2[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
  CBNPoor2[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
  CBNPoor2 <- CBNPoor2[Daily_Calories<100000] # arbitrary removal of outliers
  CBNPoor2[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
  
  CBNPoor2$Ghand_per_Calory<- CBNPoor2$Ghandgram *4/CBNPoor2$EqSizeCalory
  CBNPoor2$Hoboobat_per_Calory<- CBNPoor2$Hoboobatgram *3/CBNPoor2$EqSizeCalory
  CBNPoor2$Nan_per_Calory<- CBNPoor2$Nangram *2.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Berenj_per_Calory<- CBNPoor2$Berenjgram *1.2/CBNPoor2$EqSizeCalory
  CBNPoor2$Roghan_per_Calory<- CBNPoor2$Roghangram *8/CBNPoor2$EqSizeCalory
  CBNPoor2$Goosht_per_Calory<- CBNPoor2$Gooshtgram *2.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Morgh_per_Calory<- CBNPoor2$Morghgram *2/CBNPoor2$EqSizeCalory
  CBNPoor2$Mahi_per_Calory<- CBNPoor2$Mahigram *1/CBNPoor2$EqSizeCalory
  CBNPoor2$Shir_per_Calory<- CBNPoor2$Shirgram *2.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Mast_per_Calory<- CBNPoor2$Mastgram *1.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Panir_per_Calory<- CBNPoor2$Mastgram *2.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Tokhmemorgh_per_Calory<- CBNPoor2$Tokhmemorghgram *1.4/CBNPoor2$EqSizeCalory
  CBNPoor2$Mive_per_Calory<- CBNPoor2$Mivegram *0.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Sabzi_per_Calory<- CBNPoor2$Sabzigram *0.5/CBNPoor2$EqSizeCalory
  CBNPoor2$Makarooni_per_Calory<- CBNPoor2$Makaroonigram *3.6/CBNPoor2$EqSizeCalory
  CBNPoor2$Sibzamini_per_Calory<- CBNPoor2$Sibzaminigram *0.9/CBNPoor2$EqSizeCalory
  utils::View(CBNPoor2)
  
  #CalculatePer_calories in clusters
  CBNPoor2[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
  CBNPoor2[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(197:212)][] 
  utils::View(CBNPoor2)
  
  CBNPoor2[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
  CBNPoor2[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(214:229)][] 
  for (col in c("Daily2_Ghand","Daily2_Hoboobat","Daily2_Nan","Daily2_Berenj","Daily2_Roghan","Daily2_Goosht","Daily2_Morgh","Daily2_Mahi","Daily2_Shir","Daily2_Mast","Daily2_Panir","Daily2_Tokhmemorgh","Daily2_Mive","Daily2_Sabzi","Daily2_Makarooni","Daily2_Sibzamini")) CBNPoor2[is.na(get(col)), (col) := 0]
  utils::View(CBNPoor2)
  
  # Food grams from Calories2
  CBNPoor2$Ghandgram2  <- CBNPoor2$Daily2_Ghand /4
  CBNPoor2$Hoboobatgram2 <- CBNPoor2$Daily2_Hoboobat /3
  CBNPoor2$Nangram2 <- CBNPoor2$Daily2_Nan  /2.5
  CBNPoor2$Berenjgram2 <- CBNPoor2$Daily2_Berenj  /1.2
  CBNPoor2$Roghangram2 <- CBNPoor2$Daily2_Roghan /8
  CBNPoor2$Gooshtgram2 <- CBNPoor2$Daily2_Goosht  /2.5
  CBNPoor2$Morghgram2 <- CBNPoor2$Daily2_Morgh  /2
  CBNPoor2$Mahigram2 <- CBNPoor2$Daily2_Mahi  /1
  CBNPoor2$Shirgram2 <- CBNPoor2$Daily2_Shir  /2.5
  CBNPoor2$Mastgram2 <- CBNPoor2$Daily2_Mast  /1.5
  CBNPoor2$Panirgram2 <- CBNPoor2$Daily2_Panir  /2.5
  CBNPoor2$Tokhmemorghgram2 <- CBNPoor2$Daily2_Tokhmemorgh /1.4
  CBNPoor2$Mivegram2 <- CBNPoor2$Daily2_Mive  /0.5
  CBNPoor2$Sabzigram2 <- CBNPoor2$Daily2_Sabzi  /0.5
  CBNPoor2$Makaroonigram2 <- CBNPoor2$Daily2_Makarooni  /3.6
  CBNPoor2$Sibzaminigram2 <- CBNPoor2$Daily2_Sibzamini /0.9
  utils::View(CBNPoor2)
  
  # real prices
  # CBNPoor2[,GhandIndex:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,HoboobatIndex:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,RoghanIndex:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  #CBNPoor2[,RoghanIndex:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,NanIndex:=weighted.mean(NanPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,GooshtIndex:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,MorghIndex:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,MahiIndex:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,ShirIndex:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,MastIndex:=weighted.mean(MastPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,PanirIndex:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,TokhmemorghIndex:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,MiveIndex:=weighted.mean(MivePrice,Weight,na.rm = TRUE),ProvinceCode==23]
  #CBNPoor2[,SabziIndex:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  # CBNPoor2[,MakarooniIndex:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  #CBNPoor2[,SibzaminiIndex:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),ProvinceCode==23]
  
  # New bundle
  CBNPoor2[,GhandExp2:=GhandPrice*Ghandgram2*0.001]
  for (col in c("GhandExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,HoboobatExp2:=HoboobatPrice*Hoboobatgram2*0.001]
  for (col in c("HoboobatExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,NanExp2:=NanPrice*Nangram2*0.001]
  for (col in c("NanExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,BerenjExp2:=BerenjPrice*Berenjgram2*0.001]
  for (col in c("BerenjExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,RoghanExp2:=RoghanPrice*Roghangram2*0.001]
  for (col in c("RoghanExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,GooshtExp2:=GooshtPrice*Gooshtgram2*0.001]
  for (col in c("GooshtExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,MorghExp2:=MorghPrice*Morghgram2*0.001]
  for (col in c("MorghExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,MahiExp2:=MahiPrice*Mahigram2*0.001]
  for (col in c("MahiExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,ShirExp2:=ShirPrice*Shirgram2*0.001]
  for (col in c("ShirExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,MastExp2:=MastPrice*Mastgram2*0.001]
  for (col in c("MastExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,PanirExp2:=PanirPrice*Panirgram2*0.001]
  for (col in c("PanirExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,TokhmemorghExp2:=TokhmemorghPrice*Tokhmemorghgram2*0.001]
  for (col in c("TokhmemorghExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,MiveExp2:=MivePrice*Mivegram2*0.001]
  for (col in c("MiveExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,SabziExp2:=SabziPrice*Sabzigram2*0.001]
  for (col in c("SabziExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,MakarooniExp2:=MakarooniPrice*Makaroonigram2*0.001]
  for (col in c("MakarooniExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[,SibzaminiExp2:=SibzaminiPrice*Sibzaminigram2*0.001]
  for (col in c("SibzaminiExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
  CBNPoor2[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(247:262)][] 
  utils::View(CBNPoor2)
  a<-CBNPoor2[,.(FoodExpenditure_Per_day,FoodExpenditure_Per_day2)]
  
  model <- lm(FoodExpenditure_Per ~ Total_Exp_Month_Per_nondurable , weights = Weight, data=CBNPoor2)
  summary(model)
  Engel<-0.303
  Engel_Reverse<-1/Engel
  CBNPoor2$Total_Exp_Month_Per3<-Engel_Reverse*CBNPoor2$FoodExpenditure_Per
  Povertyline<-Engel_Reverse*mean(CBNPoor2$FoodExpenditure_Per)
  b<-CBNPoor2[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month_Per3)]
  m<-CBNPoor2[,.(HHID,Total_Exp_Month_Per3)]
  CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
  utils::View(CBN)
  for (col in c("Total_Exp_Month_Per3")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per]
  c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month_Per3,Poor,Poor2,Decile,Decile2,Weight)]
  
  #Sort Food Expenditure data
  CBN<- CBN[order(Total_Exp_Month_Per3)]
  c<-  c[order(Total_Exp_Month_Per3)]
  
  #Indicate new poors
  #Calculate cumulative weights
  sum(CBN$Weight)
  CBN$cumweight3 <- cumsum(CBN$Weight)
  c$cumweight3 <- cumsum(CBN$Weight)
  tx <- max(CBN$cumweight3)
  
  #Calculate deciles by weights
  CBN[,Decile3:=cut(cumweight3,breaks = seq(0,tx,tx/10),labels = 1:10)]
  c[,Decile3:=cut(cumweight3,breaks = seq(0,tx,tx/10),labels = 1:10)]
  
  #CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline ,1,0)]
  #c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline ,1,0)]
  CBN[,Poor3:=ifelse(Decile3 %in% 1:2,1,0)]
  c[,Poor3:=ifelse(Decile3 %in% 1:2,1,0)]
  c[,sum(Poor2),Poor3==1]
  c[,sum(Poor3),Poor2==1]
  c[,sum(Poor2),Poor==1 & Poor2==1]
  CBNPoor3<-CBN[Poor3==1]
  c<-CBN[Poor3==1]
  e<-CBN[,weighted.mean(Poor3,Weight),by=ProvinceCode]
  e<-  e[order(ProvinceCode)]
  
  
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

