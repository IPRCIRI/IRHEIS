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

 # Food Calories
  Food$Ghand_Calory<- Food$Ghandgram *4
  Food$Hoboobat_Calory<- Food$Hoboobatgram *3
  Food$Nan_Calory<- Food$Nangram *2.5
  Food$Berenj_Calory<- Food$Berenjgram *1.2
  Food$Roghan_Calory<- Food$Roghangram *8
  Food$Goosht_Calory<- Food$Gooshtgram *2.5
  Food$Morgh_Calory<- Food$Morghgram *2
  Food$Mahi_Calory<- Food$Mahigram *1
  Food$Shir_Calory<- Food$Shirgram *2.5
  Food$Mast_Calory<- Food$Mastgram *1.5
  Food$Panir_Calory<- Food$Mastgram *2.5
  Food$Tokhmemorgh_Calory<- Food$Tokhmemorghgram *1.4
  Food$Mive_Calory<- Food$Mivegram *0.5
  Food$Sabzi_Calory<- Food$Sabzigram *0.5
  Food$Makarooni_Calory<- Food$Makaroonigram *3.6
  Food$Sibzamini_Calory<- Food$Sibzaminigram *0.9
  #Food$Shirini_Calory<- Food$ShiriniGram*3.5
  #Food$Biscuit_Calory<- Food$BiscuitGram*3
  #Food$Khoshkbar_Calory<- Food$KhoshkbarGram*5
  
  
  Food[,Year:=NULL]
  Food[,Quarter:=NULL]
 # Food[,Month:=NULL]
 # Food[,ProvinceCode:=NULL]
  Food[,Dimension:=NULL]
  Food[,FoodExpenditure:=NULL]
  

  Food[, Daily_Calories := Reduce(`+`, .SD), .SDcols=38:53][] 
  
  Food <- Food[Daily_Calories<100000] # arbitrary removal of outliers
  # MyFoodRural<-MyFood[(MyFood$Region=="Rural"),]
  # MyFoodUrban<-MyFood[(MyFood$Region=="Urban"),]
  

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
    #plot(PRICE1, PRICE2,col=cl$cluster)
    #points(cl$centers, pch=20)
    CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode","Region"),all.x = TRUE)
    C2<-CBNPoor[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]
 
  
    
  
  
  CBN[,weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Roghangram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Nangram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Morghgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Mahigram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Shirgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Mastgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Panirgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Mivegram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=Poor]
  CBN[,weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=Poor]
  
  CBN[,weighted.mean(GhandPrice,Weight,na.rm = TRUE),GhandPrice!=0]
  CBN[,weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),HoboobatPrice!=0]
  CBN[,weighted.mean(RoghanPrice,Weight,na.rm = TRUE),RoghanPrice!=0]
  CBN[,weighted.mean(BerenjPrice,Weight,na.rm = TRUE),BerenjPrice!=0]
  CBN[,weighted.mean(NanPrice,Weight,na.rm = TRUE),NanPrice!=0]
  CBN[,weighted.mean(GooshtPrice,Weight,na.rm = TRUE),GooshtPrice!=0]
  CBN[,weighted.mean(MorghPrice,Weight,na.rm = TRUE),MorghPrice!=0]
  CBN[,weighted.mean(MahiPrice,Weight,na.rm = TRUE),MahiPrice!=0]
  CBN[,weighted.mean(ShirPrice,Weight,na.rm = TRUE),ShirPrice!=0]
  CBN[,weighted.mean(MastPrice,Weight,na.rm = TRUE),MastPrice!=0]
  CBN[,weighted.mean(PanirPrice,Weight,na.rm = TRUE),PanirPrice!=0]
  CBN[,weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),TokhmemorghPrice!=0]
  CBN[,weighted.mean(MivePrice,Weight,na.rm = TRUE),MivePrice!=0]
  CBN[,weighted.mean(SabziPrice,Weight,na.rm = TRUE),SabziPrice!=0]
  CBN[,weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),MakarooniPrice!=0]
  CBN[,weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),SibzaminiPrice!=0]

  CBN[,GhandPoor:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,HoboobatPoor:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,RoghanPoor:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,BerenjPoor:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,NanPoor:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,GooshtPoor:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,MorghPoor:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,MahiPoor:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,ShirPoor:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,MastPoor:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,PanirPoor:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,TokhmemorghPoor:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,MivePoor:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,SabziPoor:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,MakarooniPoor:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  CBN[,SibzaminiPoor:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=Poor]
  
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

