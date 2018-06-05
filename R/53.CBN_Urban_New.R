#CBN Method-Urban.R
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
library(stringr)
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
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Resturants.rda"))
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
CBN<-merge(CBN,BehdashtData,by =c("HHID"),all=TRUE)
for (col in c("Behdasht_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,TransportationData,by =c("HHID"),all=TRUE)
for (col in c("Transportation_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,OtherData,by =c("HHID"),all=TRUE)
for (col in c("Other_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,HouseData,by =c("HHID"),all=TRUE)
for (col in c("ServiceExp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,InvestmentData,by =c("HHID"),all=TRUE)
for (col in c("Investment_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,MedicalData,by =c("HHID"),all=TRUE)
for (col in c("Medical_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,DurableData,by =c("HHID"),all=TRUE)
for (col in c("Durable_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-merge(CBN,ResturantData,by =c("HHID"),all=TRUE)
for (col in c("Resturant_Exp")) CBN[is.na(get(col)), (col) := 0]
CBN<-CBN[Size!=0]
CBN<-CBN[Region=="Urban"]
CBN<-CBN[FoodExpenditure!=0]


#Calculate Per_Total Expenditures Monthly
CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(65:77,82:83)][] 
CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=65:77][] 

CBN$Total_Exp_Month_Per<-CBN$Total_Exp_Month/CBN$EqSizeRevOECD
CBN$Total_Exp_Month_Per_nondurable<-CBN$Total_Exp_Month_nondurable/CBN$EqSizeRevOECD

#Calculate Per_Food Expenditures Monthly
CBN[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBN$FoodExpenditure_Per<-CBN$FoodExpenditure/CBN$EqSizeCalory

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
CBN[,EqSizeCalory:=NULL]

load(file="PriceIndex95.rda")
CBN<-merge(CBN,PriceIndex95,by=c("ProvinceCode"),all.x = TRUE)
CBN[,ostan:=NULL]
load(file="PriceIndex.rda")
CBN<-merge(CBN,PriceIndex,by=c("ProvinceCode"),all.x = TRUE)

#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per_nondurable)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]


CBN$Ghand_W<-CBN$Ghandgram_Per_day*CBN$GhandPrice*0.001*30
CBN$Hoboobat_W<-CBN$Hoboobatgram_Per_day*CBN$HoboobatPrice*0.001*30
CBN$Roghan_W<-CBN$Roghangram_Per_day*CBN$RoghanPrice*0.001*30
CBN$Berenj_W<-CBN$Berenjgram_Per_day*CBN$BerenjPrice*0.001*30
CBN$Nan_W<-CBN$Nangram_Per_day*CBN$NanPrice*0.001*30
CBN$Goosht_W<-CBN$Gooshtgram_Per_day*CBN$GooshtPrice*0.001*30
CBN$Morgh_W<-CBN$Morghgram_Per_day*CBN$MorghPrice*0.001*30
CBN$Mahi_W<-CBN$Mahigram_Per_day*CBN$MahiPrice*0.001*30
CBN$Shir_W<-CBN$Shirgram_Per_day*CBN$ShirPrice*0.001*30
CBN$Mast_W<-CBN$Mastgram_Per_day*CBN$MastPrice*0.001*30
CBN$Panir_W<-CBN$Panirgram_Per_day*CBN$PanirPrice*0.001*30
CBN$Tokhmemorgh_W<-CBN$Tokhmemorghgram_Per_day*CBN$TokhmemorghPrice*0.001*30
CBN$Mive_W<-CBN$Mivegram_Per_day*CBN$MivePrice*0.001*30
CBN$Sabzi_W<-CBN$Sabzigram_Per_day*CBN$SabziPrice*0.001*30
CBN$Makarooni_W<-CBN$Makaroonigram_Per_day*CBN$MakarooniPrice*0.001*30
CBN$Sibzamini_W<-CBN$Sibzaminigram_Per_day*CBN$SibzaminiPrice*0.001*30
CBN$Home_W<-CBN$ServiceExp/CBN$EqSizeRevOECD

#Seperate big cities
CBN[,sum(Weight*Size),by=ProvinceCode][order(V1)]
CBN[,HHIDs:=as.character(HHID)]
CBN[,ShahrestanCode:=as.integer(str_sub(HHIDs,2,5))]
CBN[,sum(Weight*Size),by=ShahrestanCode][order(V1)][330:387]
CBNTehran<-CBN[ProvinceCode==23]
CBNTehran[,sum(Weight*Size),by=ShahrestanCode]
CBNTabriz<-CBN[ProvinceCode==3]
CBNTabriz[,sum(Weight*Size),by=ShahrestanCode]
CBNAhvaz<-CBN[ProvinceCode==6]
CBNAhvaz[,sum(Weight*Size),by=ShahrestanCode]
CBNShiraz<-CBN[ProvinceCode==7]
CBNShiraz[,sum(Weight*Size),by=ShahrestanCode]
CBNMashhad<-CBN[ProvinceCode==9]
CBNMashhad[,sum(Weight*Size),by=ShahrestanCode]
CBNEsfahan<-CBN[ProvinceCode==10]
CBNEsfahan[,sum(Weight*Size),by=ShahrestanCode]
CBNKaraj<-CBN[ProvinceCode==30]
CBNKaraj[,sum(Weight*Size),by=ShahrestanCode]
CBNKermanshah<-CBN[ProvinceCode==5]
CBNKermanshah[,sum(Weight*Size),by=ShahrestanCode]


CBN<-CBN[ShahrestanCode==2301,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==303,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==603,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==707,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==916,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==1002,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==3001,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==2301,ProvinceCode:=as.numeric(ShahrestanCode)]
CBN<-CBN[ShahrestanCode==502,ProvinceCode:=as.numeric(ShahrestanCode)]

# Food Calories
CBN$Ghand_Calory<- CBN$Ghandgram *4
CBN$Hoboobat_Calory<- CBN$Hoboobatgram *3
CBN$Nan_Calory<- CBN$Nangram *2.5
CBN$Berenj_Calory<- CBN$Berenjgram *1.2
CBN$Roghan_Calory<- CBN$Roghangram *8
CBN$Goosht_Calory<- CBN$Gooshtgram *2.5
CBN$Morgh_Calory<- CBN$Morghgram *2
CBN$Mahi_Calory<- CBN$Mahigram *1
CBN$Shir_Calory<- CBN$Shirgram *2.5
CBN$Mast_Calory<- CBN$Mastgram *1.5
CBN$Panir_Calory<- CBN$Mastgram *2.5
CBN$Tokhmemorgh_Calory<- CBN$Tokhmemorghgram *1.4
CBN$Mive_Calory<- CBN$Mivegram *0.5
CBN$Sabzi_Calory<- CBN$Sabzigram *0.5
CBN$Makarooni_Calory<- CBN$Makaroonigram *3.6
CBN$Sibzamini_Calory<- CBN$Sibzaminigram *0.9
#utils::View(CBN)

#CalculatePer_calories
CBN[, Daily_Exp_Calories := Reduce(`+`, .SD), .SDcols=147:162][] 
CBN[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBN[,Per_Daily_Exp_Calories:=Daily_Exp_Calories/EqSizeCalory]
CBN <- CBN[Per_Daily_Exp_Calories<100000] # arbitrary removal of outliers
#CBN[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
#CBN[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]
#CBN[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
#CBN[,weighted.mean(Size,Weight,na.rm = TRUE),by=cluster]
#CBN[,sum(Weight*Size),by=cluster]
#CBN[,sum(Weight),by=cluster]
#CBN[,sum(Poor),by=cluster]


#Calculate_Per_calories
CBN$Ghand_per_Calory<- CBN$Ghandgram *4/CBN$EqSizeCalory
CBN$Hoboobat_per_Calory<- CBN$Hoboobatgram *3/CBN$EqSizeCalory
CBN$Nan_per_Calory<- CBN$Nangram *2.5/CBN$EqSizeCalory
CBN$Berenj_per_Calory<- CBN$Berenjgram *1.2/CBN$EqSizeCalory
CBN$Roghan_per_Calory<- CBN$Roghangram *8/CBN$EqSizeCalory
CBN$Goosht_per_Calory<- CBN$Gooshtgram *2.5/CBN$EqSizeCalory
CBN$Morgh_per_Calory<- CBN$Morghgram *2/CBN$EqSizeCalory
CBN$Mahi_per_Calory<- CBN$Mahigram *1/CBN$EqSizeCalory
CBN$Shir_per_Calory<- CBN$Shirgram *2.5/CBN$EqSizeCalory
CBN$Mast_per_Calory<- CBN$Mastgram *1.5/CBN$EqSizeCalory
CBN$Panir_per_Calory<- CBN$Mastgram *2.5/CBN$EqSizeCalory
CBN$Tokhmemorgh_per_Calory<- CBN$Tokhmemorghgram *1.4/CBN$EqSizeCalory
CBN$Mive_per_Calory<- CBN$Mivegram *0.5/CBN$EqSizeCalory
CBN$Sabzi_per_Calory<- CBN$Sabzigram *0.5/CBN$EqSizeCalory
CBN$Makarooni_per_Calory<- CBN$Makaroonigram *3.6/CBN$EqSizeCalory
CBN$Sibzamini_per_Calory<- CBN$Sibzaminigram *0.9/CBN$EqSizeCalory


#Assume that deciles 1 and 2 are poor
CBN[,Poor:=ifelse(Decile %in% 1:2,1,0)]
CBNPoor<-CBN[Poor==1]
PriceWeights<-CBN[,.(HHID,Ghand_W,Hoboobat_W,Roghan_W,Berenj_W,Nan_W,Goosht_W,Morgh_W,Mahi_W,Shir_W,Mast_W,Panir_W,Tokhmemorgh_W,Mive_W,Sabzi_W,Makarooni_W,Sibzamini_W,Home_W,ProvinceCode,Weight)]
dt3 <- PriceWeights[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
dt3<- dt3[order(ProvinceCode)]
dt3 <- dt3[,.(Ghand_W,Hoboobat_W,Roghan_W,Berenj_W,Nan_W,Goosht_W,Morgh_W,Mahi_W,Shir_W,Mast_W,Panir_W,Tokhmemorgh_W,Mive_W,Sabzi_W,Makarooni_W,Sibzamini_W,Home_W)]



#K-means algorithm for clustering by prices
test<-CBNPoor[,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice,MetrPrice,ProvinceCode,Weight)]
#test<-CBNPoor[,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice,MetrPrice,Ghand_W,Hoboobat_W,Roghan_W,Berenj_W,Nan_W,Goosht_W,Morgh_W,Mahi_W,Shir_W,Mast_W,Panir_W,Tokhmemorgh_W,Mive_W,Sabzi_W,Makarooni_W,Sibzamini_W,Home_W,Region,ProvinceCode,Weight)]
dt2 <- test[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
dt2<- dt2[order(ProvinceCode)]
for (col in c("MahiPrice")) dt2[is.nan(get(col)), (col) := 200000]
dt <- dt2 [,.(GhandPrice,HoboobatPrice,RoghanPrice,BerenjPrice,NanPrice,GooshtPrice,MorghPrice,MahiPrice,ShirPrice,MastPrice,PanirPrice,TokhmemorghPrice,MivePrice,SabziPrice,MakarooniPrice,SibzaminiPrice,MetrPrice)]


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
PRICE17 <- -1*PRICE[,17] 

# Deciding how many clusters
wss <- (nrow(dt)-1)*sum(apply(dt,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(dt, centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Weighted clustering
#fgkm(dt, 5, dt3, 3, 1,seed=-1)
#cclust::cclust(dt,5, dist = "euclidean", weights = dt3, method = "kmeans")
#ewkm(dt, 5, lambda=1, maxiter=1000, delta=0.00001, maxrestart=10)
## Creating state sequence object
#mvad.seq <- seqdef(mvad[aggMvad$aggIndex, 17:86], weights=aggMvad$aggWeights)
## Computing Hamming distance between sequence
#diss <- seqdist(mvad.seq, method="HAM")
#clust5 <- wcKMedoids(PRICE, k=5, weights=PRICE)

#X12 <- cbind(PRICE1, PRICE2)
dt3.m <- dt3[,lapply(.SD, mean)]			# Weights for each vector
dtW <- dt * sqrt(dt3.m[rep(1,nrow(dt))])	# Weighted observations
kmeans(dtW,4)						# Simple K-means

cl <- kmeans(dtW,4)
cl$cluster
dt2 <- dt2[,cluster:=data.table(cl$cluster)]
dt2<-dt2[,.(ProvinceCode,cluster)]
load(file="dt4Urban.rda")
#plot(PRICE1, PRICE2,col=cl$cluster)
#points(cl$centers, pch=20)
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(Weight*Size),by=cluster]
CBNPoor[,sum(Weight),by=cluster]
CBNPoor[,sum(Poor),by=cluster]
C2<-CBNPoor[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]
######################################################################    
####Iteration 1#####


#Calculate Per_calories in clusters
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(184:199)][] 
#utils::View(CBNPoor)



#Calculate Per_calories in clusters(=2100)
CBNPoor[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(201:216)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]

# real prices indexes
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257
TotalIndexTehran<-235
KhorakIndexTehran<-260


CBNPoor<-CBNPoor[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor<-CBNPoor[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor<-CBNPoor[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor<-CBNPoor[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor<-CBNPoor[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor16[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor16[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor16[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_1<-Engel_Reverse1*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_1<-Engel_Reverse2*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_1<-Engel_Reverse3*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_1<-Engel_Reverse4*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)


ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Exp_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Exp_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Exp_Month_Per2<3500000])
ee<- ee[order(Total_Exp_Month_Per2)]

#utils::View(CBN)
#b<-CBNPoor[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor[,.(HHID,Total_Exp_Month_Per2)]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]

#########Iteration 2###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline1_1 & cluster==1,1,0)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline1_1 & cluster==1 ,1,0)]
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline2_1 & cluster==2,1,Poor2)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline2_1 & cluster==2 ,1,Poor2)]
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline3_1 & cluster==3,1,Poor2)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline3_1 & cluster==3 ,1,Poor2)]
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline4_1 & cluster==4,1,Poor2)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline4_1 & cluster==4 ,1,Poor2)]
CBN[,weighted.mean(Poor2,Weight),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2<-CBN[Poor2==1]


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
CBNPoor2[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(186:201)][] 
#utils::View(CBNPoor2)

#Calculate Per_calories in clusters(=2100)
CBNPoor2[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(203:218)][] 


CBNPoor2[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor2[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor2[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor2[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[Per_Daily_Exp_Calories!=0]
CBNPoor2[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor2[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor2[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor2[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor2[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor2[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor2[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257

CBNPoor2<-CBNPoor2[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor2<-CBNPoor2[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor2[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor2[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor2<-CBNPoor2[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor2[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor216[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor216[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor216[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor2[cluster==1]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline1_1]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline1_1]
Engel1<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_2<-Engel_Reverse1*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 2
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==2]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline2_1]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline2_1]
Engel2<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_2<-Engel_Reverse2*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 3
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==3]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline3_1]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline3_1]
Engel3<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_2<-Engel_Reverse3*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 4
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==4]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline4_1]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline4_1]
Engel4<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_2<-Engel_Reverse4*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#w<-CBNPoor2[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor2[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor2[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 3###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline1_2 & cluster==1,1,0)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline1_2 & cluster==1 ,1,0)]
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline2_2 & cluster==2,1,Poor3)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline2_2 & cluster==2 ,1,Poor3)]
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline3_2 & cluster==3,1,Poor3)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline3_2 & cluster==3 ,1,Poor3)]
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline4_2 & cluster==4,1,Poor3)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline4_2 & cluster==4 ,1,Poor3)]
c[,weighted.mean(Poor3,Weight),by=cluster]
CBNPoor3<-CBN[Poor3==1]


#CalculatePer_calories in clusters
CBNPoor3[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(187:202)][] 
#utils::View(CBNPoor3)

#Calculate Per_calories in clusters(=2100)
CBNPoor3[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(204:219)][] 


CBNPoor3[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor3[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor3[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor3[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[Per_Daily_Exp_Calories!=0]
CBNPoor3[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor3[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor3[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor3[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor3[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor3[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor3[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257

CBNPoor3<-CBNPoor3[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor3<-CBNPoor3[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor3[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor3[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor3<-CBNPoor3[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor3[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor316[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor316[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor316[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor3[cluster==1]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline1_2]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline1_2]
Engel1<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_3<-Engel_Reverse1*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 2
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==2]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline2_2]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline2_2]
Engel2<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_3<-Engel_Reverse2*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 3
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==3]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline3_2]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline3_2]
Engel3<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_3<-Engel_Reverse3*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 4
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==4]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline4_2]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline4_2]
Engel4<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_3<-Engel_Reverse4*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)


#w<-CBNPoor3[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor3[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor3[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 4###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline1_3 & cluster==1,1,0)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline1_3 & cluster==1 ,1,0)]
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline2_3 & cluster==2,1,Poor4)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline2_3 & cluster==2 ,1,Poor4)]
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline3_3 & cluster==3,1,Poor4)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline3_3 & cluster==3 ,1,Poor4)]
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline4_3 & cluster==4,1,Poor4)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline4_3 & cluster==4 ,1,Poor4)]
c[,weighted.mean(Poor4,Weight),by=cluster]
CBNPoor4<-CBN[Poor4==1]



#CalculatePer_calories in clusters
CBNPoor4[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(188:203)][] 
#utils::View(CBNPoor4)

#Calculate Per_calories in clusters(=2100)
CBNPoor4[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(205:220)][] 


CBNPoor4[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor4[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor4[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor4[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[Per_Daily_Exp_Calories!=0]
CBNPoor4[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor4[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor4[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor4[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor4[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor4[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor4[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257


CBNPoor4<-CBNPoor4[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor4<-CBNPoor4[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor4[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor4[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor4<-CBNPoor4[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor4[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor416[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor416[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor416[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor4[cluster==1]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline1_3]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline1_3]
Engel1<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_4<-Engel_Reverse1*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 2
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==2]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline2_3]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline2_3]
Engel2<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_4<-Engel_Reverse2*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 3
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==3]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline3_3]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline3_3]
Engel3<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_4<-Engel_Reverse3*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 4
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==4]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline4_3]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline4_3]
Engel4<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_4<-Engel_Reverse4*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)



#w<-CBNPoor4[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor4[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor4[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 5###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline1_4 & cluster==1,1,0)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline1_4 & cluster==1 ,1,0)]
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline2_4 & cluster==2,1,Poor5)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline2_4 & cluster==2 ,1,Poor5)]
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline3_4 & cluster==3,1,Poor5)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline3_4 & cluster==3 ,1,Poor5)]
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline4_4 & cluster==4,1,Poor5)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline4_4 & cluster==4 ,1,Poor5)]
c[,weighted.mean(Poor5,Weight),by=cluster]
CBNPoor5<-CBN[Poor5==1]


#CalculatePer_calories in clusters
CBNPoor5[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(189:204)][] 
#utils::View(CBNPoor5)

#Calculate Per_calories in clusters(=2100)
CBNPoor5[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(206:221)][] 


CBNPoor5[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor5[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor5[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor5[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[Per_Daily_Exp_Calories!=0]
CBNPoor5[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor5[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor5[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor5[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor5[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor5[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor5[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257


CBNPoor5<-CBNPoor5[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor5<-CBNPoor5[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor5[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor5[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor5<-CBNPoor5[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor5[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor516[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor516[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor516[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor5[cluster==1]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline1_4]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline1_4]
Engel1<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_5<-Engel_Reverse1*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 2
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==2]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline2_4]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline2_4]
Engel2<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_5<-Engel_Reverse2*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 3
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==3]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline3_4]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline3_4]
Engel3<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_5<-Engel_Reverse3*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 4
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==4]
CBNPoorCluster2<-CBNPoorCluster[Total_Exp_Month_Per2<1.2*Povertyline4_4]
CBNPoorCluster2<-CBNPoorCluster2[Total_Exp_Month_Per2>0.5*Povertyline4_4]
Engel4<-weighted.mean(CBNPoorCluster2$ratio1,CBNPoorCluster2$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_5<-Engel_Reverse4*weighted.mean(CBNPoorCluster2$Bundle_Value,CBNPoorCluster2$Weight,na.rm = TRUE)



#w<-CBNPoor5[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor5[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor5[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 6###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline1_5 & cluster==1,1,0)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline1_5 & cluster==1 ,1,0)]
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline2_5 & cluster==2,1,Poor6)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline2_5 & cluster==2 ,1,Poor6)]
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline3_5 & cluster==3,1,Poor6)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline3_5 & cluster==3 ,1,Poor6)]
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline4_5 & cluster==4,1,Poor6)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline4_5 & cluster==4 ,1,Poor6)]
c[,weighted.mean(Poor6,Weight),by=cluster]
CBNPoor6<-CBN[Poor6==1]

#CalculatePer_calories in clusters
CBNPoor6[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(190:205)][] 
#utils::View(CBNPoor6)

#Calculate Per_calories in clusters(=2100)
CBNPoor6[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(207:222)][] 


CBNPoor6[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor6[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor6[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor6[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[Per_Daily_Exp_Calories!=0]
CBNPoor6[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor6[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor6[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor6[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor6[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor6[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257


CBNPoor6<-CBNPoor6[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor6<-CBNPoor6[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor6[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor6[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor6<-CBNPoor6[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor6[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor616[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor616[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor616[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor6[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_6<-Engel_Reverse1*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_6<-Engel_Reverse2*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_6<-Engel_Reverse3*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_6<-Engel_Reverse4*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)



#w<-CBNPoor6[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor6[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor6[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 7###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline1_6 & cluster==1,1,0)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline1_6 & cluster==1 ,1,0)]
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline2_6 & cluster==2,1,Poor7)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline2_6 & cluster==2 ,1,Poor7)]
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline3_6 & cluster==3,1,Poor7)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline3_6 & cluster==3 ,1,Poor7)]
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline4_6 & cluster==4,1,Poor7)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline4_6 & cluster==4 ,1,Poor7)]
c[,weighted.mean(Poor7,Weight),by=cluster]
CBNPoor7<-CBN[Poor7==1]

#CalculatePer_calories in clusters
CBNPoor7[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
#utils::View(CBNPoor7)

#Calculate Per_calories in clusters(=2100)
CBNPoor7[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor7[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor7[,Calory_Price:=FoodExpenditure_Per_cluster/Daily_Calories_cluster2]
CBNPoor7[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor7[,Per_Calory_Resturant:=(Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[Per_Daily_Exp_Calories!=0]
CBNPoor7[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


CBNPoor7[,FoodExpenditure_Per_total:=FoodExpenditure_Per*(1+(Per_Calory_Resturant/Per_Daily_Exp_Calories))]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
#x<-CBNPoor7[,.(Per_Calory_Resturant,Per_Daily_Calories,Bundle_Value,FoodExpenditure_Per_cluster,Daily_Calories3,Daily_Hoboobat_cluster,Daily_Ghand_cluster,Daily_Nan_cluster,Daily_Berenj_cluster,Daily_Berenj_cluster,Daily_Roghan_cluster,Daily_Goosht_cluster,Daily_Morgh_cluster,Daily_Mahi_cluster,Daily_Shir_cluster,Daily_Mast_cluster,Daily_Panir_cluster,Daily_Tokhmemorgh_cluster,Daily_Mive_cluster,Daily_Sabzi_cluster,Daily_Makarooni_cluster,Daily_Makarooni_cluster,Daily_Sibzamini_cluster,Daily_Calories_cluster2,Calory_Price,Daily_Exp_Calories)]
#x<-CBNPoor7[,.(FoodExpenditure_Per_total,Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,cluster)]

CBNPoor7[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor7[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]


# real prices
GhandIndexTehran<-282
HoboobatIndexTehran<-257
RoghanIndexTehran<-256
NanIndexTehran<-292
BerenjIndexTehran<-292
GooshtIndexTehran<-245
MorghIndexTehran<-245
MahiIndexTehran<-283
ShirIndexTehran<-254
PanirIndexTehran<-254
MastIndexTehran<-254
TokhmemorghIndexTehran<-254
MiveIndexTehran<-231
SabziIndexTehran<-257
MakarooniIndexTehran<-292
SibzaminiIndexTehran<-257


CBNPoor7<-CBNPoor7[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor7<-CBNPoor7[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor7<-CBNPoor7[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor7[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


a<-CBNPoor7[,.(Total_Exp_Month,Total_Exp_Month_Per,Weight,cluster,FoodExpenditure,Total_Exp_Month_nondurable,FoodExpenditure_Per)]

CBNPoor7<-CBNPoor7[,ratio1:=FoodExpenditure/Total_Exp_Month]
a<-a[,ratio1:=FoodExpenditure/Total_Exp_Month]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor7[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
#CBNPoor716[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor716[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor716[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]


#model for each cluster
#cluster 1
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor7[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_7<-Engel_Reverse1*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_7<-Engel_Reverse2*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_7<-Engel_Reverse3*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_7<-Engel_Reverse4*weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)



#w<-CBNPoor7[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor7[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor7[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 8###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline1_7 & cluster==1,1,0)]
c[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline1_7 & cluster==1 ,1,0)]
CBN[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline2_7 & cluster==2,1,Poor8)]
c[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline2_7 & cluster==2 ,1,Poor8)]
CBN[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline3_7 & cluster==3,1,Poor8)]
c[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline3_7 & cluster==3 ,1,Poor8)]
CBN[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline4_7 & cluster==4,1,Poor8)]
c[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline4_7 & cluster==4 ,1,Poor8)]
c[,weighted.mean(Poor8,Weight),by=cluster]
CBNPoor8<-CBN[Poor8==1]

#Calculations
CBNPoor8[,sum(Poor8*Weight),by=cluster][order(cluster)]
CBNPoor[,sum(Poor*Weight),by=cluster][order(cluster)]
CBNPoor8[,sum(Poor8),by=cluster][order(cluster)]
CBNPoor[,sum(Poor),by=cluster][order(cluster)]
#CBNPoor[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster][order(cluster)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
#CBNPoor8[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster][order(cluster)]
CBNPoor8[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][order(cluster)]
CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]
CBN[,weighted.mean(Poor8,Weight),by=cluster][order(cluster)]
CBN[,weighted.mean(Size,Weight),by=cluster][order(cluster)]



CBN[,sum(HIndivNo),by=cluster][order(cluster)]
CBN[,sum(Poor8),by=cluster][order(cluster)]
CBN[,sum(Weight),by=cluster][order(cluster)]
CBNPoor8[,sum(Weight),by=cluster][order(cluster)]
CBN[,sum(Size*Weight),by=cluster][order(cluster)]
CBNPoor8[,sum(Size*Weight),by=cluster][order(cluster)]

CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster][order(cluster)]
CBNPoor7[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster][order(cluster)]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster][order(cluster)]

CBN[,weighted.mean(Size,Weight),by=.(cluster,Poor8)][order(cluster)]
CBN[,weighted.mean(ifelse(HEmployed %in% "TRUE",1,0),Weight),by=.(cluster,Poor8)][order(cluster)]
CBN[,weighted.mean(ifelse(HLiterate %in% "TRUE",1,0),Weight),by=.(cluster,Poor8)][order(cluster)]
CBN[,weighted.mean(ifelse(HEduYears>10,1,0),Weight),by=.(cluster,Poor8)][order(cluster)]
CBN[,weighted.mean(ifelse(HSex %in% "Female",1,0),Weight),by=.(cluster,Poor8)][order(cluster)]
CBN[,weighted.mean(Poor8,Weight),by=.(HAge>50,cluster)][order(cluster)]
CBN[,weighted.mean(Poor8,Weight),by=.(HAge>40 & HAge<50,cluster)][order(cluster)]

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

