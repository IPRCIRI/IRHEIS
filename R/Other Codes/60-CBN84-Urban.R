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

load(file=paste0(Settings$HEISProcessedPath,"Y","84","HHBase.rda"))
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
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Ghand_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Hoboobat_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Roghan_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Berenj_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Nan_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Goosht_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Morgh_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Mahi_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Shir_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Mast_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Panir_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Tokhmemorgh_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Mive_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Sabzi_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Makarooni_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Sibzamini_Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Weights.rda"))

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
load(file=paste0(Settings$HEISProcessedPath,"Y","84","HHBase.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","HHI.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Foods.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Cigars.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Cloths.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Amusements.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Communications.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Durables.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Education.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Energy.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Furnitures.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Hotels.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","House.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Medicals.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Behdashts.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Transportations.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Others.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Investments.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Resturants.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","84","Weights.rda"))

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

CBN$Total_Exp_Month_Per<-CBN$Total_Exp_Month/CBN$EqSizeOECD
CBN$Total_Exp_Month_Per_nondurable<-CBN$Total_Exp_Month_nondurable/CBN$EqSizeOECD

#Calculate Per_Food Expenditures Monthly
CBN[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBN$FoodExpenditure_Per<-CBN$FoodExpenditure/CBN$EqSizeCalory

#Calculate Per_Food Expenditures Daily
CBN$FoodExpenditure_Per_day<-CBN$FoodExpenditure_Per/30

CBN$Ghandgram_Per_day<-CBN$Ghandgram/(30*CBN$EqSizeOECD)
CBN$Hoboobatgram_Per_day<-CBN$Hoboobatgram/(30*CBN$EqSizeOECD)
CBN$Berenjgram_Per_day<-CBN$Berenjgram/(30*CBN$EqSizeOECD)
CBN$Nangram_Per_day<-CBN$Nangram/(30*CBN$EqSizeOECD)
CBN$Roghangram_Per_day<-CBN$Roghangram/(30*CBN$EqSizeOECD)
CBN$Gooshtgram_Per_day<-CBN$Gooshtgram/(30*CBN$EqSizeOECD)
CBN$Morghgram_Per_day<-CBN$Morghgram/(30*CBN$EqSizeOECD)
CBN$Mahigram_Per_day<-CBN$Mahigram/(30*CBN$EqSizeOECD)
CBN$Shirgram_Per_day<-CBN$Shirgram/(30*CBN$EqSizeOECD)
CBN$Mastgram_Per_day<-CBN$Mastgram/(30*CBN$EqSizeOECD)
CBN$Panirgram_Per_day<-CBN$Panirgram/(30*CBN$EqSizeOECD)
CBN$Tokhmemorghgram_Per_day<-CBN$Tokhmemorghgram/(30*CBN$EqSizeOECD)
CBN$Mivegram_Per_day<-CBN$Mivegram/(30*CBN$EqSizeOECD)
CBN$Sabzigram_Per_day<-CBN$Sabzigram/(30*CBN$EqSizeOECD)
CBN$Makaroonigram_Per_day<-CBN$Makaroonigram/(30*CBN$EqSizeOECD)
CBN$Sibzaminigram_Per_day<-CBN$Sibzaminigram/(30*CBN$EqSizeOECD)
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
CBN$Home_W<-CBN$ServiceExp/CBN$EqSizeOECD
CBN$Home_Per_Metr<-CBN$MetrPrice/CBN$EqSizeOECD

#Seperate big cities
CBN[,sum(Weight*Size),by=ProvinceCode][order(V1)]
CBN[,HHIDs:=as.character(HHID)]
CBN[,ShahrestanCode:=as.integer(str_sub(HHIDs,2,5))]
CBN[,sum(Weight*Size),by=ShahrestanCode][order(V1)]
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
CBN[, Daily_Exp_Calories := Reduce(`+`, .SD), .SDcols=148:163][] 
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
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]

#K-means weights
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
###Iteration1-1
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes31<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)

#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-2
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes32<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)

#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-3
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes33<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)

#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]


###Iteration1-4
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes34<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-5
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes35<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-6
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes36<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-7
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes37<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-8
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes38<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-9
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes39<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]

###Iteration1-10
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(185:200)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(202:217)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]
#CBN[,weighted.mean(Poor,Weight),by=cluster][order(cluster)]

#Real Prices
T_Bundle_Value<-subset(CBNPoor, ProvinceCode==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=ProvinceCode]
CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=ProvinceCode]
CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=ProvinceCode]
Indexes2<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,ProvinceCode,Weight)]
Indexes2<-Indexes2[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
Indexes<-Indexes2[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
Indexes310<-Indexes[,.(ProvinceCode,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
Indexes<-Indexes[,.(ProvinceCode,RealPriceIndex)]
CBN[,RealPriceIndex:=NULL]
CBN<-merge(CBN,Indexes,by=c("ProvinceCode"),all.x = TRUE)
CBN<-CBN[,Total_Food_Month_Per2:=FoodExpenditure_Per*RealPriceIndex]
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
#utils::View(CBN)


#Sort Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]

#Calculate cumulative weights
sum(CBN$Weight)
CBN$cumweight <- cumsum(CBN$Weight)
tx <- max(CBN$cumweight)

#Calculate deciles by weights
CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]

#Update Poors
CBN[,Poor:=ifelse(Decile %in% 2:2,1,0)]
CBNPoor<-CBN[Poor==1]
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(HIndivNo),by=.(ProvinceCode)][order(ProvinceCode)]


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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(189:204)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(206:221)][] 

CBNPoor[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[Per_Daily_Exp_Calories!=0]
CBNPoor[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNCalory<-CBNPoor[,.(Per_Daily_Calories,Per_Daily_Exp_Calories,Per_Calory_Resturant,Resturant_Exp,cluster,ProvinceCode)]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]
CBN[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]

#Food expenditures (equal 2100 CCAL)
CBNPoor[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,sum(Size*Weight),by=cluster][order(cluster)]


#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor[cluster==1]
Food_Povertyline1_1<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor[cluster==2]
Food_Povertyline2_1<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor[cluster==3]
Food_Povertyline3_1<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor[cluster==4]
Food_Povertyline4_1<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)


#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]

#########Iteration 2###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_1 & cluster==1,1,0)]
c[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_1 & cluster==1 ,1,0)]
CBN[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_1 & cluster==2,1,Poor2)]
c[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_1 & cluster==2 ,1,Poor2)]
CBN[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_1 & cluster==3,1,Poor2)]
c[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_1 & cluster==3 ,1,Poor2)]
CBN[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_1 & cluster==4,1,Poor2)]
c[,Poor2:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_1 & cluster==4 ,1,Poor2)]
CBN[,weighted.mean(Poor2,Weight),by=cluster][order(cluster)]
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
CBNPoor2[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(190:205)][] 
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
CBNPoor2[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(207:222)][] 

CBNPoor2[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor2[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor2[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor2[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[Per_Daily_Exp_Calories!=0]
CBNPoor2[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor2[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]

#sum of total food expenditures
CBNPoor2[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor2[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor2[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]



#Calculations
CBNPoor2[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor2[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor2[cluster==1]
Food_Povertyline1_2<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor2[cluster==2]
Food_Povertyline2_2<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor2[cluster==3]
Food_Povertyline3_2<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor2[cluster==4]
Food_Povertyline4_2<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 3###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_2 & cluster==1,1,0)]
c[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_2 & cluster==1 ,1,0)]
CBN[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_2 & cluster==2,1,Poor3)]
c[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_2 & cluster==2 ,1,Poor3)]
CBN[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_2 & cluster==3,1,Poor3)]
c[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_2 & cluster==3 ,1,Poor3)]
CBN[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_2 & cluster==4,1,Poor3)]
c[,Poor3:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_2 & cluster==4 ,1,Poor3)]
CBN[,weighted.mean(Poor3,Weight),by=cluster][order(cluster)]
CBNPoor2[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
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
CBNPoor3[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
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
CBNPoor3[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor3[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor3[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor3[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor3[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[Per_Daily_Exp_Calories!=0]
CBNPoor3[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor3[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor3[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor3[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor3[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor3[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor3[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor3[cluster==1]
Food_Povertyline1_3<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor3[cluster==2]
Food_Povertyline2_3<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor3[cluster==3]
Food_Povertyline3_3<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor3[cluster==4]
Food_Povertyline4_3<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 4###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_3 & cluster==1,1,0)]
c[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_3 & cluster==1 ,1,0)]
CBN[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_3 & cluster==2,1,Poor4)]
c[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_3 & cluster==2 ,1,Poor4)]
CBN[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_3 & cluster==3,1,Poor4)]
c[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_3 & cluster==3 ,1,Poor4)]
CBN[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_3 & cluster==4,1,Poor4)]
c[,Poor4:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_3 & cluster==4 ,1,Poor4)]
CBN[,weighted.mean(Poor4,Weight),by=cluster][order(cluster)]
CBNPoor3[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
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
CBNPoor4[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
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
CBNPoor4[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor4[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor4[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor4[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor4[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[Per_Daily_Exp_Calories!=0]
CBNPoor4[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor4[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor4[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor4[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor4[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor4[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor4[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor4[cluster==1]
Food_Povertyline1_4<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor4[cluster==2]
Food_Povertyline2_4<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor4[cluster==3]
Food_Povertyline3_4<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor4[cluster==4]
Food_Povertyline4_4<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 5###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_4 & cluster==1,1,0)]
c[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_4 & cluster==1 ,1,0)]
CBN[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_4 & cluster==2,1,Poor5)]
c[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_4 & cluster==2 ,1,Poor5)]
CBN[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_4 & cluster==3,1,Poor5)]
c[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_4 & cluster==3 ,1,Poor5)]
CBN[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_4 & cluster==4,1,Poor5)]
c[,Poor5:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_4 & cluster==4 ,1,Poor5)]
CBN[,weighted.mean(Poor5,Weight),by=cluster][order(cluster)]
CBNPoor4[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
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
CBNPoor5[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
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
CBNPoor5[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor5[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor5[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor5[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor5[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[Per_Daily_Exp_Calories!=0]
CBNPoor5[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor5[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor5[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor5[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor5[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor5[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor5[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor5[cluster==1]
Food_Povertyline1_5<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor5[cluster==2]
Food_Povertyline2_5<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor5[cluster==3]
Food_Povertyline3_5<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor5[cluster==4]
Food_Povertyline4_5<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 6###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_5 & cluster==1,1,0)]
c[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_5 & cluster==1 ,1,0)]
CBN[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_5 & cluster==2,1,Poor6)]
c[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_5 & cluster==2 ,1,Poor6)]
CBN[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_5 & cluster==3,1,Poor6)]
c[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_5 & cluster==3 ,1,Poor6)]
CBN[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_5 & cluster==4,1,Poor6)]
c[,Poor6:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_5 & cluster==4 ,1,Poor6)]
CBN[,weighted.mean(Poor6,Weight),by=cluster][order(cluster)]
CBNPoor5[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
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
CBNPoor6[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
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
CBNPoor6[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor6[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor6[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor6[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor6[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[Per_Daily_Exp_Calories!=0]
CBNPoor6[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor6[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor6[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor6[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor6[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor6[cluster==1]
Food_Povertyline1_6<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor6[cluster==2]
Food_Povertyline2_6<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor6[cluster==3]
Food_Povertyline3_6<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor6[cluster==4]
Food_Povertyline4_6<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 7###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_6 & cluster==1,1,0)]
c[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_6 & cluster==1 ,1,0)]
CBN[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_6 & cluster==2,1,Poor7)]
c[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_6 & cluster==2 ,1,Poor7)]
CBN[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_6 & cluster==3,1,Poor7)]
c[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_6 & cluster==3 ,1,Poor7)]
CBN[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_6 & cluster==4,1,Poor7)]
c[,Poor7:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_6 & cluster==4 ,1,Poor7)]
CBN[,weighted.mean(Poor7,Weight),by=cluster][order(cluster)]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
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

CBNPoor7[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor7[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor7[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[Per_Daily_Exp_Calories!=0]
CBNPoor7[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor7[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor7[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor7[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor7[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor7[cluster==1]
Food_Povertyline1_7<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor7[cluster==2]
Food_Povertyline2_7<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor7[cluster==3]
Food_Povertyline3_7<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor7[cluster==4]
Food_Povertyline4_7<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 8###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_7 & cluster==1,1,0)]
c[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_7 & cluster==1 ,1,0)]
CBN[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_7 & cluster==2,1,Poor8)]
c[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_7 & cluster==2 ,1,Poor8)]
CBN[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_7 & cluster==3,1,Poor8)]
c[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_7 & cluster==3 ,1,Poor8)]
CBN[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_7 & cluster==4,1,Poor8)]
c[,Poor8:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_7 & cluster==4 ,1,Poor8)]
CBN[,weighted.mean(Poor8,Weight),by=cluster][order(cluster)]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBN[Poor8==1]


#CalculatePer_calories in clusters
CBNPoor8[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
#utils::View(CBNPoor8)

#Calculate Per_calories in clusters(=2100)
CBNPoor8[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor8[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor8[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor8[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor8[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[Per_Daily_Exp_Calories!=0]
CBNPoor8[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor8[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor8[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor8[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor8[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor8[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]


#Calculations
CBNPoor8[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor8[cluster==1]
Food_Povertyline1_8<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor8[cluster==2]
Food_Povertyline2_8<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor8[cluster==3]
Food_Povertyline3_8<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor8[cluster==4]
Food_Povertyline4_8<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 9###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_8 & cluster==1,1,0)]
c[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_8 & cluster==1 ,1,0)]
CBN[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_8 & cluster==2,1,Poor9)]
c[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_8 & cluster==2 ,1,Poor9)]
CBN[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_8 & cluster==3,1,Poor9)]
c[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_8 & cluster==3 ,1,Poor9)]
CBN[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_8 & cluster==4,1,Poor9)]
c[,Poor9:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_8 & cluster==4 ,1,Poor9)]
CBN[,weighted.mean(Poor9,Weight),by=cluster][order(cluster)]
CBNPoor8[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBN[Poor9==1]


#CalculatePer_calories in clusters
CBNPoor9[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(191:206)][] 
#utils::View(CBNPoor9)

#Calculate Per_calories in clusters(=2100)
CBNPoor9[,Daily2_Ghand:=(Daily_Ghand_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Ghand")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Hoboobat:=(Daily_Hoboobat_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Hoboobat")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Nan:=(Daily_Nan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Nan")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Berenj:=(Daily_Berenj_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Berenj")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Roghan:=(Daily_Roghan_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Roghan")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Goosht:=(Daily_Goosht_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Goosht")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Morgh:=(Daily_Morgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Morgh")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Mahi:=(Daily_Mahi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mahi")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Shir:=(Daily_Shir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Shir")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Mast:=(Daily_Mast_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mast")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Panir:=(Daily_Panir_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Panir")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Tokhmemorgh:=(Daily_Tokhmemorgh_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Tokhmemorgh")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Mive:=(Daily_Mive_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Mive")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Sabzi:=(Daily_Sabzi_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sabzi")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Makarooni:=(Daily_Makarooni_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Makarooni")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,Daily2_Sibzamini:=(Daily_Sibzamini_cluster*2100)/(Daily_Calories_cluster2)]
for (col in c("Daily2_Sibzamini")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(208:223)][] 


CBNPoor9[,FoodExpenditure_Per_cluster:=weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(FoodExpenditure_Per_cluster,Weight,na.rm = TRUE),by=cluster]

CBNPoor9[,Calory_Price:=(FoodExpenditure_Per_cluster/(Daily_Calories_cluster2))]
CBNPoor9[,weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=cluster]

#Calculate per_Calory from resturants
CBNPoor9[,Per_Calory_Resturant:=(0.7*Resturant_Exp/EqSizeCalory)/Calory_Price]
for (col in c("Per_Calory_Resturant")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[Per_Daily_Exp_Calories!=0]
CBNPoor9[,Per_Daily_Calories:=Per_Daily_Exp_Calories+Per_Calory_Resturant]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Per_Calory_Resturant,Weight,na.rm = TRUE),by=cluster]


#sum of total food expenditures
CBNPoor9[,FoodExpenditure_Per_total:=FoodExpenditure_Per+(0.7*Resturant_Exp/EqSizeCalory)]
CBNPoor9[,weighted.mean(FoodExpenditure_Per_total,Weight,na.rm = TRUE),by=cluster]

#Food expenditures (equal 2100 CCAL)
CBNPoor9[,Bundle_Value:=FoodExpenditure_Per_total*2100/Per_Daily_Calories]
CBNPoor9[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=ProvinceCode]



#Calculations
CBNPoor9[,weighted.mean(Per_Daily_Exp_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]



#Food Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBNPoor9[cluster==1]
Food_Povertyline1_9<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
CBNPoorCluster<-CBNPoor9[cluster==2]
Food_Povertyline2_9<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
CBNPoorCluster<-CBNPoor9[cluster==3]
Food_Povertyline3_9<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
CBNPoorCluster<-CBNPoor9[cluster==4]
Food_Povertyline4_9<-weighted.mean(CBNPoorCluster$Bundle_Value,CBNPoorCluster$Weight,na.rm = TRUE)

#ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Food_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,cluster)]
#mean(ee[,Total_Food_Month_Per2==Total_Exp_Month_nondurable_Real])
#mean(ee[,Total_Food_Month_Per2<3500000])
#ee<- ee[order(Total_Food_Month_Per2)]
#utils::View(CBN)
#for (col in c("Total_Food_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(FoodExpenditure_Per,FoodExpenditure_Per_total,Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Food_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 10###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Food_Month_Per2)]
c<-  c[order(Total_Food_Month_Per2)]

#Indicate new poors
CBN[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_9 & cluster==1,1,0)]
c[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline1_9 & cluster==1 ,1,0)]
CBN[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_9 & cluster==2,1,Poor10)]
c[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline2_9 & cluster==2 ,1,Poor10)]
CBN[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_9 & cluster==3,1,Poor10)]
c[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline3_9 & cluster==3 ,1,Poor10)]
CBN[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_9 & cluster==4,1,Poor10)]
c[,Poor10:=ifelse(FoodExpenditure_Per_total < Food_Povertyline4_9 & cluster==4 ,1,Poor10)]
CBN[,weighted.mean(Poor10,Weight),by=cluster][order(cluster)]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBN[Poor10==1]

#Engel
#CBNPoor9<-CBNPoor9[,ratio1:=FoodExpenditure/Total_Exp_Month]
#CBNPoor9[,weighted.mean(ratio1,Weight),by=cluster]
#summary(CBNPoor9$ratio1)
CBN<-CBN[,ratio1:=FoodExpenditure/Total_Exp_Month]
CBN[,weighted.mean(ratio1,Weight),by=ProvinceCode][order(ProvinceCode)]
CBN<-CBN[,ratio2:=ServiceExp/Total_Exp_Month]
CBN[,weighted.mean(ratio2,Weight),by=ProvinceCode][order(ProvinceCode)]

# Poverty Line for each cluster
#cluster 1
CBNPoorCluster<-CBN[cluster==1 & FoodExpenditure_Per<1.1*Food_Povertyline1_9 & FoodExpenditure_Per>0.90*Food_Povertyline1_9]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_9<-Engel_Reverse1*Food_Povertyline1_9

#cluster 2
CBNPoorCluster<-CBN[cluster==2 & FoodExpenditure_Per<1.1*Food_Povertyline2_9 & FoodExpenditure_Per>0.90*Food_Povertyline2_9]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_9<-Engel_Reverse2*Food_Povertyline2_9

#cluster 3
CBNPoorCluster<-CBN[cluster==3 & FoodExpenditure_Per<1.1*Food_Povertyline3_9 & FoodExpenditure_Per>0.90*Food_Povertyline3_9]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_9<-Engel_Reverse3*Food_Povertyline3_9

#cluster 4
CBNPoorCluster<-CBN[cluster==4 & FoodExpenditure_Per<1.1*Food_Povertyline4_9 & FoodExpenditure_Per>0.90*Food_Povertyline4_9]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_9<-Engel_Reverse4*Food_Povertyline4_9

#Indicate final poors
CBN<-CBN[,Total_Exp_Month_Per2:=Total_Exp_Month_Per_nondurable*RealPriceIndex]

CBN[,Poor11:=ifelse(Total_Exp_Month_Per_nondurable < Povertyline1_9 & cluster==1,1,0)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per_nondurable < Povertyline2_9 & cluster==2,1,Poor11)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per_nondurable < Povertyline3_9 & cluster==3,1,Poor11)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per_nondurable < Povertyline4_9 & cluster==4,1,Poor11)]
CBN[,weighted.mean(Poor11,Weight),by=cluster][order(cluster)]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBN[Poor11==1]

CBN[,sum(Size*Weight),by=cluster][order(cluster)]
CBNPoor9[,sum(Size*Weight),by=cluster][order(cluster)]
CBNPoor9[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBN[,weighted.mean(Poor11,Weight)]
CBN[,weighted.mean(Poor11,Weight),by=cluster][order(cluster)]
CBN[,weighted.mean(Poor11,Weight),by=ProvinceCode][order(ProvinceCode)]
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

