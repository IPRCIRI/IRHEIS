#CBN Method-Rural.R
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
CBN<-CBN[Size!=0]
CBN<-CBN[Region=="Rural"]
CBN<-CBN[FoodExpenditure!=0]


#Calculate Per_Total Expenditures Monthly
CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(65:77,82:83)][] 
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


#Assume that deciles 1 and 2 are poor
CBN[,Poor:=ifelse(Decile %in% 1:2,1,0)]
CBNPoor<-CBN[Poor==1]
PriceWeights<-CBN[,.(HHID,Ghand_W,Hoboobat_W,Roghan_W,Berenj_W,Nan_W,Goosht_W,Morgh_W,Mahi_W,Shir_W,Mast_W,Panir_W,Tokhmemorgh_W,Mive_W,Sabzi_W,Makarooni_W,Sibzamini_W,Home_W,ProvinceCode,Weight)]
dt3 <- PriceWeights[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode)]
dt3<- dt3[order(ProvinceCode)]
dt3 <- dt3[,.(Ghand_W,Hoboobat_W,Roghan_W,Berenj_W,Nan_W,Goosht_W,Morgh_W,Mahi_W,Shir_W,Mast_W,Panir_W,Tokhmemorgh_W,Mive_W,Sabzi_W,Makarooni_W,Sibzamini_W,Home_W)]

#PriceWeights<-Food[,.(Ghandgram,GhandPrice)]
#PriceWeights$Ghand_W<-Food$Ghandgram*Food$GhandPrice
#PriceWeights$Hoboobat_W<-Food$Hoboobatgram*Food$HoboobatPrice
#PriceWeights$Roghan_W<-Food$Roghangram*Food$RoghanPrice
#PriceWeights$Berenj_W<-Food$Berenjgram*Food$BerenjPrice
#PriceWeights$Nan_W<-Food$Nangram*Food$NanPrice
#PriceWeights$Goosht_W<-Food$Gooshtgram*Food$GooshtPrice
#PriceWeights$Morgh_W<-Food$Morghgram*Food$MorghPrice
#PriceWeights$Mahi_W<-Food$Mahigram*Food$MahiPrice
#PriceWeights$Shir_W<-Food$Shirgram*Food$ShirPrice
#PriceWeights$Mast_W<-Food$Mastgram*Food$MastPrice
#PriceWeights$Panir_W<-Food$Panirgram*Food$PanirPrice
#PriceWeights$Tokhmemorgh_W<-Food$Tokhmemorghgram*Food$TokhmemorghPrice
#PriceWeights$Mive_W<-Food$Mivegram*Food$MivePrice
#PriceWeights$Sabzi_W<-Food$Sabzigram*Food$SabziPrice
#PriceWeights$Makarooni_W<-Food$Makaroonigram*Food$MakarooniPrice
#PriceWeights$Sibzamini_W<-Food$Sibzaminigram*Food$SibzaminiPrice
#PriceWeights$Home_W<-CBN$ServiceExp

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
kmeans(dtW,5)						# Simple K-means

cl <- kmeans(dtW,5)
cl$cluster
dt2 <- dt2[,cluster:=data.table(cl$cluster)]
dt2<-dt2[,.(ProvinceCode,cluster)]
#plot(PRICE1, PRICE2,col=cl$cluster)
#points(cl$centers, pch=20)
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
CBNPoor[,sum(Weight*Size),by=cluster]
CBNPoor[,sum(Weight),by=cluster]
CBNPoor[,sum(Poor),by=cluster]
C2<-CBNPoor[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]
######################################################################    
#K-means algorithm for clustering by consumption
#test3<-CBNPoor[,.(Ghandgram,Hoboobatgram,Roghangram,Berenjgram,Nangram,Gooshtgram,Morghgram,Mahigram,Shirgram,Mastgram,Panirgram,Tokhmemorghgram,Mivegram,Sabzigram,Makaroonigram,Sibzaminigram,Region,ProvinceCode,Weight)]
#dt3 <- test3[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,ProvinceCode)]
#dt3<- dt3[order(ProvinceCode,Region)]
#for (col in c("MahiPrice")) dt3[is.nan(get(col)), (col) := 200000]
#dtt <- dt3 [,.(Ghandgram,Hoboobatgram,Roghangram,Berenjgram,Nangram,Gooshtgram,Morghgram,Mahigram,Shirgram,Mastgram,Panirgram,Tokhmemorghgram,Mivegram,Sabzigram,Makaroonigram,Sibzaminigram)]

#pca2 <- princomp(dtt, cor=T)
#Gram <- pca2$scores
#Gram1 <- -1*Gram[,1] 
#Gram2 <- -1*Gram[,2] 
#Gram3 <- -1*Gram[,3] 
#Gram4 <- -1*Gram[,4] 
#Gram5 <- -1*Gram[,5]
#Gram6 <- -1*Gram[,6] 
#Gram7 <- -1*Gram[,7] 
#Gram8 <- -1*Gram[,8] 
#Gram9 <- -1*Gram[,9] 
#Gram10 <- -1*Gram[,10] 
#Gram11 <- -1*Gram[,11] 
#Gram12 <- -1*Gram[,12] 
#Gram13 <- -1*Gram[,13]
#Gram14 <- -1*Gram[,14] 
#Gram15 <- -1*Gram[,15] 
#Gram16 <- -1*Gram[,16] 

#X12 <- cbind(Gram1, Gram2)
#cl2 <- kmeans(Gram,5)
#cl2$cluster
#dt3 <- dt3[,cluster:=data.table(cl$cluster)]
#dt3<-dt3[,.(ProvinceCode,Region,cluster)]
#plot(Gram1, Gram2,col=cl$cluster)
#points(cl$centers, pch=20)
#CBNPoor8<-merge(CBNPoor,dt3,by=c("ProvinceCode","Region"),all.x = TRUE)
#C28<-CBNPoor8[,.(HHID,ProvinceCode,Region,Decile,Poor,cluster)]
##########################################################
####Iteration 1#####

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
CBNPoor[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=180:195][] 
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
#utils::View(CBNPoor)

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
#utils::View(CBNPoor)

#CalculatePer_calories
CBNPoor[, Daily_Calories := Reduce(`+`, .SD), .SDcols=197:212][] 
CBNPoor[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor <- CBNPoor[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Size,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,sum(Weight*Size),by=cluster]
CBNPoor[,sum(Weight),by=cluster]
CBNPoor[,sum(Poor),by=cluster]

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
#utils::View(CBNPoor)

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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(233:248)][] 
#utils::View(CBNPoor)

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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(250:265)][] 

#utils::View(CBNPoor)


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
#utils::View(CBNPoor)

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
TotalIndexTehran<-235
KhorakIndexTehran<-260

CBNPoor<-CBNPoor[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor<-CBNPoor[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor<-CBNPoor[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor<-CBNPoor[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor<-CBNPoor[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor<-CBNPoor[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor<-CBNPoor[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor<-CBNPoor[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor<-CBNPoor[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor<-CBNPoor[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor<-CBNPoor[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor<-CBNPoor[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor<-CBNPoor[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor<-CBNPoor[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor<-CBNPoor[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor<-CBNPoor[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor<-CBNPoor[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor<-CBNPoor[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor<-CBNPoor[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]

# New bundle
CBNPoor[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(302:317)][] 
CBNPoor[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor)
CBNPoor<-CBNPoor[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor<-CBNPoor[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor<-CBNPoor[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]

CBNPoor<-CBNPoor[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor<-CBNPoor[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor[is.na(get(col)), (col) := 0]
CBNPoor[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(323:338)][] 
CBNPoor[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor)
#order(CBNPoor$AdditionalExpenditure_Per)

a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
CBNPoor[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per_day,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(FoodExpenditure_Per2100,Weight,na.rm = TRUE),by=cluster]


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1

CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_1<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_1<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_1<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_1<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_1<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

ee<-CBNPoor[,.(Total_Exp_Month_Real,Total_Exp_Month,Total_Exp_Month_Per2,Total_Exp_Month_Per_nondurable,Total_Exp_Month_nondurable_Real_Per,FoodExpenditure_Per,FoodExpenditure_Real_Per,cluster)]
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
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline5_1 & cluster==5,1,Poor2)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline5_1 & cluster==5 ,1,Poor2)]
CBN[,weighted.mean(Poor2,Weight),by=cluster]
CBNPoor[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]
CBNPoor2<-CBN[Poor2==1]

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
CBNPoor2[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=182:197][] 
CBNPoor2[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor2[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor2)

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
#utils::View(CBNPoor2)

#CalculatePer_calories
CBNPoor2[, Daily_Calories := Reduce(`+`, .SD), .SDcols=199:214][] 
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
#utils::View(CBNPoor2)

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
CBNPoor2[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(235:250)][] 
#utils::View(CBNPoor2)

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
CBNPoor2[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(252:267)][] 
#utils::View(CBNPoor2)

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
#utils::View(CBNPoor2)

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


CBNPoor2<-CBNPoor2[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor2<-CBNPoor2[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor2<-CBNPoor2[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor2<-CBNPoor2[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor2<-CBNPoor2[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor2<-CBNPoor2[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor2<-CBNPoor2[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor2<-CBNPoor2[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor2<-CBNPoor2[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor2<-CBNPoor2[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor2<-CBNPoor2[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor2<-CBNPoor2[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor2<-CBNPoor2[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor2<-CBNPoor2[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor2<-CBNPoor2[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor2<-CBNPoor2[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor2<-CBNPoor2[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor2[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor2[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(304:319)][] 
CBNPoor2[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor2)
CBNPoor2<-CBNPoor2[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor2[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor2<-CBNPoor2[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]

CBNPoor2<-CBNPoor2[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2<-CBNPoor2[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor2[is.na(get(col)), (col) := 0]
CBNPoor2[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(325:340)][] 
CBNPoor2[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor2)
#order(CBNPoor2$AdditionalExpenditure_Per)

a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor2)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor2[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_2<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_2<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_2<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_2<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_2<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline5_2 & cluster==5,1,Poor3)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline5_2 & cluster==5 ,1,Poor3)]
c[,weighted.mean(Poor3,Weight),by=cluster]
CBNPoor3<-CBN[Poor3==1]

#weighted consumption in each cluster
CBNPoor3<-CBNPoor3[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor3<-CBNPoor3[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor3<-CBNPoor3[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor3<-CBNPoor3[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor3<-CBNPoor3[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor3[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor3[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=183:198][] 
CBNPoor3[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor3[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor3)

# Food Calories
CBNPoor3$Ghand_Calory<- CBNPoor3$Ghandgram *4
CBNPoor3$Hoboobat_Calory<- CBNPoor3$Hoboobatgram *3
CBNPoor3$Nan_Calory<- CBNPoor3$Nangram *2.5
CBNPoor3$Berenj_Calory<- CBNPoor3$Berenjgram *1.2
CBNPoor3$Roghan_Calory<- CBNPoor3$Roghangram *8
CBNPoor3$Goosht_Calory<- CBNPoor3$Gooshtgram *2.5
CBNPoor3$Morgh_Calory<- CBNPoor3$Morghgram *2
CBNPoor3$Mahi_Calory<- CBNPoor3$Mahigram *1
CBNPoor3$Shir_Calory<- CBNPoor3$Shirgram *2.5
CBNPoor3$Mast_Calory<- CBNPoor3$Mastgram *1.5
CBNPoor3$Panir_Calory<- CBNPoor3$Mastgram *2.5
CBNPoor3$Tokhmemorgh_Calory<- CBNPoor3$Tokhmemorghgram *1.4
CBNPoor3$Mive_Calory<- CBNPoor3$Mivegram *0.5
CBNPoor3$Sabzi_Calory<- CBNPoor3$Sabzigram *0.5
CBNPoor3$Makarooni_Calory<- CBNPoor3$Makaroonigram *3.6
CBNPoor3$Sibzamini_Calory<- CBNPoor3$Sibzaminigram *0.9
#utils::View(CBNPoor3)

#CalculatePer_calories
CBNPoor3[, Daily_Calories := Reduce(`+`, .SD), .SDcols=200:215][] 
CBNPoor3[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor3[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor3 <- CBNPoor3[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor3[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]

CBNPoor3$Ghand_per_Calory<- CBNPoor3$Ghandgram *4/CBNPoor3$EqSizeCalory
CBNPoor3$Hoboobat_per_Calory<- CBNPoor3$Hoboobatgram *3/CBNPoor3$EqSizeCalory
CBNPoor3$Nan_per_Calory<- CBNPoor3$Nangram *2.5/CBNPoor3$EqSizeCalory
CBNPoor3$Berenj_per_Calory<- CBNPoor3$Berenjgram *1.2/CBNPoor3$EqSizeCalory
CBNPoor3$Roghan_per_Calory<- CBNPoor3$Roghangram *8/CBNPoor3$EqSizeCalory
CBNPoor3$Goosht_per_Calory<- CBNPoor3$Gooshtgram *2.5/CBNPoor3$EqSizeCalory
CBNPoor3$Morgh_per_Calory<- CBNPoor3$Morghgram *2/CBNPoor3$EqSizeCalory
CBNPoor3$Mahi_per_Calory<- CBNPoor3$Mahigram *1/CBNPoor3$EqSizeCalory
CBNPoor3$Shir_per_Calory<- CBNPoor3$Shirgram *2.5/CBNPoor3$EqSizeCalory
CBNPoor3$Mast_per_Calory<- CBNPoor3$Mastgram *1.5/CBNPoor3$EqSizeCalory
CBNPoor3$Panir_per_Calory<- CBNPoor3$Mastgram *2.5/CBNPoor3$EqSizeCalory
CBNPoor3$Tokhmemorgh_per_Calory<- CBNPoor3$Tokhmemorghgram *1.4/CBNPoor3$EqSizeCalory
CBNPoor3$Mive_per_Calory<- CBNPoor3$Mivegram *0.5/CBNPoor3$EqSizeCalory
CBNPoor3$Sabzi_per_Calory<- CBNPoor3$Sabzigram *0.5/CBNPoor3$EqSizeCalory
CBNPoor3$Makarooni_per_Calory<- CBNPoor3$Makaroonigram *3.6/CBNPoor3$EqSizeCalory
CBNPoor3$Sibzamini_per_Calory<- CBNPoor3$Sibzaminigram *0.9/CBNPoor3$EqSizeCalory
#utils::View(CBNPoor3)

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
CBNPoor3[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(236:251)][] 
#utils::View(CBNPoor3)

CBNPoor3[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor3[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(253:268)][] 
#utils::View(CBNPoor3)

# Food grams from Calories2
CBNPoor3$Ghandgram2  <- CBNPoor3$Daily2_Ghand /4
CBNPoor3$Hoboobatgram2 <- CBNPoor3$Daily2_Hoboobat /3
CBNPoor3$Nangram2 <- CBNPoor3$Daily2_Nan  /2.5
CBNPoor3$Berenjgram2 <- CBNPoor3$Daily2_Berenj  /1.2
CBNPoor3$Roghangram2 <- CBNPoor3$Daily2_Roghan /8
CBNPoor3$Gooshtgram2 <- CBNPoor3$Daily2_Goosht  /2.5
CBNPoor3$Morghgram2 <- CBNPoor3$Daily2_Morgh  /2
CBNPoor3$Mahigram2 <- CBNPoor3$Daily2_Mahi  /1
CBNPoor3$Shirgram2 <- CBNPoor3$Daily2_Shir  /2.5
CBNPoor3$Mastgram2 <- CBNPoor3$Daily2_Mast  /1.5
CBNPoor3$Panirgram2 <- CBNPoor3$Daily2_Panir  /2.5
CBNPoor3$Tokhmemorghgram2 <- CBNPoor3$Daily2_Tokhmemorgh /1.4
CBNPoor3$Mivegram2 <- CBNPoor3$Daily2_Mive  /0.5
CBNPoor3$Sabzigram2 <- CBNPoor3$Daily2_Sabzi  /0.5
CBNPoor3$Makaroonigram2 <- CBNPoor3$Daily2_Makarooni  /3.6
CBNPoor3$Sibzaminigram2 <- CBNPoor3$Daily2_Sibzamini /0.9
#utils::View(CBNPoor3)

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


CBNPoor3<-CBNPoor3[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor3<-CBNPoor3[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor3<-CBNPoor3[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor3<-CBNPoor3[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor3<-CBNPoor3[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor3<-CBNPoor3[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor3<-CBNPoor3[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor3<-CBNPoor3[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor3<-CBNPoor3[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor3<-CBNPoor3[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor3<-CBNPoor3[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor3<-CBNPoor3[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor3<-CBNPoor3[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor3<-CBNPoor3[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor3<-CBNPoor3[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor3<-CBNPoor3[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor3<-CBNPoor3[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor3[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor3[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(305:320)][] 
CBNPoor3[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor3)
CBNPoor3<-CBNPoor3[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor3[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor3<-CBNPoor3[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]

CBNPoor3<-CBNPoor3[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3<-CBNPoor3[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor3[is.na(get(col)), (col) := 0]
CBNPoor3[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(325:340)][] 
CBNPoor3[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor3)
#order(CBNPoor3$AdditionalExpenditure_Per)

a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor3)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor3[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_3<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_3<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_3<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_3<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_3<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline5_3 & cluster==5,1,Poor4)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline5_3 & cluster==5 ,1,Poor4)]
c[,weighted.mean(Poor4,Weight),by=cluster]
CBNPoor4<-CBN[Poor4==1]

#weighted consumption in each cluster
CBNPoor4<-CBNPoor4[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor4<-CBNPoor4[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor4<-CBNPoor4[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor4<-CBNPoor4[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor4<-CBNPoor4[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor4[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor4[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=184:199][] 
CBNPoor4[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor4[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor4)

# Food Calories
CBNPoor4$Ghand_Calory<- CBNPoor4$Ghandgram *4
CBNPoor4$Hoboobat_Calory<- CBNPoor4$Hoboobatgram *3
CBNPoor4$Nan_Calory<- CBNPoor4$Nangram *2.5
CBNPoor4$Berenj_Calory<- CBNPoor4$Berenjgram *1.2
CBNPoor4$Roghan_Calory<- CBNPoor4$Roghangram *8
CBNPoor4$Goosht_Calory<- CBNPoor4$Gooshtgram *2.5
CBNPoor4$Morgh_Calory<- CBNPoor4$Morghgram *2
CBNPoor4$Mahi_Calory<- CBNPoor4$Mahigram *1
CBNPoor4$Shir_Calory<- CBNPoor4$Shirgram *2.5
CBNPoor4$Mast_Calory<- CBNPoor4$Mastgram *1.5
CBNPoor4$Panir_Calory<- CBNPoor4$Mastgram *2.5
CBNPoor4$Tokhmemorgh_Calory<- CBNPoor4$Tokhmemorghgram *1.4
CBNPoor4$Mive_Calory<- CBNPoor4$Mivegram *0.5
CBNPoor4$Sabzi_Calory<- CBNPoor4$Sabzigram *0.5
CBNPoor4$Makarooni_Calory<- CBNPoor4$Makaroonigram *3.6
CBNPoor4$Sibzamini_Calory<- CBNPoor4$Sibzaminigram *0.9
#utils::View(CBNPoor4)

#CalculatePer_calories
CBNPoor4[, Daily_Calories := Reduce(`+`, .SD), .SDcols=201:216][] 
CBNPoor4[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor4[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor4 <- CBNPoor4[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor4[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]

CBNPoor4$Ghand_per_Calory<- CBNPoor4$Ghandgram *4/CBNPoor4$EqSizeCalory
CBNPoor4$Hoboobat_per_Calory<- CBNPoor4$Hoboobatgram *3/CBNPoor4$EqSizeCalory
CBNPoor4$Nan_per_Calory<- CBNPoor4$Nangram *2.5/CBNPoor4$EqSizeCalory
CBNPoor4$Berenj_per_Calory<- CBNPoor4$Berenjgram *1.2/CBNPoor4$EqSizeCalory
CBNPoor4$Roghan_per_Calory<- CBNPoor4$Roghangram *8/CBNPoor4$EqSizeCalory
CBNPoor4$Goosht_per_Calory<- CBNPoor4$Gooshtgram *2.5/CBNPoor4$EqSizeCalory
CBNPoor4$Morgh_per_Calory<- CBNPoor4$Morghgram *2/CBNPoor4$EqSizeCalory
CBNPoor4$Mahi_per_Calory<- CBNPoor4$Mahigram *1/CBNPoor4$EqSizeCalory
CBNPoor4$Shir_per_Calory<- CBNPoor4$Shirgram *2.5/CBNPoor4$EqSizeCalory
CBNPoor4$Mast_per_Calory<- CBNPoor4$Mastgram *1.5/CBNPoor4$EqSizeCalory
CBNPoor4$Panir_per_Calory<- CBNPoor4$Mastgram *2.5/CBNPoor4$EqSizeCalory
CBNPoor4$Tokhmemorgh_per_Calory<- CBNPoor4$Tokhmemorghgram *1.4/CBNPoor4$EqSizeCalory
CBNPoor4$Mive_per_Calory<- CBNPoor4$Mivegram *0.5/CBNPoor4$EqSizeCalory
CBNPoor4$Sabzi_per_Calory<- CBNPoor4$Sabzigram *0.5/CBNPoor4$EqSizeCalory
CBNPoor4$Makarooni_per_Calory<- CBNPoor4$Makaroonigram *3.6/CBNPoor4$EqSizeCalory
CBNPoor4$Sibzamini_per_Calory<- CBNPoor4$Sibzaminigram *0.9/CBNPoor4$EqSizeCalory
#utils::View(CBNPoor4)

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
CBNPoor4[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(237:252)][] 
#utils::View(CBNPoor4)

CBNPoor4[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor4[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(254:269)][] 
#utils::View(CBNPoor4)

# Food grams from Calories2
CBNPoor4$Ghandgram2  <- CBNPoor4$Daily2_Ghand /4
CBNPoor4$Hoboobatgram2 <- CBNPoor4$Daily2_Hoboobat /3
CBNPoor4$Nangram2 <- CBNPoor4$Daily2_Nan  /2.5
CBNPoor4$Berenjgram2 <- CBNPoor4$Daily2_Berenj  /1.2
CBNPoor4$Roghangram2 <- CBNPoor4$Daily2_Roghan /8
CBNPoor4$Gooshtgram2 <- CBNPoor4$Daily2_Goosht  /2.5
CBNPoor4$Morghgram2 <- CBNPoor4$Daily2_Morgh  /2
CBNPoor4$Mahigram2 <- CBNPoor4$Daily2_Mahi  /1
CBNPoor4$Shirgram2 <- CBNPoor4$Daily2_Shir  /2.5
CBNPoor4$Mastgram2 <- CBNPoor4$Daily2_Mast  /1.5
CBNPoor4$Panirgram2 <- CBNPoor4$Daily2_Panir  /2.5
CBNPoor4$Tokhmemorghgram2 <- CBNPoor4$Daily2_Tokhmemorgh /1.4
CBNPoor4$Mivegram2 <- CBNPoor4$Daily2_Mive  /0.5
CBNPoor4$Sabzigram2 <- CBNPoor4$Daily2_Sabzi  /0.5
CBNPoor4$Makaroonigram2 <- CBNPoor4$Daily2_Makarooni  /3.6
CBNPoor4$Sibzaminigram2 <- CBNPoor4$Daily2_Sibzamini /0.9
#utils::View(CBNPoor4)

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


CBNPoor4<-CBNPoor4[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor4<-CBNPoor4[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor4<-CBNPoor4[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor4<-CBNPoor4[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor4<-CBNPoor4[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor4<-CBNPoor4[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor4<-CBNPoor4[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor4<-CBNPoor4[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor4<-CBNPoor4[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor4<-CBNPoor4[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor4<-CBNPoor4[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor4<-CBNPoor4[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor4<-CBNPoor4[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor4<-CBNPoor4[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor4<-CBNPoor4[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor4<-CBNPoor4[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor4<-CBNPoor4[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor4[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor4[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(306:321)][] 
CBNPoor4[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor4)
CBNPoor4<-CBNPoor4[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor4<-CBNPoor4[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4<-CBNPoor4[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor4[is.na(get(col)), (col) := 0]
CBNPoor4[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(326:341)][] 
CBNPoor4[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor4)
#order(CBNPoor4$AdditionalExpenditure_Per)

a<-CBNPoor4[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor4<-CBNPoor4[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor4)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor4[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_4<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_4<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_4<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_4<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_4<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline5_4 & cluster==5,1,Poor5)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline5_4 & cluster==5 ,1,Poor5)]
c[,weighted.mean(Poor5,Weight),by=cluster]
CBNPoor5<-CBN[Poor5==1]


#weighted consumption in each cluster
CBNPoor5<-CBNPoor5[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor5<-CBNPoor5[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor5<-CBNPoor5[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor5<-CBNPoor5[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor5<-CBNPoor5[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor5[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor5[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=185:200][] 
CBNPoor5[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor5[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor5)

# Food Calories
CBNPoor5$Ghand_Calory<- CBNPoor5$Ghandgram *4
CBNPoor5$Hoboobat_Calory<- CBNPoor5$Hoboobatgram *3
CBNPoor5$Nan_Calory<- CBNPoor5$Nangram *2.5
CBNPoor5$Berenj_Calory<- CBNPoor5$Berenjgram *1.2
CBNPoor5$Roghan_Calory<- CBNPoor5$Roghangram *8
CBNPoor5$Goosht_Calory<- CBNPoor5$Gooshtgram *2.5
CBNPoor5$Morgh_Calory<- CBNPoor5$Morghgram *2
CBNPoor5$Mahi_Calory<- CBNPoor5$Mahigram *1
CBNPoor5$Shir_Calory<- CBNPoor5$Shirgram *2.5
CBNPoor5$Mast_Calory<- CBNPoor5$Mastgram *1.5
CBNPoor5$Panir_Calory<- CBNPoor5$Mastgram *2.5
CBNPoor5$Tokhmemorgh_Calory<- CBNPoor5$Tokhmemorghgram *1.4
CBNPoor5$Mive_Calory<- CBNPoor5$Mivegram *0.5
CBNPoor5$Sabzi_Calory<- CBNPoor5$Sabzigram *0.5
CBNPoor5$Makarooni_Calory<- CBNPoor5$Makaroonigram *3.6
CBNPoor5$Sibzamini_Calory<- CBNPoor5$Sibzaminigram *0.9
#utils::View(CBNPoor5)

#CalculatePer_calories
CBNPoor5[, Daily_Calories := Reduce(`+`, .SD), .SDcols=202:217][] 
CBNPoor5[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor5[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor5 <- CBNPoor5[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor5[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]

CBNPoor5$Ghand_per_Calory<- CBNPoor5$Ghandgram *4/CBNPoor5$EqSizeCalory
CBNPoor5$Hoboobat_per_Calory<- CBNPoor5$Hoboobatgram *3/CBNPoor5$EqSizeCalory
CBNPoor5$Nan_per_Calory<- CBNPoor5$Nangram *2.5/CBNPoor5$EqSizeCalory
CBNPoor5$Berenj_per_Calory<- CBNPoor5$Berenjgram *1.2/CBNPoor5$EqSizeCalory
CBNPoor5$Roghan_per_Calory<- CBNPoor5$Roghangram *8/CBNPoor5$EqSizeCalory
CBNPoor5$Goosht_per_Calory<- CBNPoor5$Gooshtgram *2.5/CBNPoor5$EqSizeCalory
CBNPoor5$Morgh_per_Calory<- CBNPoor5$Morghgram *2/CBNPoor5$EqSizeCalory
CBNPoor5$Mahi_per_Calory<- CBNPoor5$Mahigram *1/CBNPoor5$EqSizeCalory
CBNPoor5$Shir_per_Calory<- CBNPoor5$Shirgram *2.5/CBNPoor5$EqSizeCalory
CBNPoor5$Mast_per_Calory<- CBNPoor5$Mastgram *1.5/CBNPoor5$EqSizeCalory
CBNPoor5$Panir_per_Calory<- CBNPoor5$Mastgram *2.5/CBNPoor5$EqSizeCalory
CBNPoor5$Tokhmemorgh_per_Calory<- CBNPoor5$Tokhmemorghgram *1.4/CBNPoor5$EqSizeCalory
CBNPoor5$Mive_per_Calory<- CBNPoor5$Mivegram *0.5/CBNPoor5$EqSizeCalory
CBNPoor5$Sabzi_per_Calory<- CBNPoor5$Sabzigram *0.5/CBNPoor5$EqSizeCalory
CBNPoor5$Makarooni_per_Calory<- CBNPoor5$Makaroonigram *3.6/CBNPoor5$EqSizeCalory
CBNPoor5$Sibzamini_per_Calory<- CBNPoor5$Sibzaminigram *0.9/CBNPoor5$EqSizeCalory
#utils::View(CBNPoor5)

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
CBNPoor5[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(238:253)][] 
#utils::View(CBNPoor5)

CBNPoor5[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor5[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(255:270)][] 
#utils::View(CBNPoor5)

# Food grams from Calories2
CBNPoor5$Ghandgram2  <- CBNPoor5$Daily2_Ghand /4
CBNPoor5$Hoboobatgram2 <- CBNPoor5$Daily2_Hoboobat /3
CBNPoor5$Nangram2 <- CBNPoor5$Daily2_Nan  /2.5
CBNPoor5$Berenjgram2 <- CBNPoor5$Daily2_Berenj  /1.2
CBNPoor5$Roghangram2 <- CBNPoor5$Daily2_Roghan /8
CBNPoor5$Gooshtgram2 <- CBNPoor5$Daily2_Goosht  /2.5
CBNPoor5$Morghgram2 <- CBNPoor5$Daily2_Morgh  /2
CBNPoor5$Mahigram2 <- CBNPoor5$Daily2_Mahi  /1
CBNPoor5$Shirgram2 <- CBNPoor5$Daily2_Shir  /2.5
CBNPoor5$Mastgram2 <- CBNPoor5$Daily2_Mast  /1.5
CBNPoor5$Panirgram2 <- CBNPoor5$Daily2_Panir  /2.5
CBNPoor5$Tokhmemorghgram2 <- CBNPoor5$Daily2_Tokhmemorgh /1.4
CBNPoor5$Mivegram2 <- CBNPoor5$Daily2_Mive  /0.5
CBNPoor5$Sabzigram2 <- CBNPoor5$Daily2_Sabzi  /0.5
CBNPoor5$Makaroonigram2 <- CBNPoor5$Daily2_Makarooni  /3.6
CBNPoor5$Sibzaminigram2 <- CBNPoor5$Daily2_Sibzamini /0.9
#utils::View(CBNPoor5)

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


CBNPoor5<-CBNPoor5[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor5<-CBNPoor5[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor5<-CBNPoor5[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor5<-CBNPoor5[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor5<-CBNPoor5[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor5<-CBNPoor5[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor5<-CBNPoor5[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor5<-CBNPoor5[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor5<-CBNPoor5[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor5<-CBNPoor5[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor5<-CBNPoor5[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor5<-CBNPoor5[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor5<-CBNPoor5[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor5<-CBNPoor5[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor5<-CBNPoor5[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor5<-CBNPoor5[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor5<-CBNPoor5[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor5[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor5[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(307:322)][] 
CBNPoor5[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor5)
CBNPoor5<-CBNPoor5[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor5<-CBNPoor5[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5<-CBNPoor5[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor5[is.na(get(col)), (col) := 0]
CBNPoor5[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(327:342)][] 
CBNPoor5[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor5)
#order(CBNPoor5$AdditionalExpenditure_Per)


a<-CBNPoor5[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor5<-CBNPoor5[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor5)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor5[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_5<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_5<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_5<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_5<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_5<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline5_5 & cluster==5,1,Poor6)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline5_5 & cluster==5 ,1,Poor6)]
c[,weighted.mean(Poor6,Weight),by=cluster]
CBNPoor6<-CBN[Poor6==1]

#weighted consumption in each cluster
CBNPoor6<-CBNPoor6[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor6<-CBNPoor6[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor6<-CBNPoor6[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor6<-CBNPoor6[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor6<-CBNPoor6[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor6[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor6[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=186:201][] 
CBNPoor6[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor6[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor6)

# Food Calories
CBNPoor6$Ghand_Calory<- CBNPoor6$Ghandgram *4
CBNPoor6$Hoboobat_Calory<- CBNPoor6$Hoboobatgram *3
CBNPoor6$Nan_Calory<- CBNPoor6$Nangram *2.5
CBNPoor6$Berenj_Calory<- CBNPoor6$Berenjgram *1.2
CBNPoor6$Roghan_Calory<- CBNPoor6$Roghangram *8
CBNPoor6$Goosht_Calory<- CBNPoor6$Gooshtgram *2.5
CBNPoor6$Morgh_Calory<- CBNPoor6$Morghgram *2
CBNPoor6$Mahi_Calory<- CBNPoor6$Mahigram *1
CBNPoor6$Shir_Calory<- CBNPoor6$Shirgram *2.5
CBNPoor6$Mast_Calory<- CBNPoor6$Mastgram *1.5
CBNPoor6$Panir_Calory<- CBNPoor6$Mastgram *2.5
CBNPoor6$Tokhmemorgh_Calory<- CBNPoor6$Tokhmemorghgram *1.4
CBNPoor6$Mive_Calory<- CBNPoor6$Mivegram *0.5
CBNPoor6$Sabzi_Calory<- CBNPoor6$Sabzigram *0.5
CBNPoor6$Makarooni_Calory<- CBNPoor6$Makaroonigram *3.6
CBNPoor6$Sibzamini_Calory<- CBNPoor6$Sibzaminigram *0.9
#utils::View(CBNPoor6)

#CalculatePer_calories
CBNPoor6[, Daily_Calories := Reduce(`+`, .SD), .SDcols=203:218][] 
CBNPoor6[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor6[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor6 <- CBNPoor6[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor6[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor6[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor6$Ghand_per_Calory<- CBNPoor6$Ghandgram *4/CBNPoor6$EqSizeCalory
CBNPoor6$Hoboobat_per_Calory<- CBNPoor6$Hoboobatgram *3/CBNPoor6$EqSizeCalory
CBNPoor6$Nan_per_Calory<- CBNPoor6$Nangram *2.5/CBNPoor6$EqSizeCalory
CBNPoor6$Berenj_per_Calory<- CBNPoor6$Berenjgram *1.2/CBNPoor6$EqSizeCalory
CBNPoor6$Roghan_per_Calory<- CBNPoor6$Roghangram *8/CBNPoor6$EqSizeCalory
CBNPoor6$Goosht_per_Calory<- CBNPoor6$Gooshtgram *2.5/CBNPoor6$EqSizeCalory
CBNPoor6$Morgh_per_Calory<- CBNPoor6$Morghgram *2/CBNPoor6$EqSizeCalory
CBNPoor6$Mahi_per_Calory<- CBNPoor6$Mahigram *1/CBNPoor6$EqSizeCalory
CBNPoor6$Shir_per_Calory<- CBNPoor6$Shirgram *2.5/CBNPoor6$EqSizeCalory
CBNPoor6$Mast_per_Calory<- CBNPoor6$Mastgram *1.5/CBNPoor6$EqSizeCalory
CBNPoor6$Panir_per_Calory<- CBNPoor6$Mastgram *2.5/CBNPoor6$EqSizeCalory
CBNPoor6$Tokhmemorgh_per_Calory<- CBNPoor6$Tokhmemorghgram *1.4/CBNPoor6$EqSizeCalory
CBNPoor6$Mive_per_Calory<- CBNPoor6$Mivegram *0.5/CBNPoor6$EqSizeCalory
CBNPoor6$Sabzi_per_Calory<- CBNPoor6$Sabzigram *0.5/CBNPoor6$EqSizeCalory
CBNPoor6$Makarooni_per_Calory<- CBNPoor6$Makaroonigram *3.6/CBNPoor6$EqSizeCalory
CBNPoor6$Sibzamini_per_Calory<- CBNPoor6$Sibzaminigram *0.9/CBNPoor6$EqSizeCalory
#utils::View(CBNPoor6)

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
CBNPoor6[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(239:254)][] 
#utils::View(CBNPoor6)

CBNPoor6[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor6[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(256:271)][] 
#utils::View(CBNPoor6)

# Food grams from Calories2
CBNPoor6$Ghandgram2  <- CBNPoor6$Daily2_Ghand /4
CBNPoor6$Hoboobatgram2 <- CBNPoor6$Daily2_Hoboobat /3
CBNPoor6$Nangram2 <- CBNPoor6$Daily2_Nan  /2.5
CBNPoor6$Berenjgram2 <- CBNPoor6$Daily2_Berenj  /1.2
CBNPoor6$Roghangram2 <- CBNPoor6$Daily2_Roghan /8
CBNPoor6$Gooshtgram2 <- CBNPoor6$Daily2_Goosht  /2.5
CBNPoor6$Morghgram2 <- CBNPoor6$Daily2_Morgh  /2
CBNPoor6$Mahigram2 <- CBNPoor6$Daily2_Mahi  /1
CBNPoor6$Shirgram2 <- CBNPoor6$Daily2_Shir  /2.5
CBNPoor6$Mastgram2 <- CBNPoor6$Daily2_Mast  /1.5
CBNPoor6$Panirgram2 <- CBNPoor6$Daily2_Panir  /2.5
CBNPoor6$Tokhmemorghgram2 <- CBNPoor6$Daily2_Tokhmemorgh /1.4
CBNPoor6$Mivegram2 <- CBNPoor6$Daily2_Mive  /0.5
CBNPoor6$Sabzigram2 <- CBNPoor6$Daily2_Sabzi  /0.5
CBNPoor6$Makaroonigram2 <- CBNPoor6$Daily2_Makarooni  /3.6
CBNPoor6$Sibzaminigram2 <- CBNPoor6$Daily2_Sibzamini /0.9
#utils::View(CBNPoor6)

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


CBNPoor6<-CBNPoor6[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor6<-CBNPoor6[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor6<-CBNPoor6[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor6<-CBNPoor6[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor6<-CBNPoor6[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor6<-CBNPoor6[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor6<-CBNPoor6[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor6<-CBNPoor6[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor6<-CBNPoor6[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor6<-CBNPoor6[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor6<-CBNPoor6[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor6<-CBNPoor6[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor6<-CBNPoor6[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor6<-CBNPoor6[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor6<-CBNPoor6[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor6<-CBNPoor6[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor6<-CBNPoor6[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor6[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor6[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(308:323)][] 
CBNPoor6[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor6)
CBNPoor6<-CBNPoor6[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor6<-CBNPoor6[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6<-CBNPoor6[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor6[is.na(get(col)), (col) := 0]
CBNPoor6[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(328:343)][] 
CBNPoor6[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor6)
#order(CBNPoor6$AdditionalExpenditure_Per)

a<-CBNPoor6[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor6<-CBNPoor6[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor6)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor6[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_6<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_6<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_6<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_6<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_6<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline5_6 & cluster==5,1,Poor7)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline5_6 & cluster==5 ,1,Poor7)]
c[,weighted.mean(Poor7,Weight),by=cluster]
CBNPoor7<-CBN[Poor7==1]

#weighted consumption in each cluster
CBNPoor7<-CBNPoor7[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor7<-CBNPoor7[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor7<-CBNPoor7[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor7<-CBNPoor7[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor7<-CBNPoor7[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor7[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor7[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=187:202][] 
CBNPoor7[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor7[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor7)

# Food Calories
CBNPoor7$Ghand_Calory<- CBNPoor7$Ghandgram *4
CBNPoor7$Hoboobat_Calory<- CBNPoor7$Hoboobatgram *3
CBNPoor7$Nan_Calory<- CBNPoor7$Nangram *2.5
CBNPoor7$Berenj_Calory<- CBNPoor7$Berenjgram *1.2
CBNPoor7$Roghan_Calory<- CBNPoor7$Roghangram *8
CBNPoor7$Goosht_Calory<- CBNPoor7$Gooshtgram *2.5
CBNPoor7$Morgh_Calory<- CBNPoor7$Morghgram *2
CBNPoor7$Mahi_Calory<- CBNPoor7$Mahigram *1
CBNPoor7$Shir_Calory<- CBNPoor7$Shirgram *2.5
CBNPoor7$Mast_Calory<- CBNPoor7$Mastgram *1.5
CBNPoor7$Panir_Calory<- CBNPoor7$Mastgram *2.5
CBNPoor7$Tokhmemorgh_Calory<- CBNPoor7$Tokhmemorghgram *1.4
CBNPoor7$Mive_Calory<- CBNPoor7$Mivegram *0.5
CBNPoor7$Sabzi_Calory<- CBNPoor7$Sabzigram *0.5
CBNPoor7$Makarooni_Calory<- CBNPoor7$Makaroonigram *3.6
CBNPoor7$Sibzamini_Calory<- CBNPoor7$Sibzaminigram *0.9
#utils::View(CBNPoor7)

#CalculatePer_calories
CBNPoor7[, Daily_Calories := Reduce(`+`, .SD), .SDcols=204:219][] 
CBNPoor7[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor7[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor7 <- CBNPoor7[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor7[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor7[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor7$Ghand_per_Calory<- CBNPoor7$Ghandgram *4/CBNPoor7$EqSizeCalory
CBNPoor7$Hoboobat_per_Calory<- CBNPoor7$Hoboobatgram *3/CBNPoor7$EqSizeCalory
CBNPoor7$Nan_per_Calory<- CBNPoor7$Nangram *2.5/CBNPoor7$EqSizeCalory
CBNPoor7$Berenj_per_Calory<- CBNPoor7$Berenjgram *1.2/CBNPoor7$EqSizeCalory
CBNPoor7$Roghan_per_Calory<- CBNPoor7$Roghangram *8/CBNPoor7$EqSizeCalory
CBNPoor7$Goosht_per_Calory<- CBNPoor7$Gooshtgram *2.5/CBNPoor7$EqSizeCalory
CBNPoor7$Morgh_per_Calory<- CBNPoor7$Morghgram *2/CBNPoor7$EqSizeCalory
CBNPoor7$Mahi_per_Calory<- CBNPoor7$Mahigram *1/CBNPoor7$EqSizeCalory
CBNPoor7$Shir_per_Calory<- CBNPoor7$Shirgram *2.5/CBNPoor7$EqSizeCalory
CBNPoor7$Mast_per_Calory<- CBNPoor7$Mastgram *1.5/CBNPoor7$EqSizeCalory
CBNPoor7$Panir_per_Calory<- CBNPoor7$Mastgram *2.5/CBNPoor7$EqSizeCalory
CBNPoor7$Tokhmemorgh_per_Calory<- CBNPoor7$Tokhmemorghgram *1.4/CBNPoor7$EqSizeCalory
CBNPoor7$Mive_per_Calory<- CBNPoor7$Mivegram *0.5/CBNPoor7$EqSizeCalory
CBNPoor7$Sabzi_per_Calory<- CBNPoor7$Sabzigram *0.5/CBNPoor7$EqSizeCalory
CBNPoor7$Makarooni_per_Calory<- CBNPoor7$Makaroonigram *3.6/CBNPoor7$EqSizeCalory
CBNPoor7$Sibzamini_per_Calory<- CBNPoor7$Sibzaminigram *0.9/CBNPoor7$EqSizeCalory
#utils::View(CBNPoor7)

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
CBNPoor7[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(240:255)][] 
#utils::View(CBNPoor7)

CBNPoor7[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor7[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(257:272)][] 
#utils::View(CBNPoor7)

# Food grams from Calories2
CBNPoor7$Ghandgram2  <- CBNPoor7$Daily2_Ghand /4
CBNPoor7$Hoboobatgram2 <- CBNPoor7$Daily2_Hoboobat /3
CBNPoor7$Nangram2 <- CBNPoor7$Daily2_Nan  /2.5
CBNPoor7$Berenjgram2 <- CBNPoor7$Daily2_Berenj  /1.2
CBNPoor7$Roghangram2 <- CBNPoor7$Daily2_Roghan /8
CBNPoor7$Gooshtgram2 <- CBNPoor7$Daily2_Goosht  /2.5
CBNPoor7$Morghgram2 <- CBNPoor7$Daily2_Morgh  /2
CBNPoor7$Mahigram2 <- CBNPoor7$Daily2_Mahi  /1
CBNPoor7$Shirgram2 <- CBNPoor7$Daily2_Shir  /2.5
CBNPoor7$Mastgram2 <- CBNPoor7$Daily2_Mast  /1.5
CBNPoor7$Panirgram2 <- CBNPoor7$Daily2_Panir  /2.5
CBNPoor7$Tokhmemorghgram2 <- CBNPoor7$Daily2_Tokhmemorgh /1.4
CBNPoor7$Mivegram2 <- CBNPoor7$Daily2_Mive  /0.5
CBNPoor7$Sabzigram2 <- CBNPoor7$Daily2_Sabzi  /0.5
CBNPoor7$Makaroonigram2 <- CBNPoor7$Daily2_Makarooni  /3.6
CBNPoor7$Sibzaminigram2 <- CBNPoor7$Daily2_Sibzamini /0.9
#utils::View(CBNPoor7)

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


CBNPoor7<-CBNPoor7[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor7<-CBNPoor7[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor7<-CBNPoor7[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor7<-CBNPoor7[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor7<-CBNPoor7[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor7<-CBNPoor7[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor7<-CBNPoor7[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor7<-CBNPoor7[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor7<-CBNPoor7[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor7<-CBNPoor7[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor7<-CBNPoor7[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor7<-CBNPoor7[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor7<-CBNPoor7[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor7<-CBNPoor7[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor7<-CBNPoor7[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor7<-CBNPoor7[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor7<-CBNPoor7[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor7<-CBNPoor7[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor7[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor7[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(309:324)][] 
CBNPoor7[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor7)
CBNPoor7<-CBNPoor7[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor7<-CBNPoor7[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor7<-CBNPoor7[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7<-CBNPoor7[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor7[is.na(get(col)), (col) := 0]
CBNPoor7[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(329:344)][] 
CBNPoor7[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor7)
#order(CBNPoor7$AdditionalExpenditure_Per)

a<-CBNPoor7[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor7<-CBNPoor7[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor7)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor7,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor7[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_7<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor7,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_7<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor7,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_7<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor7,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_7<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor7,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor7<-CBNPoor7[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor7[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_7<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


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
CBN[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline5_7 & cluster==5,1,Poor8)]
c[,Poor8:=ifelse(Total_Exp_Month_Per2 < Povertyline5_7 & cluster==5 ,1,Poor8)]
c[,weighted.mean(Poor8,Weight),by=cluster]
CBNPoor8<-CBN[Poor8==1]

#weighted consumption in each cluster
CBNPoor8<-CBNPoor8[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor8<-CBNPoor8[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor8<-CBNPoor8[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor8<-CBNPoor8[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor8<-CBNPoor8[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor8[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor8[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=188:203][] 
CBNPoor8[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor8[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor8)

# Food Calories
CBNPoor8$Ghand_Calory<- CBNPoor8$Ghandgram *4
CBNPoor8$Hoboobat_Calory<- CBNPoor8$Hoboobatgram *3
CBNPoor8$Nan_Calory<- CBNPoor8$Nangram *2.5
CBNPoor8$Berenj_Calory<- CBNPoor8$Berenjgram *1.2
CBNPoor8$Roghan_Calory<- CBNPoor8$Roghangram *8
CBNPoor8$Goosht_Calory<- CBNPoor8$Gooshtgram *2.5
CBNPoor8$Morgh_Calory<- CBNPoor8$Morghgram *2
CBNPoor8$Mahi_Calory<- CBNPoor8$Mahigram *1
CBNPoor8$Shir_Calory<- CBNPoor8$Shirgram *2.5
CBNPoor8$Mast_Calory<- CBNPoor8$Mastgram *1.5
CBNPoor8$Panir_Calory<- CBNPoor8$Mastgram *2.5
CBNPoor8$Tokhmemorgh_Calory<- CBNPoor8$Tokhmemorghgram *1.4
CBNPoor8$Mive_Calory<- CBNPoor8$Mivegram *0.5
CBNPoor8$Sabzi_Calory<- CBNPoor8$Sabzigram *0.5
CBNPoor8$Makarooni_Calory<- CBNPoor8$Makaroonigram *3.6
CBNPoor8$Sibzamini_Calory<- CBNPoor8$Sibzaminigram *0.9
#utils::View(CBNPoor8)

#CalculatePer_calories
CBNPoor8[, Daily_Calories := Reduce(`+`, .SD), .SDcols=205:220][] 
CBNPoor8[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor8[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor8 <- CBNPoor8[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor8[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor8[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor8$Ghand_per_Calory<- CBNPoor8$Ghandgram *4/CBNPoor8$EqSizeCalory
CBNPoor8$Hoboobat_per_Calory<- CBNPoor8$Hoboobatgram *3/CBNPoor8$EqSizeCalory
CBNPoor8$Nan_per_Calory<- CBNPoor8$Nangram *2.5/CBNPoor8$EqSizeCalory
CBNPoor8$Berenj_per_Calory<- CBNPoor8$Berenjgram *1.2/CBNPoor8$EqSizeCalory
CBNPoor8$Roghan_per_Calory<- CBNPoor8$Roghangram *8/CBNPoor8$EqSizeCalory
CBNPoor8$Goosht_per_Calory<- CBNPoor8$Gooshtgram *2.5/CBNPoor8$EqSizeCalory
CBNPoor8$Morgh_per_Calory<- CBNPoor8$Morghgram *2/CBNPoor8$EqSizeCalory
CBNPoor8$Mahi_per_Calory<- CBNPoor8$Mahigram *1/CBNPoor8$EqSizeCalory
CBNPoor8$Shir_per_Calory<- CBNPoor8$Shirgram *2.5/CBNPoor8$EqSizeCalory
CBNPoor8$Mast_per_Calory<- CBNPoor8$Mastgram *1.5/CBNPoor8$EqSizeCalory
CBNPoor8$Panir_per_Calory<- CBNPoor8$Mastgram *2.5/CBNPoor8$EqSizeCalory
CBNPoor8$Tokhmemorgh_per_Calory<- CBNPoor8$Tokhmemorghgram *1.4/CBNPoor8$EqSizeCalory
CBNPoor8$Mive_per_Calory<- CBNPoor8$Mivegram *0.5/CBNPoor8$EqSizeCalory
CBNPoor8$Sabzi_per_Calory<- CBNPoor8$Sabzigram *0.5/CBNPoor8$EqSizeCalory
CBNPoor8$Makarooni_per_Calory<- CBNPoor8$Makaroonigram *3.6/CBNPoor8$EqSizeCalory
CBNPoor8$Sibzamini_per_Calory<- CBNPoor8$Sibzaminigram *0.9/CBNPoor8$EqSizeCalory
#utils::View(CBNPoor8)

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
CBNPoor8[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(241:256)][] 
#utils::View(CBNPoor8)

CBNPoor8[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor8[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(258:273)][] 
#utils::View(CBNPoor8)

# Food grams from Calories2
CBNPoor8$Ghandgram2  <- CBNPoor8$Daily2_Ghand /4
CBNPoor8$Hoboobatgram2 <- CBNPoor8$Daily2_Hoboobat /3
CBNPoor8$Nangram2 <- CBNPoor8$Daily2_Nan  /2.5
CBNPoor8$Berenjgram2 <- CBNPoor8$Daily2_Berenj  /1.2
CBNPoor8$Roghangram2 <- CBNPoor8$Daily2_Roghan /8
CBNPoor8$Gooshtgram2 <- CBNPoor8$Daily2_Goosht  /2.5
CBNPoor8$Morghgram2 <- CBNPoor8$Daily2_Morgh  /2
CBNPoor8$Mahigram2 <- CBNPoor8$Daily2_Mahi  /1
CBNPoor8$Shirgram2 <- CBNPoor8$Daily2_Shir  /2.5
CBNPoor8$Mastgram2 <- CBNPoor8$Daily2_Mast  /1.5
CBNPoor8$Panirgram2 <- CBNPoor8$Daily2_Panir  /2.5
CBNPoor8$Tokhmemorghgram2 <- CBNPoor8$Daily2_Tokhmemorgh /1.4
CBNPoor8$Mivegram2 <- CBNPoor8$Daily2_Mive  /0.5
CBNPoor8$Sabzigram2 <- CBNPoor8$Daily2_Sabzi  /0.5
CBNPoor8$Makaroonigram2 <- CBNPoor8$Daily2_Makarooni  /3.6
CBNPoor8$Sibzaminigram2 <- CBNPoor8$Daily2_Sibzamini /0.9
#utils::View(CBNPoor8)

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


CBNPoor8<-CBNPoor8[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor8<-CBNPoor8[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor8<-CBNPoor8[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor8<-CBNPoor8[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor8<-CBNPoor8[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor8<-CBNPoor8[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor8<-CBNPoor8[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor8<-CBNPoor8[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor8<-CBNPoor8[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor8<-CBNPoor8[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor8<-CBNPoor8[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor8<-CBNPoor8[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor8<-CBNPoor8[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor8<-CBNPoor8[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor8<-CBNPoor8[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor8<-CBNPoor8[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor8<-CBNPoor8[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor8<-CBNPoor8[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor8[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor8[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(310:325)][] 
CBNPoor8[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor8)
CBNPoor8<-CBNPoor8[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor8<-CBNPoor8[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor8<-CBNPoor8[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8<-CBNPoor8[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor8[is.na(get(col)), (col) := 0]
CBNPoor8[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(330:345)][] 
CBNPoor8[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor8)
#order(CBNPoor8$AdditionalExpenditure_Per)

a<-CBNPoor8[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor8<-CBNPoor8[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor8)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor8,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor8[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_8<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor8,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor8[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_8<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor8,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor8[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_8<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor8,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor8[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_8<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor8,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor8<-CBNPoor8[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor8[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_8<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor8[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor8[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor8[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 9###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline1_8 & cluster==1,1,0)]
c[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline1_8 & cluster==1 ,1,0)]
CBN[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline2_8 & cluster==2,1,Poor9)]
c[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline2_8 & cluster==2 ,1,Poor9)]
CBN[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline3_8 & cluster==3,1,Poor9)]
c[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline3_8 & cluster==3 ,1,Poor9)]
CBN[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline4_8 & cluster==4,1,Poor9)]
c[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline4_8 & cluster==4 ,1,Poor9)]
CBN[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline5_8 & cluster==5,1,Poor9)]
c[,Poor9:=ifelse(Total_Exp_Month_Per2 < Povertyline5_8 & cluster==5 ,1,Poor9)]
c[,weighted.mean(Poor9,Weight),by=cluster]
CBNPoor9<-CBN[Poor9==1]

#weighted consumption in each cluster
CBNPoor9<-CBNPoor9[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor9<-CBNPoor9[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor9<-CBNPoor9[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor9<-CBNPoor9[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor9<-CBNPoor9[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor9[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor9[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=189:204][] 
CBNPoor9[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor9[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor9)

# Food Calories
CBNPoor9$Ghand_Calory<- CBNPoor9$Ghandgram *4
CBNPoor9$Hoboobat_Calory<- CBNPoor9$Hoboobatgram *3
CBNPoor9$Nan_Calory<- CBNPoor9$Nangram *2.5
CBNPoor9$Berenj_Calory<- CBNPoor9$Berenjgram *1.2
CBNPoor9$Roghan_Calory<- CBNPoor9$Roghangram *8
CBNPoor9$Goosht_Calory<- CBNPoor9$Gooshtgram *2.5
CBNPoor9$Morgh_Calory<- CBNPoor9$Morghgram *2
CBNPoor9$Mahi_Calory<- CBNPoor9$Mahigram *1
CBNPoor9$Shir_Calory<- CBNPoor9$Shirgram *2.5
CBNPoor9$Mast_Calory<- CBNPoor9$Mastgram *1.5
CBNPoor9$Panir_Calory<- CBNPoor9$Mastgram *2.5
CBNPoor9$Tokhmemorgh_Calory<- CBNPoor9$Tokhmemorghgram *1.4
CBNPoor9$Mive_Calory<- CBNPoor9$Mivegram *0.5
CBNPoor9$Sabzi_Calory<- CBNPoor9$Sabzigram *0.5
CBNPoor9$Makarooni_Calory<- CBNPoor9$Makaroonigram *3.6
CBNPoor9$Sibzamini_Calory<- CBNPoor9$Sibzaminigram *0.9
#utils::View(CBNPoor9)

#CalculatePer_calories
CBNPoor9[, Daily_Calories := Reduce(`+`, .SD), .SDcols=206:221][] 
CBNPoor9[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor9[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor9 <- CBNPoor9[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor9[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor9[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor9$Ghand_per_Calory<- CBNPoor9$Ghandgram *4/CBNPoor9$EqSizeCalory
CBNPoor9$Hoboobat_per_Calory<- CBNPoor9$Hoboobatgram *3/CBNPoor9$EqSizeCalory
CBNPoor9$Nan_per_Calory<- CBNPoor9$Nangram *2.5/CBNPoor9$EqSizeCalory
CBNPoor9$Berenj_per_Calory<- CBNPoor9$Berenjgram *1.2/CBNPoor9$EqSizeCalory
CBNPoor9$Roghan_per_Calory<- CBNPoor9$Roghangram *8/CBNPoor9$EqSizeCalory
CBNPoor9$Goosht_per_Calory<- CBNPoor9$Gooshtgram *2.5/CBNPoor9$EqSizeCalory
CBNPoor9$Morgh_per_Calory<- CBNPoor9$Morghgram *2/CBNPoor9$EqSizeCalory
CBNPoor9$Mahi_per_Calory<- CBNPoor9$Mahigram *1/CBNPoor9$EqSizeCalory
CBNPoor9$Shir_per_Calory<- CBNPoor9$Shirgram *2.5/CBNPoor9$EqSizeCalory
CBNPoor9$Mast_per_Calory<- CBNPoor9$Mastgram *1.5/CBNPoor9$EqSizeCalory
CBNPoor9$Panir_per_Calory<- CBNPoor9$Mastgram *2.5/CBNPoor9$EqSizeCalory
CBNPoor9$Tokhmemorgh_per_Calory<- CBNPoor9$Tokhmemorghgram *1.4/CBNPoor9$EqSizeCalory
CBNPoor9$Mive_per_Calory<- CBNPoor9$Mivegram *0.5/CBNPoor9$EqSizeCalory
CBNPoor9$Sabzi_per_Calory<- CBNPoor9$Sabzigram *0.5/CBNPoor9$EqSizeCalory
CBNPoor9$Makarooni_per_Calory<- CBNPoor9$Makaroonigram *3.6/CBNPoor9$EqSizeCalory
CBNPoor9$Sibzamini_per_Calory<- CBNPoor9$Sibzaminigram *0.9/CBNPoor9$EqSizeCalory
#utils::View(CBNPoor9)

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
CBNPoor9[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(242:257)][] 
#utils::View(CBNPoor9)

CBNPoor9[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor9[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(259:274)][] 
#utils::View(CBNPoor9)

# Food grams from Calories2
CBNPoor9$Ghandgram2  <- CBNPoor9$Daily2_Ghand /4
CBNPoor9$Hoboobatgram2 <- CBNPoor9$Daily2_Hoboobat /3
CBNPoor9$Nangram2 <- CBNPoor9$Daily2_Nan  /2.5
CBNPoor9$Berenjgram2 <- CBNPoor9$Daily2_Berenj  /1.2
CBNPoor9$Roghangram2 <- CBNPoor9$Daily2_Roghan /8
CBNPoor9$Gooshtgram2 <- CBNPoor9$Daily2_Goosht  /2.5
CBNPoor9$Morghgram2 <- CBNPoor9$Daily2_Morgh  /2
CBNPoor9$Mahigram2 <- CBNPoor9$Daily2_Mahi  /1
CBNPoor9$Shirgram2 <- CBNPoor9$Daily2_Shir  /2.5
CBNPoor9$Mastgram2 <- CBNPoor9$Daily2_Mast  /1.5
CBNPoor9$Panirgram2 <- CBNPoor9$Daily2_Panir  /2.5
CBNPoor9$Tokhmemorghgram2 <- CBNPoor9$Daily2_Tokhmemorgh /1.4
CBNPoor9$Mivegram2 <- CBNPoor9$Daily2_Mive  /0.5
CBNPoor9$Sabzigram2 <- CBNPoor9$Daily2_Sabzi  /0.5
CBNPoor9$Makaroonigram2 <- CBNPoor9$Daily2_Makarooni  /3.6
CBNPoor9$Sibzaminigram2 <- CBNPoor9$Daily2_Sibzamini /0.9
#utils::View(CBNPoor9)

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


CBNPoor9<-CBNPoor9[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor9<-CBNPoor9[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor9<-CBNPoor9[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor9<-CBNPoor9[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor9<-CBNPoor9[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor9<-CBNPoor9[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor9<-CBNPoor9[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor9<-CBNPoor9[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor9<-CBNPoor9[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor9<-CBNPoor9[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor9<-CBNPoor9[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor9<-CBNPoor9[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor9<-CBNPoor9[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor9<-CBNPoor9[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor9<-CBNPoor9[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor9<-CBNPoor9[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor9<-CBNPoor9[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor9<-CBNPoor9[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor9[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor9[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(311:326)][] 
CBNPoor9[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor9)
CBNPoor9<-CBNPoor9[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor9<-CBNPoor9[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor9<-CBNPoor9[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9<-CBNPoor9[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor9[is.na(get(col)), (col) := 0]
CBNPoor9[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(331:346)][] 
CBNPoor9[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor9)
#order(CBNPoor9$AdditionalExpenditure_Per)

a<-CBNPoor9[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor9<-CBNPoor9[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor9)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor9,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor9[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_9<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor9,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor9[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_9<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor9,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor9[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_9<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor9,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor9[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_9<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor9,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor9<-CBNPoor9[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor9[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_9<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor9[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor9[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor9[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 10###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline1_9 & cluster==1,1,0)]
c[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline1_9 & cluster==1 ,1,0)]
CBN[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline2_9 & cluster==2,1,Poor10)]
c[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline2_9 & cluster==2 ,1,Poor10)]
CBN[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline3_9 & cluster==3,1,Poor10)]
c[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline3_9 & cluster==3 ,1,Poor10)]
CBN[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline4_9 & cluster==4,1,Poor10)]
c[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline4_9 & cluster==4 ,1,Poor10)]
CBN[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline5_9 & cluster==5,1,Poor10)]
c[,Poor10:=ifelse(Total_Exp_Month_Per2 < Povertyline5_9 & cluster==5 ,1,Poor10)]
c[,weighted.mean(Poor10,Weight),by=cluster]
CBNPoor10<-CBN[Poor10==1]

#weighted consumption in each cluster
CBNPoor10<-CBNPoor10[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor10<-CBNPoor10[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor10<-CBNPoor10[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor10<-CBNPoor10[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor10<-CBNPoor10[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor10[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor10[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=190:205][] 
CBNPoor10[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor10[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor10)

# Food Calories
CBNPoor10$Ghand_Calory<- CBNPoor10$Ghandgram *4
CBNPoor10$Hoboobat_Calory<- CBNPoor10$Hoboobatgram *3
CBNPoor10$Nan_Calory<- CBNPoor10$Nangram *2.5
CBNPoor10$Berenj_Calory<- CBNPoor10$Berenjgram *1.2
CBNPoor10$Roghan_Calory<- CBNPoor10$Roghangram *8
CBNPoor10$Goosht_Calory<- CBNPoor10$Gooshtgram *2.5
CBNPoor10$Morgh_Calory<- CBNPoor10$Morghgram *2
CBNPoor10$Mahi_Calory<- CBNPoor10$Mahigram *1
CBNPoor10$Shir_Calory<- CBNPoor10$Shirgram *2.5
CBNPoor10$Mast_Calory<- CBNPoor10$Mastgram *1.5
CBNPoor10$Panir_Calory<- CBNPoor10$Mastgram *2.5
CBNPoor10$Tokhmemorgh_Calory<- CBNPoor10$Tokhmemorghgram *1.4
CBNPoor10$Mive_Calory<- CBNPoor10$Mivegram *0.5
CBNPoor10$Sabzi_Calory<- CBNPoor10$Sabzigram *0.5
CBNPoor10$Makarooni_Calory<- CBNPoor10$Makaroonigram *3.6
CBNPoor10$Sibzamini_Calory<- CBNPoor10$Sibzaminigram *0.9
#utils::View(CBNPoor10)

#CalculatePer_calories
CBNPoor10[, Daily_Calories := Reduce(`+`, .SD), .SDcols=207:222][] 
CBNPoor10[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor10[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor10 <- CBNPoor10[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor10[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor10$Ghand_per_Calory<- CBNPoor10$Ghandgram *4/CBNPoor10$EqSizeCalory
CBNPoor10$Hoboobat_per_Calory<- CBNPoor10$Hoboobatgram *3/CBNPoor10$EqSizeCalory
CBNPoor10$Nan_per_Calory<- CBNPoor10$Nangram *2.5/CBNPoor10$EqSizeCalory
CBNPoor10$Berenj_per_Calory<- CBNPoor10$Berenjgram *1.2/CBNPoor10$EqSizeCalory
CBNPoor10$Roghan_per_Calory<- CBNPoor10$Roghangram *8/CBNPoor10$EqSizeCalory
CBNPoor10$Goosht_per_Calory<- CBNPoor10$Gooshtgram *2.5/CBNPoor10$EqSizeCalory
CBNPoor10$Morgh_per_Calory<- CBNPoor10$Morghgram *2/CBNPoor10$EqSizeCalory
CBNPoor10$Mahi_per_Calory<- CBNPoor10$Mahigram *1/CBNPoor10$EqSizeCalory
CBNPoor10$Shir_per_Calory<- CBNPoor10$Shirgram *2.5/CBNPoor10$EqSizeCalory
CBNPoor10$Mast_per_Calory<- CBNPoor10$Mastgram *1.5/CBNPoor10$EqSizeCalory
CBNPoor10$Panir_per_Calory<- CBNPoor10$Mastgram *2.5/CBNPoor10$EqSizeCalory
CBNPoor10$Tokhmemorgh_per_Calory<- CBNPoor10$Tokhmemorghgram *1.4/CBNPoor10$EqSizeCalory
CBNPoor10$Mive_per_Calory<- CBNPoor10$Mivegram *0.5/CBNPoor10$EqSizeCalory
CBNPoor10$Sabzi_per_Calory<- CBNPoor10$Sabzigram *0.5/CBNPoor10$EqSizeCalory
CBNPoor10$Makarooni_per_Calory<- CBNPoor10$Makaroonigram *3.6/CBNPoor10$EqSizeCalory
CBNPoor10$Sibzamini_per_Calory<- CBNPoor10$Sibzaminigram *0.9/CBNPoor10$EqSizeCalory
#utils::View(CBNPoor10)

#CalculatePer_calories in clusters
CBNPoor10[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor10[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(243:258)][] 
#utils::View(CBNPoor10)

CBNPoor10[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor10[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(260:275)][] 
#utils::View(CBNPoor10)

# Food grams from Calories2
CBNPoor10$Ghandgram2  <- CBNPoor10$Daily2_Ghand /4
CBNPoor10$Hoboobatgram2 <- CBNPoor10$Daily2_Hoboobat /3
CBNPoor10$Nangram2 <- CBNPoor10$Daily2_Nan  /2.5
CBNPoor10$Berenjgram2 <- CBNPoor10$Daily2_Berenj  /1.2
CBNPoor10$Roghangram2 <- CBNPoor10$Daily2_Roghan /8
CBNPoor10$Gooshtgram2 <- CBNPoor10$Daily2_Goosht  /2.5
CBNPoor10$Morghgram2 <- CBNPoor10$Daily2_Morgh  /2
CBNPoor10$Mahigram2 <- CBNPoor10$Daily2_Mahi  /1
CBNPoor10$Shirgram2 <- CBNPoor10$Daily2_Shir  /2.5
CBNPoor10$Mastgram2 <- CBNPoor10$Daily2_Mast  /1.5
CBNPoor10$Panirgram2 <- CBNPoor10$Daily2_Panir  /2.5
CBNPoor10$Tokhmemorghgram2 <- CBNPoor10$Daily2_Tokhmemorgh /1.4
CBNPoor10$Mivegram2 <- CBNPoor10$Daily2_Mive  /0.5
CBNPoor10$Sabzigram2 <- CBNPoor10$Daily2_Sabzi  /0.5
CBNPoor10$Makaroonigram2 <- CBNPoor10$Daily2_Makarooni  /3.6
CBNPoor10$Sibzaminigram2 <- CBNPoor10$Daily2_Sibzamini /0.9
#utils::View(CBNPoor10)

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


CBNPoor10<-CBNPoor10[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor10<-CBNPoor10[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor10<-CBNPoor10[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor10<-CBNPoor10[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor10<-CBNPoor10[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor10<-CBNPoor10[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor10<-CBNPoor10[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor10<-CBNPoor10[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor10<-CBNPoor10[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor10<-CBNPoor10[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor10<-CBNPoor10[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor10<-CBNPoor10[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor10<-CBNPoor10[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor10<-CBNPoor10[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor10<-CBNPoor10[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor10<-CBNPoor10[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor10<-CBNPoor10[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor10<-CBNPoor10[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor10[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor10[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(312:327)][] 
CBNPoor10[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor10)
CBNPoor10<-CBNPoor10[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor10<-CBNPoor10[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor10<-CBNPoor10[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10<-CBNPoor10[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor10[is.na(get(col)), (col) := 0]
CBNPoor10[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(332:347)][] 
CBNPoor10[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor10)
#order(CBNPoor10$AdditionalExpenditure_Per)

a<-CBNPoor10[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor10<-CBNPoor10[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor10)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor10,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor10[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_10<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor10,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor10[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_10<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor10,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor10[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_10<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor10,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor10[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_10<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor10,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor10<-CBNPoor10[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor10[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_10<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor10[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor10[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor10[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 11###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline1_10 & cluster==1,1,0)]
c[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline1_10 & cluster==1 ,1,0)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline2_10 & cluster==2,1,Poor11)]
c[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline2_10 & cluster==2 ,1,Poor11)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline3_10 & cluster==3,1,Poor11)]
c[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline3_10 & cluster==3 ,1,Poor11)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline4_10 & cluster==4,1,Poor11)]
c[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline4_10 & cluster==4 ,1,Poor11)]
CBN[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline5_10 & cluster==5,1,Poor11)]
c[,Poor11:=ifelse(Total_Exp_Month_Per2 < Povertyline5_10 & cluster==5 ,1,Poor11)]
c[,weighted.mean(Poor11,Weight),by=cluster]
CBNPoor11<-CBN[Poor11==1]

#weighted consumption in each cluster
CBNPoor11<-CBNPoor11[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor11<-CBNPoor11[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor11<-CBNPoor11[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor11<-CBNPoor11[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor11<-CBNPoor11[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor11[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor11[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=191:206][] 
CBNPoor11[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor11[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor11)

# Food Calories
CBNPoor11$Ghand_Calory<- CBNPoor11$Ghandgram *4
CBNPoor11$Hoboobat_Calory<- CBNPoor11$Hoboobatgram *3
CBNPoor11$Nan_Calory<- CBNPoor11$Nangram *2.5
CBNPoor11$Berenj_Calory<- CBNPoor11$Berenjgram *1.2
CBNPoor11$Roghan_Calory<- CBNPoor11$Roghangram *8
CBNPoor11$Goosht_Calory<- CBNPoor11$Gooshtgram *2.5
CBNPoor11$Morgh_Calory<- CBNPoor11$Morghgram *2
CBNPoor11$Mahi_Calory<- CBNPoor11$Mahigram *1
CBNPoor11$Shir_Calory<- CBNPoor11$Shirgram *2.5
CBNPoor11$Mast_Calory<- CBNPoor11$Mastgram *1.5
CBNPoor11$Panir_Calory<- CBNPoor11$Mastgram *2.5
CBNPoor11$Tokhmemorgh_Calory<- CBNPoor11$Tokhmemorghgram *1.4
CBNPoor11$Mive_Calory<- CBNPoor11$Mivegram *0.5
CBNPoor11$Sabzi_Calory<- CBNPoor11$Sabzigram *0.5
CBNPoor11$Makarooni_Calory<- CBNPoor11$Makaroonigram *3.6
CBNPoor11$Sibzamini_Calory<- CBNPoor11$Sibzaminigram *0.9
#utils::View(CBNPoor11)

#CalculatePer_calories
CBNPoor11[, Daily_Calories := Reduce(`+`, .SD), .SDcols=208:223][] 
CBNPoor11[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor11[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor11 <- CBNPoor11[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor11[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor11$Ghand_per_Calory<- CBNPoor11$Ghandgram *4/CBNPoor11$EqSizeCalory
CBNPoor11$Hoboobat_per_Calory<- CBNPoor11$Hoboobatgram *3/CBNPoor11$EqSizeCalory
CBNPoor11$Nan_per_Calory<- CBNPoor11$Nangram *2.5/CBNPoor11$EqSizeCalory
CBNPoor11$Berenj_per_Calory<- CBNPoor11$Berenjgram *1.2/CBNPoor11$EqSizeCalory
CBNPoor11$Roghan_per_Calory<- CBNPoor11$Roghangram *8/CBNPoor11$EqSizeCalory
CBNPoor11$Goosht_per_Calory<- CBNPoor11$Gooshtgram *2.5/CBNPoor11$EqSizeCalory
CBNPoor11$Morgh_per_Calory<- CBNPoor11$Morghgram *2/CBNPoor11$EqSizeCalory
CBNPoor11$Mahi_per_Calory<- CBNPoor11$Mahigram *1/CBNPoor11$EqSizeCalory
CBNPoor11$Shir_per_Calory<- CBNPoor11$Shirgram *2.5/CBNPoor11$EqSizeCalory
CBNPoor11$Mast_per_Calory<- CBNPoor11$Mastgram *1.5/CBNPoor11$EqSizeCalory
CBNPoor11$Panir_per_Calory<- CBNPoor11$Mastgram *2.5/CBNPoor11$EqSizeCalory
CBNPoor11$Tokhmemorgh_per_Calory<- CBNPoor11$Tokhmemorghgram *1.4/CBNPoor11$EqSizeCalory
CBNPoor11$Mive_per_Calory<- CBNPoor11$Mivegram *0.5/CBNPoor11$EqSizeCalory
CBNPoor11$Sabzi_per_Calory<- CBNPoor11$Sabzigram *0.5/CBNPoor11$EqSizeCalory
CBNPoor11$Makarooni_per_Calory<- CBNPoor11$Makaroonigram *3.6/CBNPoor11$EqSizeCalory
CBNPoor11$Sibzamini_per_Calory<- CBNPoor11$Sibzaminigram *0.9/CBNPoor11$EqSizeCalory
#utils::View(CBNPoor11)

#CalculatePer_calories in clusters
CBNPoor11[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor11[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(244:259)][] 
#utils::View(CBNPoor11)

CBNPoor11[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor11[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(261:276)][] 
#utils::View(CBNPoor11)

# Food grams from Calories2
CBNPoor11$Ghandgram2  <- CBNPoor11$Daily2_Ghand /4
CBNPoor11$Hoboobatgram2 <- CBNPoor11$Daily2_Hoboobat /3
CBNPoor11$Nangram2 <- CBNPoor11$Daily2_Nan  /2.5
CBNPoor11$Berenjgram2 <- CBNPoor11$Daily2_Berenj  /1.2
CBNPoor11$Roghangram2 <- CBNPoor11$Daily2_Roghan /8
CBNPoor11$Gooshtgram2 <- CBNPoor11$Daily2_Goosht  /2.5
CBNPoor11$Morghgram2 <- CBNPoor11$Daily2_Morgh  /2
CBNPoor11$Mahigram2 <- CBNPoor11$Daily2_Mahi  /1
CBNPoor11$Shirgram2 <- CBNPoor11$Daily2_Shir  /2.5
CBNPoor11$Mastgram2 <- CBNPoor11$Daily2_Mast  /1.5
CBNPoor11$Panirgram2 <- CBNPoor11$Daily2_Panir  /2.5
CBNPoor11$Tokhmemorghgram2 <- CBNPoor11$Daily2_Tokhmemorgh /1.4
CBNPoor11$Mivegram2 <- CBNPoor11$Daily2_Mive  /0.5
CBNPoor11$Sabzigram2 <- CBNPoor11$Daily2_Sabzi  /0.5
CBNPoor11$Makaroonigram2 <- CBNPoor11$Daily2_Makarooni  /3.6
CBNPoor11$Sibzaminigram2 <- CBNPoor11$Daily2_Sibzamini /0.9
#utils::View(CBNPoor11)

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


CBNPoor11<-CBNPoor11[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor11<-CBNPoor11[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor11<-CBNPoor11[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor11<-CBNPoor11[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor11<-CBNPoor11[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor11<-CBNPoor11[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor11<-CBNPoor11[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor11<-CBNPoor11[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor11<-CBNPoor11[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor11<-CBNPoor11[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor11<-CBNPoor11[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor11<-CBNPoor11[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor11<-CBNPoor11[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor11<-CBNPoor11[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor11<-CBNPoor11[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor11<-CBNPoor11[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor11<-CBNPoor11[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor11<-CBNPoor11[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor11[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor11[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(313:328)][] 
CBNPoor11[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor11)
CBNPoor11<-CBNPoor11[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor11<-CBNPoor11[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor11<-CBNPoor11[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11<-CBNPoor11[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor11[is.na(get(col)), (col) := 0]
CBNPoor11[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(333:348)][] 
CBNPoor11[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor11)
#order(CBNPoor11$AdditionalExpenditure_Per)

a<-CBNPoor11[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor11<-CBNPoor11[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor11)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor11,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor11[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_11<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor11,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor11[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_11<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor11,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor11[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_11<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor11,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor11[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_11<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor11,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor11<-CBNPoor11[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor11[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_11<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor11[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor11[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor11[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 12###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline1_11 & cluster==1,1,0)]
c[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline1_11 & cluster==1 ,1,0)]
CBN[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline2_11 & cluster==2,1,Poor12)]
c[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline2_11 & cluster==2 ,1,Poor12)]
CBN[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline3_11 & cluster==3,1,Poor12)]
c[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline3_11 & cluster==3 ,1,Poor12)]
CBN[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline4_11 & cluster==4,1,Poor12)]
c[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline4_11 & cluster==4 ,1,Poor12)]
CBN[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline5_11 & cluster==5,1,Poor12)]
c[,Poor12:=ifelse(Total_Exp_Month_Per2 < Povertyline5_11 & cluster==5 ,1,Poor12)]
c[,weighted.mean(Poor12,Weight),by=cluster]
CBNPoor12<-CBN[Poor12==1]

#weighted consumption in each cluster
CBNPoor12<-CBNPoor12[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor12<-CBNPoor12[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor12<-CBNPoor12[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor12<-CBNPoor12[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor12<-CBNPoor12[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor12[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor12[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=192:207][] 
CBNPoor12[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor12[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor12)

# Food Calories
CBNPoor12$Ghand_Calory<- CBNPoor12$Ghandgram *4
CBNPoor12$Hoboobat_Calory<- CBNPoor12$Hoboobatgram *3
CBNPoor12$Nan_Calory<- CBNPoor12$Nangram *2.5
CBNPoor12$Berenj_Calory<- CBNPoor12$Berenjgram *1.2
CBNPoor12$Roghan_Calory<- CBNPoor12$Roghangram *8
CBNPoor12$Goosht_Calory<- CBNPoor12$Gooshtgram *2.5
CBNPoor12$Morgh_Calory<- CBNPoor12$Morghgram *2
CBNPoor12$Mahi_Calory<- CBNPoor12$Mahigram *1
CBNPoor12$Shir_Calory<- CBNPoor12$Shirgram *2.5
CBNPoor12$Mast_Calory<- CBNPoor12$Mastgram *1.5
CBNPoor12$Panir_Calory<- CBNPoor12$Mastgram *2.5
CBNPoor12$Tokhmemorgh_Calory<- CBNPoor12$Tokhmemorghgram *1.4
CBNPoor12$Mive_Calory<- CBNPoor12$Mivegram *0.5
CBNPoor12$Sabzi_Calory<- CBNPoor12$Sabzigram *0.5
CBNPoor12$Makarooni_Calory<- CBNPoor12$Makaroonigram *3.6
CBNPoor12$Sibzamini_Calory<- CBNPoor12$Sibzaminigram *0.9
#utils::View(CBNPoor12)

#CalculatePer_calories
CBNPoor12[, Daily_Calories := Reduce(`+`, .SD), .SDcols=209:224][] 
CBNPoor12[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor12[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor12 <- CBNPoor12[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor12[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor12$Ghand_per_Calory<- CBNPoor12$Ghandgram *4/CBNPoor12$EqSizeCalory
CBNPoor12$Hoboobat_per_Calory<- CBNPoor12$Hoboobatgram *3/CBNPoor12$EqSizeCalory
CBNPoor12$Nan_per_Calory<- CBNPoor12$Nangram *2.5/CBNPoor12$EqSizeCalory
CBNPoor12$Berenj_per_Calory<- CBNPoor12$Berenjgram *1.2/CBNPoor12$EqSizeCalory
CBNPoor12$Roghan_per_Calory<- CBNPoor12$Roghangram *8/CBNPoor12$EqSizeCalory
CBNPoor12$Goosht_per_Calory<- CBNPoor12$Gooshtgram *2.5/CBNPoor12$EqSizeCalory
CBNPoor12$Morgh_per_Calory<- CBNPoor12$Morghgram *2/CBNPoor12$EqSizeCalory
CBNPoor12$Mahi_per_Calory<- CBNPoor12$Mahigram *1/CBNPoor12$EqSizeCalory
CBNPoor12$Shir_per_Calory<- CBNPoor12$Shirgram *2.5/CBNPoor12$EqSizeCalory
CBNPoor12$Mast_per_Calory<- CBNPoor12$Mastgram *1.5/CBNPoor12$EqSizeCalory
CBNPoor12$Panir_per_Calory<- CBNPoor12$Mastgram *2.5/CBNPoor12$EqSizeCalory
CBNPoor12$Tokhmemorgh_per_Calory<- CBNPoor12$Tokhmemorghgram *1.4/CBNPoor12$EqSizeCalory
CBNPoor12$Mive_per_Calory<- CBNPoor12$Mivegram *0.5/CBNPoor12$EqSizeCalory
CBNPoor12$Sabzi_per_Calory<- CBNPoor12$Sabzigram *0.5/CBNPoor12$EqSizeCalory
CBNPoor12$Makarooni_per_Calory<- CBNPoor12$Makaroonigram *3.6/CBNPoor12$EqSizeCalory
CBNPoor12$Sibzamini_per_Calory<- CBNPoor12$Sibzaminigram *0.9/CBNPoor12$EqSizeCalory
#utils::View(CBNPoor12)

#CalculatePer_calories in clusters
CBNPoor12[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor12[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(245:260)][] 
#utils::View(CBNPoor12)

CBNPoor12[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor12[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(262:277)][] 
#utils::View(CBNPoor12)

# Food grams from Calories2
CBNPoor12$Ghandgram2  <- CBNPoor12$Daily2_Ghand /4
CBNPoor12$Hoboobatgram2 <- CBNPoor12$Daily2_Hoboobat /3
CBNPoor12$Nangram2 <- CBNPoor12$Daily2_Nan  /2.5
CBNPoor12$Berenjgram2 <- CBNPoor12$Daily2_Berenj  /1.2
CBNPoor12$Roghangram2 <- CBNPoor12$Daily2_Roghan /8
CBNPoor12$Gooshtgram2 <- CBNPoor12$Daily2_Goosht  /2.5
CBNPoor12$Morghgram2 <- CBNPoor12$Daily2_Morgh  /2
CBNPoor12$Mahigram2 <- CBNPoor12$Daily2_Mahi  /1
CBNPoor12$Shirgram2 <- CBNPoor12$Daily2_Shir  /2.5
CBNPoor12$Mastgram2 <- CBNPoor12$Daily2_Mast  /1.5
CBNPoor12$Panirgram2 <- CBNPoor12$Daily2_Panir  /2.5
CBNPoor12$Tokhmemorghgram2 <- CBNPoor12$Daily2_Tokhmemorgh /1.4
CBNPoor12$Mivegram2 <- CBNPoor12$Daily2_Mive  /0.5
CBNPoor12$Sabzigram2 <- CBNPoor12$Daily2_Sabzi  /0.5
CBNPoor12$Makaroonigram2 <- CBNPoor12$Daily2_Makarooni  /3.6
CBNPoor12$Sibzaminigram2 <- CBNPoor12$Daily2_Sibzamini /0.9
#utils::View(CBNPoor12)

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


CBNPoor12<-CBNPoor12[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor12<-CBNPoor12[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor12<-CBNPoor12[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor12<-CBNPoor12[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor12<-CBNPoor12[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor12<-CBNPoor12[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor12<-CBNPoor12[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor12<-CBNPoor12[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor12<-CBNPoor12[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor12<-CBNPoor12[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor12<-CBNPoor12[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor12<-CBNPoor12[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor12<-CBNPoor12[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor12<-CBNPoor12[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor12<-CBNPoor12[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor12<-CBNPoor12[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor12<-CBNPoor12[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor12<-CBNPoor12[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor12[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor12[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(314:329)][] 
CBNPoor12[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor12)
CBNPoor12<-CBNPoor12[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor12<-CBNPoor12[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor12<-CBNPoor12[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12<-CBNPoor12[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor12[is.na(get(col)), (col) := 0]
CBNPoor12[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(334:349)][] 
CBNPoor12[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor12)
#order(CBNPoor12$AdditionalExpenditure_Per)

a<-CBNPoor12[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor12<-CBNPoor12[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor12)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor12,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor12[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_12<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor12,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor12[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_12<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor12,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor12[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_12<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor12,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor12[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_12<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor12,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor12<-CBNPoor12[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor12[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_12<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor12[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor12[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor12[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 13###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline1_12 & cluster==1,1,0)]
c[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline1_12 & cluster==1 ,1,0)]
CBN[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline2_12 & cluster==2,1,Poor13)]
c[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline2_12 & cluster==2 ,1,Poor13)]
CBN[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline3_12 & cluster==3,1,Poor13)]
c[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline3_12 & cluster==3 ,1,Poor13)]
CBN[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline4_12 & cluster==4,1,Poor13)]
c[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline4_12 & cluster==4 ,1,Poor13)]
CBN[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline5_12 & cluster==5,1,Poor13)]
c[,Poor13:=ifelse(Total_Exp_Month_Per2 < Povertyline5_12 & cluster==5 ,1,Poor13)]
c[,weighted.mean(Poor13,Weight),by=cluster]
CBNPoor13<-CBN[Poor13==1]

#weighted consumption in each cluster
CBNPoor13<-CBNPoor13[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor13<-CBNPoor13[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor13<-CBNPoor13[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor13<-CBNPoor13[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor13<-CBNPoor13[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor13[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor13[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=193:208][] 
CBNPoor13[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor13[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor13)

# Food Calories
CBNPoor13$Ghand_Calory<- CBNPoor13$Ghandgram *4
CBNPoor13$Hoboobat_Calory<- CBNPoor13$Hoboobatgram *3
CBNPoor13$Nan_Calory<- CBNPoor13$Nangram *2.5
CBNPoor13$Berenj_Calory<- CBNPoor13$Berenjgram *1.2
CBNPoor13$Roghan_Calory<- CBNPoor13$Roghangram *8
CBNPoor13$Goosht_Calory<- CBNPoor13$Gooshtgram *2.5
CBNPoor13$Morgh_Calory<- CBNPoor13$Morghgram *2
CBNPoor13$Mahi_Calory<- CBNPoor13$Mahigram *1
CBNPoor13$Shir_Calory<- CBNPoor13$Shirgram *2.5
CBNPoor13$Mast_Calory<- CBNPoor13$Mastgram *1.5
CBNPoor13$Panir_Calory<- CBNPoor13$Mastgram *2.5
CBNPoor13$Tokhmemorgh_Calory<- CBNPoor13$Tokhmemorghgram *1.4
CBNPoor13$Mive_Calory<- CBNPoor13$Mivegram *0.5
CBNPoor13$Sabzi_Calory<- CBNPoor13$Sabzigram *0.5
CBNPoor13$Makarooni_Calory<- CBNPoor13$Makaroonigram *3.6
CBNPoor13$Sibzamini_Calory<- CBNPoor13$Sibzaminigram *0.9
#utils::View(CBNPoor13)

#CalculatePer_calories
CBNPoor13[, Daily_Calories := Reduce(`+`, .SD), .SDcols=210:225][] 
CBNPoor13[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor13[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor13 <- CBNPoor13[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor13[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor13$Ghand_per_Calory<- CBNPoor13$Ghandgram *4/CBNPoor13$EqSizeCalory
CBNPoor13$Hoboobat_per_Calory<- CBNPoor13$Hoboobatgram *3/CBNPoor13$EqSizeCalory
CBNPoor13$Nan_per_Calory<- CBNPoor13$Nangram *2.5/CBNPoor13$EqSizeCalory
CBNPoor13$Berenj_per_Calory<- CBNPoor13$Berenjgram *1.2/CBNPoor13$EqSizeCalory
CBNPoor13$Roghan_per_Calory<- CBNPoor13$Roghangram *8/CBNPoor13$EqSizeCalory
CBNPoor13$Goosht_per_Calory<- CBNPoor13$Gooshtgram *2.5/CBNPoor13$EqSizeCalory
CBNPoor13$Morgh_per_Calory<- CBNPoor13$Morghgram *2/CBNPoor13$EqSizeCalory
CBNPoor13$Mahi_per_Calory<- CBNPoor13$Mahigram *1/CBNPoor13$EqSizeCalory
CBNPoor13$Shir_per_Calory<- CBNPoor13$Shirgram *2.5/CBNPoor13$EqSizeCalory
CBNPoor13$Mast_per_Calory<- CBNPoor13$Mastgram *1.5/CBNPoor13$EqSizeCalory
CBNPoor13$Panir_per_Calory<- CBNPoor13$Mastgram *2.5/CBNPoor13$EqSizeCalory
CBNPoor13$Tokhmemorgh_per_Calory<- CBNPoor13$Tokhmemorghgram *1.4/CBNPoor13$EqSizeCalory
CBNPoor13$Mive_per_Calory<- CBNPoor13$Mivegram *0.5/CBNPoor13$EqSizeCalory
CBNPoor13$Sabzi_per_Calory<- CBNPoor13$Sabzigram *0.5/CBNPoor13$EqSizeCalory
CBNPoor13$Makarooni_per_Calory<- CBNPoor13$Makaroonigram *3.6/CBNPoor13$EqSizeCalory
CBNPoor13$Sibzamini_per_Calory<- CBNPoor13$Sibzaminigram *0.9/CBNPoor13$EqSizeCalory
#utils::View(CBNPoor13)

#CalculatePer_calories in clusters
CBNPoor13[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor13[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(246:261)][] 
#utils::View(CBNPoor13)

CBNPoor13[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor13[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(263:278)][] 
#utils::View(CBNPoor13)

# Food grams from Calories2
CBNPoor13$Ghandgram2  <- CBNPoor13$Daily2_Ghand /4
CBNPoor13$Hoboobatgram2 <- CBNPoor13$Daily2_Hoboobat /3
CBNPoor13$Nangram2 <- CBNPoor13$Daily2_Nan  /2.5
CBNPoor13$Berenjgram2 <- CBNPoor13$Daily2_Berenj  /1.2
CBNPoor13$Roghangram2 <- CBNPoor13$Daily2_Roghan /8
CBNPoor13$Gooshtgram2 <- CBNPoor13$Daily2_Goosht  /2.5
CBNPoor13$Morghgram2 <- CBNPoor13$Daily2_Morgh  /2
CBNPoor13$Mahigram2 <- CBNPoor13$Daily2_Mahi  /1
CBNPoor13$Shirgram2 <- CBNPoor13$Daily2_Shir  /2.5
CBNPoor13$Mastgram2 <- CBNPoor13$Daily2_Mast  /1.5
CBNPoor13$Panirgram2 <- CBNPoor13$Daily2_Panir  /2.5
CBNPoor13$Tokhmemorghgram2 <- CBNPoor13$Daily2_Tokhmemorgh /1.4
CBNPoor13$Mivegram2 <- CBNPoor13$Daily2_Mive  /0.5
CBNPoor13$Sabzigram2 <- CBNPoor13$Daily2_Sabzi  /0.5
CBNPoor13$Makaroonigram2 <- CBNPoor13$Daily2_Makarooni  /3.6
CBNPoor13$Sibzaminigram2 <- CBNPoor13$Daily2_Sibzamini /0.9
#utils::View(CBNPoor13)

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


CBNPoor13<-CBNPoor13[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor13<-CBNPoor13[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor13<-CBNPoor13[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor13<-CBNPoor13[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor13<-CBNPoor13[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor13<-CBNPoor13[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor13<-CBNPoor13[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor13<-CBNPoor13[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor13<-CBNPoor13[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor13<-CBNPoor13[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor13<-CBNPoor13[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor13<-CBNPoor13[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor13<-CBNPoor13[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor13<-CBNPoor13[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor13<-CBNPoor13[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor13<-CBNPoor13[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor13<-CBNPoor13[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor13<-CBNPoor13[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor13[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor13[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(315:330)][] 
CBNPoor13[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor13)
CBNPoor13<-CBNPoor13[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor13<-CBNPoor13[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor13<-CBNPoor13[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13<-CBNPoor13[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor13[is.na(get(col)), (col) := 0]
CBNPoor13[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(335:350)][] 
CBNPoor13[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor13)
#order(CBNPoor13$AdditionalExpenditure_Per)

a<-CBNPoor13[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor13<-CBNPoor13[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor13)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor13,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor13[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_13<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor13,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor13[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_13<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor13,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor13[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_13<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor13,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor13[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_13<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor13,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor13<-CBNPoor13[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor13[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_13<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor13[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor13[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor13[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 14###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline1_13 & cluster==1,1,0)]
c[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline1_13 & cluster==1 ,1,0)]
CBN[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline2_13 & cluster==2,1,Poor14)]
c[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline2_13 & cluster==2 ,1,Poor14)]
CBN[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline3_13 & cluster==3,1,Poor14)]
c[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline3_13 & cluster==3 ,1,Poor14)]
CBN[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline4_13 & cluster==4,1,Poor14)]
c[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline4_13 & cluster==4 ,1,Poor14)]
CBN[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline5_13 & cluster==5,1,Poor14)]
c[,Poor14:=ifelse(Total_Exp_Month_Per2 < Povertyline5_13 & cluster==5 ,1,Poor14)]
c[,weighted.mean(Poor14,Weight),by=cluster]
CBNPoor14<-CBN[Poor14==1]

#weighted consumption in each cluster
CBNPoor14<-CBNPoor14[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor14<-CBNPoor14[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor14<-CBNPoor14[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor14<-CBNPoor14[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor14<-CBNPoor14[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor14[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor14[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=194:209][] 
CBNPoor14[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor14[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor14)

# Food Calories
CBNPoor14$Ghand_Calory<- CBNPoor14$Ghandgram *4
CBNPoor14$Hoboobat_Calory<- CBNPoor14$Hoboobatgram *3
CBNPoor14$Nan_Calory<- CBNPoor14$Nangram *2.5
CBNPoor14$Berenj_Calory<- CBNPoor14$Berenjgram *1.2
CBNPoor14$Roghan_Calory<- CBNPoor14$Roghangram *8
CBNPoor14$Goosht_Calory<- CBNPoor14$Gooshtgram *2.5
CBNPoor14$Morgh_Calory<- CBNPoor14$Morghgram *2
CBNPoor14$Mahi_Calory<- CBNPoor14$Mahigram *1
CBNPoor14$Shir_Calory<- CBNPoor14$Shirgram *2.5
CBNPoor14$Mast_Calory<- CBNPoor14$Mastgram *1.5
CBNPoor14$Panir_Calory<- CBNPoor14$Mastgram *2.5
CBNPoor14$Tokhmemorgh_Calory<- CBNPoor14$Tokhmemorghgram *1.4
CBNPoor14$Mive_Calory<- CBNPoor14$Mivegram *0.5
CBNPoor14$Sabzi_Calory<- CBNPoor14$Sabzigram *0.5
CBNPoor14$Makarooni_Calory<- CBNPoor14$Makaroonigram *3.6
CBNPoor14$Sibzamini_Calory<- CBNPoor14$Sibzaminigram *0.9
#utils::View(CBNPoor14)

#CalculatePer_calories
CBNPoor14[, Daily_Calories := Reduce(`+`, .SD), .SDcols=211:226][] 
CBNPoor14[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor14[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor14 <- CBNPoor14[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor14[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor14$Ghand_per_Calory<- CBNPoor14$Ghandgram *4/CBNPoor14$EqSizeCalory
CBNPoor14$Hoboobat_per_Calory<- CBNPoor14$Hoboobatgram *3/CBNPoor14$EqSizeCalory
CBNPoor14$Nan_per_Calory<- CBNPoor14$Nangram *2.5/CBNPoor14$EqSizeCalory
CBNPoor14$Berenj_per_Calory<- CBNPoor14$Berenjgram *1.2/CBNPoor14$EqSizeCalory
CBNPoor14$Roghan_per_Calory<- CBNPoor14$Roghangram *8/CBNPoor14$EqSizeCalory
CBNPoor14$Goosht_per_Calory<- CBNPoor14$Gooshtgram *2.5/CBNPoor14$EqSizeCalory
CBNPoor14$Morgh_per_Calory<- CBNPoor14$Morghgram *2/CBNPoor14$EqSizeCalory
CBNPoor14$Mahi_per_Calory<- CBNPoor14$Mahigram *1/CBNPoor14$EqSizeCalory
CBNPoor14$Shir_per_Calory<- CBNPoor14$Shirgram *2.5/CBNPoor14$EqSizeCalory
CBNPoor14$Mast_per_Calory<- CBNPoor14$Mastgram *1.5/CBNPoor14$EqSizeCalory
CBNPoor14$Panir_per_Calory<- CBNPoor14$Mastgram *2.5/CBNPoor14$EqSizeCalory
CBNPoor14$Tokhmemorgh_per_Calory<- CBNPoor14$Tokhmemorghgram *1.4/CBNPoor14$EqSizeCalory
CBNPoor14$Mive_per_Calory<- CBNPoor14$Mivegram *0.5/CBNPoor14$EqSizeCalory
CBNPoor14$Sabzi_per_Calory<- CBNPoor14$Sabzigram *0.5/CBNPoor14$EqSizeCalory
CBNPoor14$Makarooni_per_Calory<- CBNPoor14$Makaroonigram *3.6/CBNPoor14$EqSizeCalory
CBNPoor14$Sibzamini_per_Calory<- CBNPoor14$Sibzaminigram *0.9/CBNPoor14$EqSizeCalory
#utils::View(CBNPoor14)

#CalculatePer_calories in clusters
CBNPoor14[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor14[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(247:262)][] 
#utils::View(CBNPoor14)

CBNPoor14[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor14[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(264:279)][] 
#utils::View(CBNPoor14)

# Food grams from Calories2
CBNPoor14$Ghandgram2  <- CBNPoor14$Daily2_Ghand /4
CBNPoor14$Hoboobatgram2 <- CBNPoor14$Daily2_Hoboobat /3
CBNPoor14$Nangram2 <- CBNPoor14$Daily2_Nan  /2.5
CBNPoor14$Berenjgram2 <- CBNPoor14$Daily2_Berenj  /1.2
CBNPoor14$Roghangram2 <- CBNPoor14$Daily2_Roghan /8
CBNPoor14$Gooshtgram2 <- CBNPoor14$Daily2_Goosht  /2.5
CBNPoor14$Morghgram2 <- CBNPoor14$Daily2_Morgh  /2
CBNPoor14$Mahigram2 <- CBNPoor14$Daily2_Mahi  /1
CBNPoor14$Shirgram2 <- CBNPoor14$Daily2_Shir  /2.5
CBNPoor14$Mastgram2 <- CBNPoor14$Daily2_Mast  /1.5
CBNPoor14$Panirgram2 <- CBNPoor14$Daily2_Panir  /2.5
CBNPoor14$Tokhmemorghgram2 <- CBNPoor14$Daily2_Tokhmemorgh /1.4
CBNPoor14$Mivegram2 <- CBNPoor14$Daily2_Mive  /0.5
CBNPoor14$Sabzigram2 <- CBNPoor14$Daily2_Sabzi  /0.5
CBNPoor14$Makaroonigram2 <- CBNPoor14$Daily2_Makarooni  /3.6
CBNPoor14$Sibzaminigram2 <- CBNPoor14$Daily2_Sibzamini /0.9
#utils::View(CBNPoor14)

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


CBNPoor14<-CBNPoor14[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor14<-CBNPoor14[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor14<-CBNPoor14[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor14<-CBNPoor14[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor14<-CBNPoor14[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor14<-CBNPoor14[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor14<-CBNPoor14[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor14<-CBNPoor14[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor14<-CBNPoor14[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor14<-CBNPoor14[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor14<-CBNPoor14[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor14<-CBNPoor14[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor14<-CBNPoor14[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor14<-CBNPoor14[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor14<-CBNPoor14[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor14<-CBNPoor14[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor14<-CBNPoor14[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor14<-CBNPoor14[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor14[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor14[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(316:331)][] 
CBNPoor14[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor14)
CBNPoor14<-CBNPoor14[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor14<-CBNPoor14[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor14<-CBNPoor14[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14<-CBNPoor14[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor14[is.na(get(col)), (col) := 0]
CBNPoor14[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(336:351)][] 
CBNPoor14[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor14)
#order(CBNPoor14$AdditionalExpenditure_Per)

a<-CBNPoor14[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor14<-CBNPoor14[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor14)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor14,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor14[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_14<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor14,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor14[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_14<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor14,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor14[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_14<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor14,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor14[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_14<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor14,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor14<-CBNPoor14[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor14[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_14<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#w<-CBNPoor14[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor14[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor14[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 15###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline1_14 & cluster==1,1,0)]
c[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline1_14 & cluster==1 ,1,0)]
CBN[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline2_14 & cluster==2,1,Poor15)]
c[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline2_14 & cluster==2 ,1,Poor15)]
CBN[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline3_14 & cluster==3,1,Poor15)]
c[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline3_14 & cluster==3 ,1,Poor15)]
CBN[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline4_14 & cluster==4,1,Poor15)]
c[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline4_14 & cluster==4 ,1,Poor15)]
CBN[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline5_14 & cluster==5,1,Poor15)]
c[,Poor15:=ifelse(Total_Exp_Month_Per2 < Povertyline5_14 & cluster==5 ,1,Poor15)]
c[,weighted.mean(Poor15,Weight),by=cluster]
CBNPoor15<-CBN[Poor15==1]

#weighted consumption in each cluster
CBNPoor15<-CBNPoor15[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor15<-CBNPoor15[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor15<-CBNPoor15[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor15<-CBNPoor15[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor15<-CBNPoor15[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor15[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor15[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=195:210][] 
CBNPoor15[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor15[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor15)

# Food Calories
CBNPoor15$Ghand_Calory<- CBNPoor15$Ghandgram *4
CBNPoor15$Hoboobat_Calory<- CBNPoor15$Hoboobatgram *3
CBNPoor15$Nan_Calory<- CBNPoor15$Nangram *2.5
CBNPoor15$Berenj_Calory<- CBNPoor15$Berenjgram *1.2
CBNPoor15$Roghan_Calory<- CBNPoor15$Roghangram *8
CBNPoor15$Goosht_Calory<- CBNPoor15$Gooshtgram *2.5
CBNPoor15$Morgh_Calory<- CBNPoor15$Morghgram *2
CBNPoor15$Mahi_Calory<- CBNPoor15$Mahigram *1
CBNPoor15$Shir_Calory<- CBNPoor15$Shirgram *2.5
CBNPoor15$Mast_Calory<- CBNPoor15$Mastgram *1.5
CBNPoor15$Panir_Calory<- CBNPoor15$Mastgram *2.5
CBNPoor15$Tokhmemorgh_Calory<- CBNPoor15$Tokhmemorghgram *1.4
CBNPoor15$Mive_Calory<- CBNPoor15$Mivegram *0.5
CBNPoor15$Sabzi_Calory<- CBNPoor15$Sabzigram *0.5
CBNPoor15$Makarooni_Calory<- CBNPoor15$Makaroonigram *3.6
CBNPoor15$Sibzamini_Calory<- CBNPoor15$Sibzaminigram *0.9
#utils::View(CBNPoor15)

#CalculatePer_calories
CBNPoor15[, Daily_Calories := Reduce(`+`, .SD), .SDcols=212:227][] 
CBNPoor15[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor15[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor15 <- CBNPoor15[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor15[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor15$Ghand_per_Calory<- CBNPoor15$Ghandgram *4/CBNPoor15$EqSizeCalory
CBNPoor15$Hoboobat_per_Calory<- CBNPoor15$Hoboobatgram *3/CBNPoor15$EqSizeCalory
CBNPoor15$Nan_per_Calory<- CBNPoor15$Nangram *2.5/CBNPoor15$EqSizeCalory
CBNPoor15$Berenj_per_Calory<- CBNPoor15$Berenjgram *1.2/CBNPoor15$EqSizeCalory
CBNPoor15$Roghan_per_Calory<- CBNPoor15$Roghangram *8/CBNPoor15$EqSizeCalory
CBNPoor15$Goosht_per_Calory<- CBNPoor15$Gooshtgram *2.5/CBNPoor15$EqSizeCalory
CBNPoor15$Morgh_per_Calory<- CBNPoor15$Morghgram *2/CBNPoor15$EqSizeCalory
CBNPoor15$Mahi_per_Calory<- CBNPoor15$Mahigram *1/CBNPoor15$EqSizeCalory
CBNPoor15$Shir_per_Calory<- CBNPoor15$Shirgram *2.5/CBNPoor15$EqSizeCalory
CBNPoor15$Mast_per_Calory<- CBNPoor15$Mastgram *1.5/CBNPoor15$EqSizeCalory
CBNPoor15$Panir_per_Calory<- CBNPoor15$Mastgram *2.5/CBNPoor15$EqSizeCalory
CBNPoor15$Tokhmemorgh_per_Calory<- CBNPoor15$Tokhmemorghgram *1.4/CBNPoor15$EqSizeCalory
CBNPoor15$Mive_per_Calory<- CBNPoor15$Mivegram *0.5/CBNPoor15$EqSizeCalory
CBNPoor15$Sabzi_per_Calory<- CBNPoor15$Sabzigram *0.5/CBNPoor15$EqSizeCalory
CBNPoor15$Makarooni_per_Calory<- CBNPoor15$Makaroonigram *3.6/CBNPoor15$EqSizeCalory
CBNPoor15$Sibzamini_per_Calory<- CBNPoor15$Sibzaminigram *0.9/CBNPoor15$EqSizeCalory
#utils::View(CBNPoor15)

#CalculatePer_calories in clusters
CBNPoor15[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor15[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(248:263)][] 
#utils::View(CBNPoor15)

CBNPoor15[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor15[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(265:280)][] 
#utils::View(CBNPoor15)

# Food grams from Calories2
CBNPoor15$Ghandgram2  <- CBNPoor15$Daily2_Ghand /4
CBNPoor15$Hoboobatgram2 <- CBNPoor15$Daily2_Hoboobat /3
CBNPoor15$Nangram2 <- CBNPoor15$Daily2_Nan  /2.5
CBNPoor15$Berenjgram2 <- CBNPoor15$Daily2_Berenj  /1.2
CBNPoor15$Roghangram2 <- CBNPoor15$Daily2_Roghan /8
CBNPoor15$Gooshtgram2 <- CBNPoor15$Daily2_Goosht  /2.5
CBNPoor15$Morghgram2 <- CBNPoor15$Daily2_Morgh  /2
CBNPoor15$Mahigram2 <- CBNPoor15$Daily2_Mahi  /1
CBNPoor15$Shirgram2 <- CBNPoor15$Daily2_Shir  /2.5
CBNPoor15$Mastgram2 <- CBNPoor15$Daily2_Mast  /1.5
CBNPoor15$Panirgram2 <- CBNPoor15$Daily2_Panir  /2.5
CBNPoor15$Tokhmemorghgram2 <- CBNPoor15$Daily2_Tokhmemorgh /1.4
CBNPoor15$Mivegram2 <- CBNPoor15$Daily2_Mive  /0.5
CBNPoor15$Sabzigram2 <- CBNPoor15$Daily2_Sabzi  /0.5
CBNPoor15$Makaroonigram2 <- CBNPoor15$Daily2_Makarooni  /3.6
CBNPoor15$Sibzaminigram2 <- CBNPoor15$Daily2_Sibzamini /0.9
#utils::View(CBNPoor15)

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


CBNPoor15<-CBNPoor15[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor15<-CBNPoor15[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor15<-CBNPoor15[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor15<-CBNPoor15[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor15<-CBNPoor15[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor15<-CBNPoor15[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor15<-CBNPoor15[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor15<-CBNPoor15[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor15<-CBNPoor15[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor15<-CBNPoor15[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor15<-CBNPoor15[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor15<-CBNPoor15[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor15<-CBNPoor15[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor15<-CBNPoor15[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor15<-CBNPoor15[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor15<-CBNPoor15[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor15<-CBNPoor15[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor15<-CBNPoor15[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor15[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor15[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(317:332)][] 
CBNPoor15[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor15)
CBNPoor15<-CBNPoor15[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor15<-CBNPoor15[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor15<-CBNPoor15[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15<-CBNPoor15[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor15[is.na(get(col)), (col) := 0]
CBNPoor15[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(337:352)][] 
CBNPoor15[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor15)
#order(CBNPoor15$AdditionalExpenditure_Per)

a<-CBNPoor15[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor15<-CBNPoor15[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor15)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor15,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor15[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_15<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor15,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor15[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_15<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor15,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor15[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_15<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor15,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor15[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_15<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor15,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor15<-CBNPoor15[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor15[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_15<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor15[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor15[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor15[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 16###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline1_15 & cluster==1,1,0)]
c[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline1_15 & cluster==1 ,1,0)]
CBN[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline2_15 & cluster==2,1,Poor16)]
c[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline2_15 & cluster==2 ,1,Poor16)]
CBN[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline3_15 & cluster==3,1,Poor16)]
c[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline3_15 & cluster==3 ,1,Poor16)]
CBN[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline4_15 & cluster==4,1,Poor16)]
c[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline4_15 & cluster==4 ,1,Poor16)]
CBN[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline5_15 & cluster==5,1,Poor16)]
c[,Poor16:=ifelse(Total_Exp_Month_Per2 < Povertyline5_15 & cluster==5 ,1,Poor16)]
c[,weighted.mean(Poor16,Weight),by=cluster]
CBNPoor16<-CBN[Poor16==1]

#weighted consumption in each cluster
CBNPoor16<-CBNPoor16[,GhandG:=weighted.mean(Ghandgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,HoboobatG:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,RoghanG:=weighted.mean(Roghangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,BerenjG:=weighted.mean(Berenjgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,NanG:=weighted.mean(Nangram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,GooshtG:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MorghG:=weighted.mean(Morghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MahiG:=weighted.mean(Mahigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,ShirG:=weighted.mean(Shirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MastG:=weighted.mean(Mastgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,PanirG:=weighted.mean(Panirgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,TokhmemorghG:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MiveG:=weighted.mean(Mivegram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,SabziG:=weighted.mean(Sabzigram,Weight,na.rm = TRUE),by=Poor]
CBNPoor16<-CBNPoor16[,MakarooniG:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,SibzaminiG:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE),by=cluster]

#weighted prices in each cluster
CBNPoor16<-CBNPoor16[,GhandP:=weighted.mean(GhandPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,HoboobatP:=weighted.mean(HoboobatPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,RoghanP:=weighted.mean(RoghanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,BerenjP:=weighted.mean(BerenjPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,NanP:=weighted.mean(NanPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,GooshtP:=weighted.mean(GooshtPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MorghP:=weighted.mean(MorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MahiP:=weighted.mean(MahiPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,ShirP:=weighted.mean(ShirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MastP:=weighted.mean(MastPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,PanirP:=weighted.mean(PanirPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,TokhmemorghP:=weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,MiveP:=weighted.mean(MivePrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,SabziP:=weighted.mean(SabziPrice,Weight,na.rm = TRUE),by=Poor]
CBNPoor16<-CBNPoor16[,MakarooniP:=weighted.mean(MakarooniPrice,Weight,na.rm = TRUE),by=cluster]
CBNPoor16<-CBNPoor16[,SibzaminiP:=weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE),by=cluster]

#weighted food expenditures in each cluster
CBNPoor16[,GhandPoorcluster:=weighted.mean(Ghandgram,Weight,na.rm = TRUE)*weighted.mean(GhandPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,HoboobatPoorcluster:=weighted.mean(Hoboobatgram,Weight,na.rm = TRUE)*weighted.mean(HoboobatPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,RoghanPoorcluster:=weighted.mean(Roghangram,Weight,na.rm = TRUE)*weighted.mean(RoghanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,BerenjPoorcluster:=weighted.mean(Berenjgram,Weight,na.rm = TRUE)*weighted.mean(BerenjPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,NanPoorcluster:=weighted.mean(Nangram,Weight,na.rm = TRUE)*weighted.mean(NanPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,GooshtPoorcluster:=weighted.mean(Gooshtgram,Weight,na.rm = TRUE)*weighted.mean(GooshtPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,MorghPoorcluster:=weighted.mean(Morghgram,Weight,na.rm = TRUE)*weighted.mean(MorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,MahiPoorcluster:=weighted.mean(Mahigram,Weight,na.rm = TRUE)*weighted.mean(MahiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,ShirPoorcluster:=weighted.mean(Shirgram,Weight,na.rm = TRUE)*weighted.mean(ShirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,MastPoorcluster:=weighted.mean(Mastgram,Weight,na.rm = TRUE)*weighted.mean(MastPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,PanirPoorcluster:=weighted.mean(Panirgram,Weight,na.rm = TRUE)*weighted.mean(PanirPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,TokhmemorghPoorcluster:=weighted.mean(Tokhmemorghgram,Weight,na.rm = TRUE)*weighted.mean(TokhmemorghPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,MivePoorcluster:=weighted.mean(Mivegram,Weight,na.rm = TRUE)*weighted.mean(MivePrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,SabziPoorcluster:=weighted.mean(Sabzigram,Weight,na.rm = TRUE)*weighted.mean(SabziPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,MakarooniPoorcluster:=weighted.mean(Makaroonigram,Weight,na.rm = TRUE)*weighted.mean(MakarooniPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[,SibzaminiPoorcluster:=weighted.mean(Sibzaminigram,Weight,na.rm = TRUE)*weighted.mean(SibzaminiPrice,Weight,na.rm = TRUE)*0.001,by=cluster]
CBNPoor16[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=196:211][] 
CBNPoor16[, weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster][] 
CBNPoor16[, weighted.mean(Total_Exp_Month_Per_nondurable,Weight,na.rm = TRUE),by=cluster][] 

#utils::View(CBNPoor16)

# Food Calories
CBNPoor16$Ghand_Calory<- CBNPoor16$Ghandgram *4
CBNPoor16$Hoboobat_Calory<- CBNPoor16$Hoboobatgram *3
CBNPoor16$Nan_Calory<- CBNPoor16$Nangram *2.5
CBNPoor16$Berenj_Calory<- CBNPoor16$Berenjgram *1.2
CBNPoor16$Roghan_Calory<- CBNPoor16$Roghangram *8
CBNPoor16$Goosht_Calory<- CBNPoor16$Gooshtgram *2.5
CBNPoor16$Morgh_Calory<- CBNPoor16$Morghgram *2
CBNPoor16$Mahi_Calory<- CBNPoor16$Mahigram *1
CBNPoor16$Shir_Calory<- CBNPoor16$Shirgram *2.5
CBNPoor16$Mast_Calory<- CBNPoor16$Mastgram *1.5
CBNPoor16$Panir_Calory<- CBNPoor16$Mastgram *2.5
CBNPoor16$Tokhmemorgh_Calory<- CBNPoor16$Tokhmemorghgram *1.4
CBNPoor16$Mive_Calory<- CBNPoor16$Mivegram *0.5
CBNPoor16$Sabzi_Calory<- CBNPoor16$Sabzigram *0.5
CBNPoor16$Makarooni_Calory<- CBNPoor16$Makaroonigram *3.6
CBNPoor16$Sibzamini_Calory<- CBNPoor16$Sibzaminigram *0.9
#utils::View(CBNPoor16)

#CalculatePer_calories
CBNPoor16[, Daily_Calories := Reduce(`+`, .SD), .SDcols=213:228][] 
CBNPoor16[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor16[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor16 <- CBNPoor16[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor16[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]


CBNPoor16$Ghand_per_Calory<- CBNPoor16$Ghandgram *4/CBNPoor16$EqSizeCalory
CBNPoor16$Hoboobat_per_Calory<- CBNPoor16$Hoboobatgram *3/CBNPoor16$EqSizeCalory
CBNPoor16$Nan_per_Calory<- CBNPoor16$Nangram *2.5/CBNPoor16$EqSizeCalory
CBNPoor16$Berenj_per_Calory<- CBNPoor16$Berenjgram *1.2/CBNPoor16$EqSizeCalory
CBNPoor16$Roghan_per_Calory<- CBNPoor16$Roghangram *8/CBNPoor16$EqSizeCalory
CBNPoor16$Goosht_per_Calory<- CBNPoor16$Gooshtgram *2.5/CBNPoor16$EqSizeCalory
CBNPoor16$Morgh_per_Calory<- CBNPoor16$Morghgram *2/CBNPoor16$EqSizeCalory
CBNPoor16$Mahi_per_Calory<- CBNPoor16$Mahigram *1/CBNPoor16$EqSizeCalory
CBNPoor16$Shir_per_Calory<- CBNPoor16$Shirgram *2.5/CBNPoor16$EqSizeCalory
CBNPoor16$Mast_per_Calory<- CBNPoor16$Mastgram *1.5/CBNPoor16$EqSizeCalory
CBNPoor16$Panir_per_Calory<- CBNPoor16$Mastgram *2.5/CBNPoor16$EqSizeCalory
CBNPoor16$Tokhmemorgh_per_Calory<- CBNPoor16$Tokhmemorghgram *1.4/CBNPoor16$EqSizeCalory
CBNPoor16$Mive_per_Calory<- CBNPoor16$Mivegram *0.5/CBNPoor16$EqSizeCalory
CBNPoor16$Sabzi_per_Calory<- CBNPoor16$Sabzigram *0.5/CBNPoor16$EqSizeCalory
CBNPoor16$Makarooni_per_Calory<- CBNPoor16$Makaroonigram *3.6/CBNPoor16$EqSizeCalory
CBNPoor16$Sibzamini_per_Calory<- CBNPoor16$Sibzaminigram *0.9/CBNPoor16$EqSizeCalory
#utils::View(CBNPoor16)

#CalculatePer_calories in clusters
CBNPoor16[,Daily_Ghand_cluster:=weighted.mean(Ghand_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Hoboobat_cluster:=weighted.mean(Hoboobat_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Nan_cluster:=weighted.mean(Nan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Berenj_cluster:=weighted.mean(Berenj_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Roghan_cluster:=weighted.mean(Roghan_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Goosht_cluster:=weighted.mean(Goosht_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Morgh_cluster:=weighted.mean(Morgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Mahi_cluster:=weighted.mean(Mahi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Shir_cluster:=weighted.mean(Shir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Mast_cluster:=weighted.mean(Mast_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Panir_cluster:=weighted.mean(Panir_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Tokhmemorgh_cluster:=weighted.mean(Tokhmemorgh_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Mive_cluster:=weighted.mean(Mive_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Sabzi_cluster:=weighted.mean(Sabzi_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Makarooni_cluster:=weighted.mean(Makarooni_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,Daily_Sibzamini_cluster:=weighted.mean(Sibzamini_per_Calory,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(249:264)][] 
#utils::View(CBNPoor16)

CBNPoor16[,Daily2_Ghand:=(Ghand_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Hoboobat:=(Hoboobat_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Nan:=(Nan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Berenj:=(Berenj_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Roghan:=(Roghan_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Goosht:=(Goosht_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Morgh:=(Morgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Mahi:=(Mahi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Shir:=(Shir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Mast:=(Mast_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Panir:=(Panir_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Tokhmemorgh:=(Tokhmemorgh_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Mive:=(Mive_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Sabzi:=(Sabzi_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Makarooni:=(Makarooni_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[,Daily2_Sibzamini:=(Sibzamini_per_Calory*2100)/(Per_Daily_Calories)]
CBNPoor16[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(266:281)][] 
#utils::View(CBNPoor16)

# Food grams from Calories2
CBNPoor16$Ghandgram2  <- CBNPoor16$Daily2_Ghand /4
CBNPoor16$Hoboobatgram2 <- CBNPoor16$Daily2_Hoboobat /3
CBNPoor16$Nangram2 <- CBNPoor16$Daily2_Nan  /2.5
CBNPoor16$Berenjgram2 <- CBNPoor16$Daily2_Berenj  /1.2
CBNPoor16$Roghangram2 <- CBNPoor16$Daily2_Roghan /8
CBNPoor16$Gooshtgram2 <- CBNPoor16$Daily2_Goosht  /2.5
CBNPoor16$Morghgram2 <- CBNPoor16$Daily2_Morgh  /2
CBNPoor16$Mahigram2 <- CBNPoor16$Daily2_Mahi  /1
CBNPoor16$Shirgram2 <- CBNPoor16$Daily2_Shir  /2.5
CBNPoor16$Mastgram2 <- CBNPoor16$Daily2_Mast  /1.5
CBNPoor16$Panirgram2 <- CBNPoor16$Daily2_Panir  /2.5
CBNPoor16$Tokhmemorghgram2 <- CBNPoor16$Daily2_Tokhmemorgh /1.4
CBNPoor16$Mivegram2 <- CBNPoor16$Daily2_Mive  /0.5
CBNPoor16$Sabzigram2 <- CBNPoor16$Daily2_Sabzi  /0.5
CBNPoor16$Makaroonigram2 <- CBNPoor16$Daily2_Makarooni  /3.6
CBNPoor16$Sibzaminigram2 <- CBNPoor16$Daily2_Sibzamini /0.9
#utils::View(CBNPoor16)

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


CBNPoor16<-CBNPoor16[,GhandRealPrice:=GhandPrice*GhandIndexTehran/GhandIndex]
CBNPoor16<-CBNPoor16[,HoboobatRealPrice:=HoboobatPrice*HoboobatIndexTehran/HoboobatIndex]
CBNPoor16<-CBNPoor16[,RoghanRealPrice:=RoghanPrice*RoghanIndexTehran/RoghanIndex]
CBNPoor16<-CBNPoor16[,BerenjRealPrice:=BerenjPrice*BerenjIndexTehran/BerenjIndex]
CBNPoor16<-CBNPoor16[,NanRealPrice:=NanPrice*NanIndexTehran/NanIndex]
CBNPoor16<-CBNPoor16[,GooshtRealPrice:=GooshtPrice*GooshtIndexTehran/GooshtIndex]
CBNPoor16<-CBNPoor16[,MorghRealPrice:=MorghPrice*MorghIndexTehran/MorghIndex]
CBNPoor16<-CBNPoor16[,MahiRealPrice:=MahiPrice*MahiIndexTehran/MahiIndex]
CBNPoor16<-CBNPoor16[,TokhmemorghRealPrice:=TokhmemorghPrice*TokhmemorghIndexTehran/TokhmemorghIndex]
CBNPoor16<-CBNPoor16[,ShirRealPrice:=ShirPrice*ShirIndexTehran/ShirIndex]
CBNPoor16<-CBNPoor16[,MastRealPrice:=MastPrice*MastIndexTehran/MastIndex]
CBNPoor16<-CBNPoor16[,PanirRealPrice:=PanirPrice*PanirIndexTehran/PanirIndex]
CBNPoor16<-CBNPoor16[,MiveRealPrice:=MivePrice*MiveIndexTehran/MiveIndex]
CBNPoor16<-CBNPoor16[,SabziRealPrice:=SabziPrice*SabziIndexTehran/SabziIndex]
CBNPoor16<-CBNPoor16[,MakarooniRealPrice:=MakarooniPrice*MakarooniIndexTehran/MakarooniIndex]
CBNPoor16<-CBNPoor16[,SibzaminiRealPrice:=SibzaminiPrice*SibzaminiIndexTehran/SibzaminiIndex]
CBNPoor16<-CBNPoor16[,Total_Exp_Month_nondurable_Real:=Total_Exp_Month_nondurable*TotalIndexTehran/TotalIndex]
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Real:=Total_Exp_Month*TotalIndexTehran/TotalIndex]
CBNPoor16<-CBNPoor16[,FoodExpenditure_Real:=FoodExpenditure*KhorakIndexTehran/KhorakIndex]
bbb<-CBNPoor16[,.(Total_Exp_Month_nondurable_Real,Total_Exp_Month_nondurable,FoodExpenditure_Real,FoodExpenditure)]


# New bundle
CBNPoor16[,GhandExp2:=GhandRealPrice*Ghandgram2*0.001]
for (col in c("GhandExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,HoboobatExp2:=HoboobatRealPrice*Hoboobatgram2*0.001]
for (col in c("HoboobatExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,NanExp2:=NanRealPrice*Nangram2*0.001]
for (col in c("NanExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,BerenjExp2:=BerenjRealPrice*Berenjgram2*0.001]
for (col in c("BerenjExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,RoghanExp2:=RoghanRealPrice*Roghangram2*0.001]
for (col in c("RoghanExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,GooshtExp2:=GooshtRealPrice*Gooshtgram2*0.001]
for (col in c("GooshtExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,MorghExp2:=MorghRealPrice*Morghgram2*0.001]
for (col in c("MorghExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,MahiExp2:=MahiRealPrice*Mahigram2*0.001]
for (col in c("MahiExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,ShirExp2:=ShirRealPrice*Shirgram2*0.001]
for (col in c("ShirExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,MastExp2:=MastRealPrice*Mastgram2*0.001]
for (col in c("MastExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,PanirExp2:=PanirRealPrice*Panirgram2*0.001]
for (col in c("PanirExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,TokhmemorghExp2:=TokhmemorghRealPrice*Tokhmemorghgram2*0.001]
for (col in c("TokhmemorghExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,MiveExp2:=MiveRealPrice*Mivegram2*0.001]
for (col in c("MiveExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,SabziExp2:=SabziRealPrice*Sabzigram2*0.001]
for (col in c("SabziExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,MakarooniExp2:=MakarooniRealPrice*Makaroonigram2*0.001]
for (col in c("MakarooniExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[,SibzaminiExp2:=SibzaminiRealPrice*Sibzaminigram2*0.001]
for (col in c("SibzaminiExp2")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(318:333)][] 
CBNPoor16[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor16)
CBNPoor16<-CBNPoor16[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor16<-CBNPoor16[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]

CBNPoor16<-CBNPoor16[,GhandAdd:=(Daily2_Ghand-Ghand_per_Calory)*GhandRealPrice*0.001*30/4]
for (col in c("GhandAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,HoboobatAdd:=(Daily2_Hoboobat-Hoboobat_per_Calory)*HoboobatRealPrice*0.001*30/3]
for (col in c("HoboobatAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,NanAdd:=(Daily2_Nan-Nan_per_Calory)*NanRealPrice*0.001*30/2.5]
for (col in c("NanAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,BerenjAdd:=(Daily2_Berenj-Berenj_per_Calory)*BerenjRealPrice*0.001*30/1.2]
for (col in c("BerenjAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,RoghanAdd:=(Daily2_Roghan-Roghan_per_Calory)*RoghanRealPrice*0.001*30/8]
for (col in c("RoghanAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,GooshtAdd:=(Daily2_Goosht-Goosht_per_Calory)*GooshtRealPrice*0.001*30/2.5]
for (col in c("GooshtAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,MorghAdd:=(Daily2_Morgh-Morgh_per_Calory)*MorghRealPrice*0.001*30/2]
for (col in c("MorghAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,MahiAdd:=(Daily2_Mahi-Mahi_per_Calory)*MahiRealPrice*0.001*30/1]
for (col in c("MahiAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,TokhmemorghAdd:=(Daily2_Tokhmemorgh-Tokhmemorgh_per_Calory)*TokhmemorghRealPrice*0.001*30/1.4]
for (col in c("TokhmemorghAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,ShirAdd:=(Daily2_Shir-Shir_per_Calory)*ShirRealPrice*0.001*30/2.5]
for (col in c("ShirAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,MastAdd:=(Daily2_Mast-Mast_per_Calory)*MastRealPrice*0.001*30/1.5]
for (col in c("MastAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,PanirAdd:=(Daily2_Panir-Panir_per_Calory)*PanirRealPrice*0.001*30/2.5]
for (col in c("PanirAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,MiveAdd:=(Daily2_Mive-Mive_per_Calory)*MiveRealPrice*0.001*30/0.5]
for (col in c("MiveAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,SabziAdd:=(Daily2_Sabzi-Sabzi_per_Calory)*SabziRealPrice*0.001*30/0.5]
for (col in c("SabziAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,MakarooniAdd:=(Daily2_Makarooni-Makarooni_per_Calory)*MakarooniRealPrice*0.001*30/3.6]
for (col in c("MakarooniAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16<-CBNPoor16[,SibzaminiAdd:=(Daily2_Sibzamini-Sibzamini_per_Calory)*SibzaminiRealPrice*0.001*30/3.6]
for (col in c("SibzaminiAdd")) CBNPoor16[is.na(get(col)), (col) := 0]
CBNPoor16[, AdditionalExpenditure_Per := Reduce(`+`, .SD), .SDcols=c(338:353)][] 
CBNPoor16[, FoodExpenditure_Per2100 :=AdditionalExpenditure_Per+FoodExpenditure_Per][] 
#utils::View(CBNPoor16)
#order(CBNPoor16$AdditionalExpenditure_Per)

a<-CBNPoor16[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor16<-CBNPoor16[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor16)


#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor16,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Per2:=ifelse(cluster==1,Total_Exp_Month_nondurable_Real_Per,0)]
CBNPoorCluster<-CBNPoor16[cluster==1]
Engel1<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse1<-1/Engel1
Povertyline1_16<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor16,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor16[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_16<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor16,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor16[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_16<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor16,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor16[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_16<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor16,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor16<-CBNPoor16[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor16[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_16<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per2100,CBNPoorCluster$Weight,na.rm = TRUE)


#w<-CBNPoor16[,.(Total_Exp_Month_Per2,Total_Exp_Month_nondurable_Real,Poor,Poor2,cluster)]
#utils::View(CBN)
#b<-CBNPoor16[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor16[,.(HHID,Total_Exp_Month_Per2)]
CBN<-CBN[,Total_Exp_Month_Per2:=NULL]
CBN<-CBN[,cluster:=NULL]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]


#########Iteration 17###############
#Sort  Expenditure data
CBN<- CBN[order(Total_Exp_Month_Per2)]
c<-  c[order(Total_Exp_Month_Per2)]

#Indicate new poors
CBN[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline1_16 & cluster==1,1,0)]
c[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline1_16 & cluster==1 ,1,0)]
CBN[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline2_16 & cluster==2,1,Poor17)]
c[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline2_16 & cluster==2 ,1,Poor17)]
CBN[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline3_16 & cluster==3,1,Poor17)]
c[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline3_16 & cluster==3 ,1,Poor17)]
CBN[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline4_16 & cluster==4,1,Poor17)]
c[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline4_16 & cluster==4 ,1,Poor17)]
CBN[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline5_16 & cluster==5,1,Poor17)]
c[,Poor17:=ifelse(Total_Exp_Month_Per2 < Povertyline5_16 & cluster==5 ,1,Poor17)]
c[,weighted.mean(Poor17,Weight),by=cluster]
CBNPoor16[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,weighted.mean(FoodExpenditure_Per,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,weighted.mean(Size,Weight,na.rm = TRUE),by=cluster]
CBNPoor16[,sum(Weight*Size),by=cluster]
CBNPoor16[,sum(Weight),by=cluster]
CBNPoor16[,sum(Poor16),by=cluster]
CBNPoor17<-CBN[Poor17==1]


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

