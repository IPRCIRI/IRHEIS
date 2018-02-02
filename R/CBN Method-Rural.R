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
kmeans(dtW,6)						# Simple K-means

cl <- kmeans(dtW,6)
cl$cluster
dt2 <- dt2[,cluster:=data.table(cl$cluster)]
dt2<-dt2[,.(ProvinceCode,cluster)]
#plot(PRICE1, PRICE2,col=cl$cluster)
#points(cl$centers, pch=20)
CBNPoor<-merge(CBNPoor,dt2,by=c("ProvinceCode"),all.x = TRUE)
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
CBNPoor[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=178:193][] 
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
CBNPoor[, Daily_Calories := Reduce(`+`, .SD), .SDcols=195:210][] 
CBNPoor[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor <- CBNPoor[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]
CBNPoor[,weighted.mean(Daily_Calories_cluster,Weight,na.rm = TRUE),by=cluster]


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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(231:246)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(248:263)][] 

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
CBNPoor[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(300:315)][] 
CBNPoor[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor)
CBNPoor<-CBNPoor[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor<-CBNPoor[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor<-CBNPoor[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a<-a[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
a[,weighted.mean(ratio1,Weight),by=cluster]
a[,ratio1,by=cluster]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
a<-a[,ratio3:=FoodExpenditure_Real/Total_Exp_Month_nondurable]
summary(a$ratio1)

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
Povertyline1_1<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_1<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_1<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_1<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_1<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_1<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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
CBN[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline6_1 & cluster==6,1,Poor2)]
c[,Poor2:=ifelse(Total_Exp_Month_Per2 < Povertyline6_1 & cluster==6 ,1,Poor2)]
CBN[,weighted.mean(Poor2,Weight),by=cluster]
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
CBNPoor2[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=180:195][] 
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
CBNPoor2[, Daily_Calories := Reduce(`+`, .SD), .SDcols=197:212][] 
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
CBNPoor2[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(233:248)][] 
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
CBNPoor2[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(250:265)][] 
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
CBNPoor2[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(302:317)][] 
CBNPoor2[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor2)
CBNPoor2<-CBNPoor2[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor2<-CBNPoor2[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor2[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor2<-CBNPoor2[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
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
Povertyline1_2<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_2<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_2<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_2<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_2<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor2,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor2<-CBNPoor2[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor2[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_2<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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
CBN[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline6_2 & cluster==6,1,Poor3)]
c[,Poor3:=ifelse(Total_Exp_Month_Per2 < Povertyline6_2 & cluster==6 ,1,Poor3)]
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
CBNPoor3[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=181:196][] 
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
CBNPoor3[, Daily_Calories := Reduce(`+`, .SD), .SDcols=198:213][] 
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
CBNPoor3[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(234:249)][] 
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
CBNPoor3[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(251:266)][] 
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
CBNPoor3[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(303:318)][] 
CBNPoor3[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor3)
CBNPoor3<-CBNPoor3[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor3<-CBNPoor3[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
a<-CBNPoor3[,.(Weight,cluster,FoodExpenditure_Real,FoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month_nondurable_Real,FoodExpenditure_Real_Per,FoodExpenditure_Per,Total_Exp_Month_Real)]
CBNPoor3<-CBNPoor3[,ratio1:=FoodExpenditure_Real/Total_Exp_Month_Real]
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
Povertyline1_3<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_3<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_3<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_3<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_3<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor3,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor3<-CBNPoor3[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor3[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_3<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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


#########Iteration 3###############
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
CBN[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline6_3 & cluster==6,1,Poor4)]
c[,Poor4:=ifelse(Total_Exp_Month_Per2 < Povertyline6_3 & cluster==6 ,1,Poor4)]
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
CBNPoor4[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=182:197][] 
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
CBNPoor4[, Daily_Calories := Reduce(`+`, .SD), .SDcols=199:214][] 
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
CBNPoor4[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(235:250)][] 
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
CBNPoor4[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(252:267)][] 
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
CBNPoor4[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(304:319)][] 
CBNPoor4[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor4)
CBNPoor4<-CBNPoor4[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor4<-CBNPoor4[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
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
Povertyline1_4<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_4<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_4<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_4<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_4<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor4,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor4<-CBNPoor4[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor4[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_4<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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
CBN[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline6_4 & cluster==6,1,Poor5)]
c[,Poor5:=ifelse(Total_Exp_Month_Per2 < Povertyline6_4 & cluster==6 ,1,Poor5)]
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
CBNPoor5[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=183:198][] 
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
CBNPoor5[, Daily_Calories := Reduce(`+`, .SD), .SDcols=200:215][] 
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
CBNPoor5[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(236:251)][] 
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
CBNPoor5[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(253:268)][] 
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
CBNPoor5[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(305:320)][] 
CBNPoor5[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor5)
CBNPoor5<-CBNPoor5[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor5<-CBNPoor5[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
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
Povertyline1_5<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_5<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_5<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_5<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_5<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor5,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor5<-CBNPoor5[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor5[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_5<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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


#########Iteration 5###############
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
CBN[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline6_5 & cluster==6,1,Poor6)]
c[,Poor6:=ifelse(Total_Exp_Month_Per2 < Povertyline6_5 & cluster==6 ,1,Poor6)]
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
CBNPoor6[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=184:199][] 
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
CBNPoor6[, Daily_Calories := Reduce(`+`, .SD), .SDcols=201:216][] 
CBNPoor6[,EqSizeCalory :=(Size-NKids) + NKids*(1800/2100)]
CBNPoor6[,Per_Daily_Calories:=Daily_Calories/EqSizeCalory]
CBNPoor6 <- CBNPoor6[Daily_Calories<100000] # arbitrary removal of outliers
CBNPoor6[,Daily_Calories_cluster:=weighted.mean(Per_Daily_Calories,Weight,na.rm = TRUE),by=cluster]

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
CBNPoor6[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(237:252)][] 
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
CBNPoor6[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(254:269)][] 
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
CBNPoor6[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(306:321)][] 
CBNPoor6[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor6)
CBNPoor6<-CBNPoor6[,FoodExpenditure_Real_Per:=FoodExpenditure_Real/EqSizeRevOECD]
CBNPoor6<-CBNPoor6[,Total_Exp_Month_nondurable_Real_Per:=Total_Exp_Month_nondurable_Real/EqSizeRevOECD]
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
Povertyline1_6<-Engel_Reverse1*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==2,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==2]
Engel2<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse2<-1/Engel2
Povertyline2_6<-Engel_Reverse2*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==3,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==3]
Engel3<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse3<-1/Engel3
Povertyline3_6<-Engel_Reverse3*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==4,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==4]
Engel4<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse4<-1/Engel4
Povertyline4_6<-Engel_Reverse4*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==5,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==5]
Engel5<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse5<-1/Engel5
Povertyline5_6<-Engel_Reverse5*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_Real~ Total_Exp_Month_Real , weights = Weight, data=CBNPoor6,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor6<-CBNPoor6[,Total_Exp_Month_Per2:=ifelse(cluster==6,Total_Exp_Month_nondurable_Real_Per,Total_Exp_Month_Per2)]
CBNPoorCluster<-CBNPoor6[cluster==6]
Engel6<-weighted.mean(CBNPoorCluster$ratio1,CBNPoorCluster$Weight)
Engel_Reverse6<-1/Engel6
Povertyline6_6<-Engel_Reverse6*weighted.mean(CBNPoorCluster$FoodExpenditure_Per,CBNPoorCluster$Weight,na.rm = TRUE)

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


#########Iteration 6###############
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
CBN[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline6_6 & cluster==6,1,Poor7)]
c[,Poor7:=ifelse(Total_Exp_Month_Per2 < Povertyline6_6 & cluster==6 ,1,Poor7)]
c[,weighted.mean(Poor7,Weight),by=cluster]
CBNPoor7<-CBN[Poor7==1]

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)

