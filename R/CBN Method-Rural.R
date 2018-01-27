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
cl <- kmeans(dt,6)
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
CBNPoor[, Poors_Food_Expenditures := Reduce(`+`, .SD), .SDcols=176:191][] 
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
CBNPoor[, Daily_Calories := Reduce(`+`, .SD), .SDcols=193:208][] 
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
CBNPoor[, Daily_Calories_cluster2 := Reduce(`+`, .SD), .SDcols=c(229:244)][] 
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
CBNPoor[, Daily_Calories3 := Reduce(`+`, .SD), .SDcols=c(246:261)][] 

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
CBNPoor[, FoodExpenditure_Per_day2 := Reduce(`+`, .SD), .SDcols=c(295:310)][] 
CBNPoor[, FoodExpenditure_month2 := FoodExpenditure_Per_day2*EqSizeRevOECD*30][] 
#utils::View(CBNPoor)
a<-CBNPoor[,.(FoodExpenditure_month2,FoodExpenditure,Total_Exp_Month_nondurable)]
a<-a[,ratio1:=FoodExpenditure_month2/Total_Exp_Month_nondurable]
a<-a[,ratio2:=FoodExpenditure/Total_Exp_Month_nondurable]
summary(a$ratio1)
#utils::View(CBNPoor)

#model for each cluster
#cluster 1
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==1))
summary(model)
Engel1<-coef(model)[2]
Engel_Reverse1<-1/Engel1
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==1,Engel_Reverse1*FoodExpenditure_Per,0)]
CBNPoorCluster1<-CBNPoor[cluster==1]
Povertyline1_1<-Engel_Reverse1*weighted.mean(CBNPoorCluster1$FoodExpenditure_Per,CBNPoorCluster1$Weight,na.rm = TRUE)

#cluster 2
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==2))
summary(model)
Engel2<-coef(model)[2]
Engel_Reverse2<-1/Engel2
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==2,Engel_Reverse2*FoodExpenditure_Per,Total_Exp_Month_Per2)]
CBNPoorCluster2<-CBNPoor[cluster==2]
Povertyline2_1<-Engel_Reverse2*weighted.mean(CBNPoorCluster2$FoodExpenditure_Per,CBNPoorCluster2$Weight,na.rm = TRUE)

#cluster 3
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==3))
summary(model)
Engel3<-coef(model)[2]
Engel_Reverse3<-1/Engel3
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==3,Engel_Reverse3*FoodExpenditure_Per,Total_Exp_Month_Per2)]
CBNPoorCluster3<-CBNPoor[cluster==3]
Povertyline3_1<-Engel_Reverse3*weighted.mean(CBNPoorCluster3$FoodExpenditure_Per,CBNPoorCluster3$Weight,na.rm = TRUE)

#cluster 4
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==4))
summary(model)
Engel4<-coef(model)[2]
Engel_Reverse4<-1/Engel4
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==4,Engel_Reverse4*FoodExpenditure_Per,Total_Exp_Month_Per2)]
CBNPoorCluster4<-CBNPoor[cluster==4]
Povertyline4_1<-Engel_Reverse4*weighted.mean(CBNPoorCluster4$FoodExpenditure_Per,CBNPoorCluster4$Weight,na.rm = TRUE)

#cluster 5
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==5))
summary(model)
Engel5<-coef(model)[2]
Engel_Reverse5<-1/Engel5
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==5,Engel_Reverse5*FoodExpenditure_Per,Total_Exp_Month_Per2)]
CBNPoorCluster5<-CBNPoor[cluster==5]
Povertyline5_1<-Engel_Reverse5*weighted.mean(CBNPoorCluster5$FoodExpenditure_Per,CBNPoorCluster5$Weight,na.rm = TRUE)

#cluster 6
model <- lm(FoodExpenditure_month2 ~ Total_Exp_Month_nondurable , weights = Weight, data=CBNPoor,subset = (cluster==6))
summary(model)
Engel6<-coef(model)[2]
Engel_Reverse6<-1/Engel6
CBNPoor<-CBNPoor[,Total_Exp_Month_Per2:=ifelse(cluster==6,Engel_Reverse6*FoodExpenditure_Per,Total_Exp_Month_Per2)]
CBNPoorCluster6<-CBNPoor[cluster==6]
Povertyline6_1<-Engel_Reverse6*weighted.mean(CBNPoorCluster6$FoodExpenditure_Per,CBNPoorCluster6$Weight,na.rm = TRUE)


#utils::View(CBN)
#b<-CBNPoor[,.(Total_Exp_Month_nondurable,Total_Exp_Month_Per2)]
m<-CBNPoor[,.(HHID,Total_Exp_Month_Per2)]
CBN<-merge(CBN,m,by =c("HHID"),all.x=TRUE)
CBN<-merge(CBN,dt2,by=c("ProvinceCode"),all.x = TRUE)
for (col in c("Total_Exp_Month_Per2")) CBN[is.na(get(col)), (col) := Total_Exp_Month_Per_nondurable]
c<-CBN[,.(Total_Exp_Month_Per_nondurable,Total_Exp_Month,Total_Exp_Month_Per2,Poor,Decile,Weight,cluster)]
