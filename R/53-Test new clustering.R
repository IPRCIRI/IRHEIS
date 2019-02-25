#53-Test new clustering(only for 95).R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

cat(paste0("\n------------------------------\nYear:",Settings$baseyear,"\n"))
load(file=paste0(Settings$HEISProcessedPath,"Y",Settings$baseyear,"InitialPoor.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",Settings$baseyear,"BigFoodPrice.rda"))
#BigFoodPrice[,ifelse(Region==1,Region=="Urban",Region=="Rural")]
load(file = "EngleP.rda")

MD<-merge(MD,EngleP,by=c("NewArea2","Region"),all=TRUE)

#K-means weights
MD<-MD[,ServiceExp2:=ServiceExp]
FW <- MD[,.(ServiceExp,ServiceExp2,Region,NewArea,NewArea2,Weight)]

FP <- MD[,.(Engel,MetrPrice,Region,NewArea,NewArea2,Weight)]

dt1 <- FW[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,NewArea,NewArea2)]
dt1<- dt1[order(Region,NewArea)]
dt1[,NewArea:=NULL]
dt1[,NewArea2:=NULL]
dt1[,Weight:=NULL]
dt1[,Region:=NULL]

dt2 <- FP[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,NewArea,NewArea2)]
dt2<- dt2[order(Region,NewArea)]
dt2[,Weight:=NULL]
dtUrban<-dt2[Region=="Urban"]
dtRural<-dt2[Region=="Rural"]
dt2Urban<-dtUrban[,.(Engel,MetrPrice,NewArea,NewArea2,Region)]
dt2Rural<-dtRural[,.(Engel,MetrPrice,NewArea,NewArea2,Region)]
dtUrban[,NewArea:=NULL]
dtRural[,NewArea:=NULL]
dtUrban[,NewArea2:=NULL]
dtRural[,NewArea2:=NULL]
dtUrban[,Region:=NULL]
dtRural[,Region:=NULL]

# Deciding how many clusters
# Urban areas
wss <- (nrow(dtUrban)-1)*sum(apply(dtUrban,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(dtUrban, centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Urban's within groups sum of squares")

# Rural areas
wss <- (nrow(dtRural)-1)*sum(apply(dtRural,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(dtRural, centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Rural's within groups sum of squares")

#Weighted clustering
# Urban areas
dt1.m <- dt1[,lapply(.SD, mean)]			# Weights for each vector
dtW <- dtUrban * sqrt(dt1.m[rep(1,nrow(dtUrban))])	# Weighted observations
kmeans(dtW,3)						# Simple K-means
cl <- kmeans(dtW,3)
dt2Urban <- dt2Urban[,cluster:=data.table(cl$cluster)]
dt2Urban<-dt2Urban[,.(NewArea,NewArea2,Region,cluster)]
#save(dt2Urban,file ="dt2Urban.rda")

# Rural areas
dt1.m <- dt1[,lapply(.SD, mean)]			# Weights for each vector
dtW <- dtRural * sqrt(dt1.m[rep(1,nrow(dtRural))])	# Weighted observations
kmeans(dtW,3)						# Simple K-means
cl <- kmeans(dtW,3)
dt2Rural <- dt2Rural[,cluster:=data.table(cl$cluster)]
dt2Rural<-dt2Rural[,.(NewArea,NewArea2,Region,cluster)]
#save(dt2Rural,file ="dt2Rural.rda")

#load(file="dtpastUrban.rda")
#load(file="dtpastRural.rda")

#dt2total<-rbind(dtpastUrban,dtpastRural)

dt2total<-rbind(dt2Urban,dt2Rural)

#for(year in (Settings$startyear:Settings$endyear)){
#  cat(paste0("\n------------------------------\nYear:",year,"\n"))
#  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
#  MD<-merge(MD,dt2total,by=c("NewArea","NewArea2","Region"),all.x=TRUE)
#  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
#}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")