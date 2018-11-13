#52-Clustering(only for 95).R
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
 
  
  #K-means weights
  FW <- MD[,.(X_Berenj, X_Ghand, X_Goosht,X_Hoboobat,
           X_Mahi, X_Makarooni,X_Mast, X_Mive,  
          X_Morgh, X_Nan,X_Panir, X_Roghan, 
          X_Sabzi, X_Shir, X_Sibzamini, X_Tokhmemorgh,
          ServiceExp,Region,NewArea,Weight)]
  
  FP <- MD[,.(P_Berenj, P_Ghand, P_Goosht,P_Hoboobat,
              P_Mahi, P_Makarooni,P_Mast, P_Mive,  
              P_Morgh, P_Nan,P_Panir, P_Roghan, 
              P_Sabzi, P_Shir, P_Sibzamini, P_Tokhmemorgh,
              MetrPrice,Region,NewArea,Weight)]
 
  dt1 <- FW[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,NewArea)]
  dt1<- dt1[order(Region,NewArea)]
  dt1[,NewArea:=NULL]
  dt1[,Weight:=NULL]
  dt1[,Region:=NULL]

  dt2 <- FP[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region,NewArea)]
  dt2<- dt2[order(Region,NewArea)]
  dt2[,Weight:=NULL]
  dtUrban<-dt2[Region=="Urban"]
  dtRural<-dt2[Region=="Rural"]
  dt2Urban<-dtUrban[,.(P_Berenj, P_Ghand, P_Goosht,P_Hoboobat,
                       P_Mahi, P_Makarooni,P_Mast, P_Mive,  
                       P_Morgh, P_Nan,P_Panir, P_Roghan, 
                       P_Sabzi, P_Shir, P_Sibzamini, P_Tokhmemorgh,
                       MetrPrice,NewArea,Region)]
  dt2Rural<-dtRural[,.(P_Berenj, P_Ghand, P_Goosht,P_Hoboobat,
                       P_Mahi, P_Makarooni,P_Mast, P_Mive,  
                       P_Morgh, P_Nan,P_Panir, P_Roghan, 
                       P_Sabzi, P_Shir, P_Sibzamini, P_Tokhmemorgh,
                       MetrPrice,NewArea,Region)]
  dtUrban[,NewArea:=NULL]
  dtRural[,NewArea:=NULL]
  dtUrban[,Region:=NULL]
  dtRural[,Region:=NULL]

  # Deciding how many clusters
  # Urban areas
  wss <- (nrow(dtUrban)-1)*sum(apply(dtUrban,2,var))
  for (i in 2:30) wss[i] <- sum(kmeans(dtUrban, centers=i)$withinss)
  plot(1:30, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  # Rural areas
  wss <- (nrow(dtRural)-1)*sum(apply(dtRural,2,var))
  for (i in 2:30) wss[i] <- sum(kmeans(dtRural, centers=i)$withinss)
  plot(1:30, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  #Weighted clustering
  # Urban areas
  dt1.m <- dt1[,lapply(.SD, mean)]			# Weights for each vector
  dtW <- dtUrban * sqrt(dt1.m[rep(1,nrow(dtUrban))])	# Weighted observations
  kmeans(dtW,4)						# Simple K-means
  cl <- kmeans(dtW,4)
  dt2Urban <- dt2Urban[,cluster:=data.table(cl$cluster)]
  dt2Urban<-dt2Urban[,.(NewArea,Region,cluster)]
  #load(file="dt4Urban.rda")

  # Rural areas
  dt1.m <- dt1[,lapply(.SD, mean)]			# Weights for each vector
  dtW <- dtRural * sqrt(dt1.m[rep(1,nrow(dtRural))])	# Weighted observations
  kmeans(dtW,5)						# Simple K-means
  cl <- kmeans(dtW,5)
  dt2Rural <- dt2Rural[,cluster:=data.table(cl$cluster)]
  dt2Rural<-dt2Rural[,.(NewArea,Region,cluster)]
  #load(file="dt5Rural.rda")
  
  dt2total<-rbind(dt2Urban,dt2Rural)
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-merge(MD,dt2total,by=c("NewArea","Region"),all=TRUE)
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  }
  
  endtime <- proc.time()
  cat("\n\n============================\nIt took ")
  cat((endtime-starttime)["elapsed"])
  cat(" seconds")