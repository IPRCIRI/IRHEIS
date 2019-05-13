# 53-Personal Poverty line for provinces.R
# 
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Personal Pline =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"MD4test.rda"))

MDH <- TD[ TFoodExpenditure_Per>0.8*FPLine & TFoodExpenditure_Per<1.2*FPLine]


PPH <- MDH[ ,.(.N,PPLine=weighted.median(PersonalPLine,Weight),
               FPLine=mean(FPLine),EngelPersonal=mean(EngelPersonal)),by=.(Region,NewArea2)]

if (year==93) {
  PPH93<-PPH[,PPLine9396:=PPLine*1.119*1.09*1.096]
  save(PPH93,file="PPH93.rda")
}

if (year==94) {
  PPH94<-PPH[,PPLine9496:=PPLine*1.09*1.096]
  save(PPH94,file="PPH94.rda")
}

if (year==95) {
  PPH95<-PPH[,PPLine9596:=PPLine*1.096]
  save(PPH95,file="PPH95.rda")
}

if (year==96) {
  load(file="PPH95.rda" )
  load(file="PPH94.rda" )
  load(file="PPH93.rda" )
  PPH96<-PPH[,PPLine9696:=PPLine]
  save(PPH96,file="PPH96.rda")
  PPH96[,N:=NULL]
  PPH96[,EngelPersonal:=NULL]
  PPH96[,FPLine:=NULL]
  PPH96[,PPLine:=NULL]
  PPH<-merge(PPH96,PPH95,by=c("Region","NewArea2"),all=TRUE)
  PPH[,N:=NULL]
  PPH[,EngelPersonal:=NULL]
  PPH[,FPLine:=NULL]
  PPH[,PPLine:=NULL]
  PPH<-merge(PPH,PPH94,by=c("Region","NewArea2"),all=TRUE)
  PPH[,N:=NULL]
  PPH[,EngelPersonal:=NULL]
  PPH[,FPLine:=NULL]
  PPH[,PPLine:=NULL]
  PPH<-merge(PPH,PPH93,by=c("Region","NewArea2"),all=TRUE)
  PPH[,N:=NULL]
  PPH[,EngelPersonal:=NULL]
  PPH[,FPLine:=NULL]
  PPH[,PPLine:=NULL]
  PPH[is.na(PPH)] <- 0
  w <- c( "PPLine9696", "PPLine9596",
          "PPLine9496", "PPLine9396")
  
  PPH[, PPLineSum := Reduce(`+`, .SD), .SDcols=w]
  PPH[, PPLineMean :=ifelse(NewArea2=="Chaharmahal" & Region=="Rural",
                                    PPLineSum/3,PPLineSum/4)]
  
  y2<-PPH[Region=="Urban",.(PPLineMean,NewArea2)]
  y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PPLineMean)])
  ggplot(y2, aes(x = y2$NewArea, y = y2$PPLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  x2<-PPH[Region=="Rural",.(PPLineMean,NewArea2)]
  x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PPLineMean)])
  ggplot(x2, aes(x = x2$NewArea, y = x2$PPLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
}
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")