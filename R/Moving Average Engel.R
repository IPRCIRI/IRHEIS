#Moving Average Engel
#New Poverty Results Using Moving Average Engel
#Zahra Shahidi
#2019

rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(writexl)
library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(pracma)
library(tidyverse)     
library(lubridate)     
library(fpp2)          
library(zoo)
library(smooth)
library(plotrix)
library(Hmisc)
library(gridExtra)
library(dplyr)
load(file=paste0(Settings$HEISProcessedPath,"FinalClusterResults.rda"))

FinalClusterResults<-FinalClusterResults[order(cluster3,Year)]
for(Cluster3 in (1:13)){
 m<-FinalClusterResults[cluster3==Cluster3]
 m<-m[,MA_Engel:=movavg(x=m$Engle,n=5,type = "s")]
 if (Cluster3==1){
   s<-m
 }else{
   s<-rbind(s,m)
 }
}
s<-s[,MA_PovertyLine:=FPLine/MA_Engel]

FinalCountryResults <- data.table(Year=NA_integer_,MA_PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalRegionResults <- data.table(Year=NA_integer_,Region=NA_integer_,
                                 Calory_Price_Area=NA_integer_,
                                 Engle=NA_integer_,FPLine=NA_integer_,
                                 MA_PovertyLine=NA_real_,PovertyHCR=NA_real_)[0]
FinalClusterResults <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                                  House_Share=NA_real_,
                                  SampleSize=NA_integer_,
                                  Engle=NA_integer_,FPLine=NA_integer_,
                                  MA_PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

FinalProvinceResults<-data.table(Year=NA_integer_,ProvinceCode=NA_integer_,
                                 Calory_Price_Area=NA_integer_,
                                 Engle=NA_integer_,FPLine=NA_integer_,
                                 MA_PovertyLine=NA_real_,PovertyHCR=NA_real_)[0]
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  
  EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
                .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                  FPLine=mean(FPLine)),by=.(Region,cluster3)]
  f<-s[Year==year]
  
  EngleD<-merge(EngleD,f[,.(cluster3,MA_PovertyLine)],by=c("cluster3"))
  MD <- merge(MD,EngleD[,.(cluster3,Region,MA_PovertyLine,Engel)],by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < MA_PovertyLine,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  
  MD[,FGT1M:=(MA_PovertyLine-Total_Exp_Month_Per)/MA_PovertyLine]
  MD[,FGT2M:=((MA_PovertyLine-Total_Exp_Month_Per)/MA_PovertyLine)^2]
  
  ################Country##################
  
  X1 <- MD[,.(MA_PovertyLine=weighted.mean(MA_PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size))]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by="Year")
  FinalCountryResults <- rbind(FinalCountryResults,X)
  
  ################Region##################
  X1 <- MD[,.(Engle=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              MA_PovertyLine=weighted.mean(MA_PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Calory_Price_Area=weighted.mean(Calory_Price_Area,Weight)),by=Region]
  
  
  X1[,Year:=year]
  
  FinalRegionResults<- rbind(FinalRegionResults,X1) 
  
  
  ################Cluster##################
  X1 <- MD[,.(SampleSize=.N,MetrPrice=weighted.mean(MetrPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(ServiceExp/Total_Exp_Month,Weight),
              Engle=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              MA_PovertyLine=weighted.mean(MA_PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=cluster3]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=cluster3]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","cluster3"))
  FinalClusterResults<- rbind(FinalClusterResults,X)
  ################Province##################
  X1 <- MD[,.(Engle=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              MA_PovertyLine=weighted.mean(MA_PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Calory_Price_Area=weighted.mean(Calory_Price_Area,Weight)),by=ProvinceCode]
  
  
X1[,Year:=year]
  
  FinalProvinceResults<- rbind(FinalProvinceResults,X1) 
  FinalProvinceResults<-FinalProvinceResults[order(ProvinceCode)]
  M<-MD[NewArea<=30,.(ProvinceCode,NewArea2)]
  M<-M[,ProvinceCode:=as.integer(ProvinceCode)]
  M<-distinct(M)
  M<-M[NewArea2!="Sh_Arak"]
 # FinalProvinceResults<-merge(FinalProvinceResults,M,by="ProvinceCode")
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)])
  #cat(MD[TOriginalFoodExpenditure_Per>0.8*FPLine &
  #         TOriginalFoodExpenditure_Per<1.2*FPLine &
  #        Region=="Rural" & NewArea==11,
  #      weighted.mean(Engel,Weight)])
  

}
write_xlsx(FinalProvinceResults,path="ProvinceResults.xlsx",col_names=T)
write_xlsx(FinalRegionResults,path="RegionResults.xlsx",col_names=T)

s<-s[,cluster3:=as.factor(cluster3)]
s<-s[,Year:=as.factor(Year)]

ggplot(data=FinalClusterResults,aes(x=Year, y=PovertyHCR, group=cluster3, colour=cluster3))+geom_line()
FinalCountryResults1<-FinalCountryResults
FinalCountryResults1<-FinalCountryResults1[,name:=as.factor(0)]
FinalCountryResults1<-FinalCountryResults1[,.(Year,MA_PovertyLine,PovertyHCR,PovertyGap,PovertyDepth,name)]

load(file=paste0(Settings$HEISProcessedPath,"FinalCountryResults.rda"))
load(file=paste0(Settings$HEISProcessedPath,"FinalClusterResults.rda"))
FinalCountryResults2<-merge(FinalCountryResults1,FinalCountryResults[,.(PovertyLine,Year)],by=c("Year"))
FinalClusterResults<-FinalClusterResults[,cluster3:=as.factor(cluster3)]
FinalClusterResults<-FinalClusterResults[,Year:=as.factor(Year)]
ggplot(data=FinalCountryResults2,aes(x=Year, y=MA_PovertyLine/PovertyLine))+geom_line()
FinalCountryResults<-FinalCountryResults[,name:=as.factor(1)]

FinalCountryResults2<-FinalCountryResults2[,PovertyLine:=MA_PovertyLine]
FinalCountryResults2<-FinalCountryResults2[,.(Year,PovertyLine,PovertyHCR,PovertyGap,PovertyDepth,name)]
FinalCountryResults2<-FinalCountryResults2[,Year:=as.factor(Year)]


ggplot(data=FinalCountryResults,aes(x=Year, y=PovertyHCR, group=name, colour=name))+geom_line()

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")