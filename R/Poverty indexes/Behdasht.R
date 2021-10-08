#Behdasht

#Zahra Shahidi
#2021
rm(list=ls())

cat("\n\n================ Behdasht Index =====================================\n")

source("168-Step8-PovertyStats.R")



Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(writexl)
startTime <- proc.time()

Geoo <- as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx",
                                 sheet = "Sheet1"))
Geo_O<-as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx", 
                                sheet = "Sheet2"))
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPovertyLine.rda"))
  
  MD<-MD[,Behdasht:=Medical_Exp+Hygiene_Exp+Durable_Emergency]
  MD<-MD[,TenPercent:=ifelse(Behdasht>0.1*Total_Exp_Month,1,0)]
  MD<-MD[,TwentyfivePercent:=ifelse(Behdasht>0.25*Total_Exp_Month,1,0)]
  MD<-MD[,Kamarshekan:=ifelse(Behdasht>0.4*(Total_Exp_Month-OriginalFoodExpenditure),1,0)]
  FinalCountryResults<-FinalCountryResults[Year==year]
 MD<-MD[,cluster3:=as.numeric(cluster3)]
 FinalClusterResults<-as.data.table(FinalClusterResults[,cluster3:=as.numeric(cluster3)])
 FinalClusterResults1<-FinalClusterResults[Year==year]
  MD<-merge(MD,FinalClusterResults1[,.(cluster3,PovertyLine_Final)],by=c("cluster3"),all.x = TRUE)
  MD[,BehdashtPoor:=ifelse((Total_Exp_Month_Per>PovertyLine_Final) & (Total_Exp_Month_Per-(Medical_Exp/EqSizeOECD)-(Hygiene_Exp/EqSizeOECD)<PovertyLine_Final),1,0)]
  #-------------------Country Resaults-------------
    Features<-MD[,.(TenPercent=weighted.mean(TenPercent,Weight),
                  TwentyfivePercent=weighted.mean(TwentyfivePercent,Weight),
                  Behdasht=weighted.mean(Behdasht,Weight),
                  KamarShekan=weighted.mean(Kamarshekan,Weight),
                  PovertyHCR=weighted.mean(BehdashtPoor,Weight*Size))]
  #-------------------Region Resaults--------------
  Features_Region<-MD[,.(TenPercent=weighted.mean(TenPercent,Weight),
                  TwentyfivePercent=weighted.mean(TwentyfivePercent,Weight),
                  Behdasht=weighted.mean(Behdasht,Weight),
                  KamarShekan=weighted.mean(Kamarshekan,Weight),
                  PovertyHCR=weighted.mean(BehdashtPoor,Weight*Size)),by="Region"]
  
  #-------------------Province Resaults------------
  Features_Province<-MD[,.(TenPercent=weighted.mean(TenPercent,Weight),
                     TwentyfivePercent=weighted.mean(TwentyfivePercent,Weight),
                     Behdasht=weighted.mean(Behdasht,Weight),
                     KamarShekan=weighted.mean(Kamarshekan,Weight),
                     PovertyHCR=weighted.mean(BehdashtPoor,Weight*Size)),by="ProvinceName"]
    Features_Province<-merge(Features_Province,Geo_O[,.(ProvinceName,Province)])
  Features_Province<-Features_Province[,Year:=year]
  Features<-Features[,Year:=year]
#---------------Build TimeSeries-----------------  
    if (year==Settings$startyear){
    Shakhes<-Features
  }else{
    Shakhes<-rbind(Shakhes,Features)
  }
  
  if (year==Settings$startyear){
    Shakhes_Region<-Features_Region
  }else{
    Shakhes_Region<-rbind(Shakhes_Region,Features_Region)
  }
  
  
  if (year==Settings$startyear){
    Shakhes_Province<-Features_Province
  }else{
    Shakhes_Province<-rbind(Shakhes_Province,Features_Province)
  }
  
}
write_xlsx(Shakhes,path=paste0(Settings$HEISResultsPath,"Shakhes_Behdasht.xlsx"))
write_xlsx(Shakhes_Region,path=paste0(Settings$HEISResultsPath,"Shakhes_Behdasht_Region.xlsx"))
write_xlsx(Shakhes_Province,path=paste0(Settings$HEISResultsPath,"Shakhes_Behdasht_Province.xlsx"))


endTime <- proc.time()
cat("\n\n============================\nIt took",(endTime-startTime)[3], "seconds.")

