#InflationPvertyLine
#Calculate Poverty Line Based on Inflation
#Zahra Shahidi
#2019


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

FinalCountryResults <- data.table(Year=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalRegionResults <- data.table(Year=NA_integer_,Region=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                 PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalClusterResults <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                                  House_Share=NA_real_,
                                  SampleSize=NA_integer_,
                                  Engle=NA_integer_,FPLine=NA_integer_,
                                  PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/ProvinceCPI.xlsx", 
                                                             sheet = "Province"))
load(file=paste0(Settings$HEISProcessedPath,"Province.rda"))
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  if(year!=95){
  
    
  #MD<-MD[Region=="Rural"]
  
  Fin<-FinalProvinceResults[Year==95]
  
  I<-inflation[Year==year]
  Fin<-merge(Fin,I[,.(ProvinceCode,total)],by=c("ProvinceCode"))
  Fin<-Fin[,PL:=PovertyLine*total/100]
  EngleD<-FinalProvinceResults[Year==year]
  EngleD<-merge(EngleD,Fin[,.(ProvinceCode,PL)],by=c("ProvinceCode"))
  EngleD[,PovertyLine:=PL]
  }else{
    EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
                  .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                    FPLine=mean(FPLine)),by=.(ProvinceCode)]
    EngleD[,PovertyLine:=FPLine/Engel]
  
    
  }
  FF<-inflation[Year==year]
  EngleD<-merge(EngleD,FF[,.(ProvinceCode,total)])
  MD <- merge(MD,EngleD[,.(ProvinceCode,PovertyLine,Engel,total)],by=c("ProvinceCode"))
  MD<-MD[,Total_Exp_Month_Per:=Total_Exp_Month_Per]
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  
  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per)/PovertyLine)^2]
  
  ################Country##################
  
  X1 <- MD[,.(PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size))]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by="Year")
  FinalCountryResults <- rbind(FinalCountryResults,X)
  
  ################Region##################
  X1 <- MD[,.(PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=Region]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=Region]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","Region"))
  FinalRegionResults <- rbind(FinalRegionResults,X)
  
  ################Cluster##################
  X1 <- MD[,.(SampleSize=.N,MetrPrice=weighted.mean(MetrPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(ServiceExp/Total_Exp_Month,Weight),
              Engle=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=cluster3]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=cluster3]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","cluster3"))
  FinalClusterResults <- rbind(FinalClusterResults,X)
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)])
  #cat(MD[TOriginalFoodExpenditure_Per>0.8*FPLine &
  #         TOriginalFoodExpenditure_Per<1.2*FPLine &
  #        Region=="Rural" & NewArea==11,
  #      weighted.mean(Engel,Weight)])

  
}
save(FinalClusterResults,file=paste0(Settings$HEISProcessedPath,"FinalClusterResultsI.rda"))
save(FinalCountryResults,file=paste0(Settings$HEISProcessedPath,"FinalCountryResultsI.rda"))
FinalCountryResults1<-FinalCountryResults[,name:=as.factor(0)]

load(file=paste0(Settings$HEISProcessedPath,"FinalCountryResults.rda"))
FinalCountryResults<-FinalCountryResults[,name:=as.factor(1)]
FinalCountryResults<-rbind(FinalCountryResults,FinalCountryResults1)
FinalCountryResults<-FinalCountryResults[,Year:=as.factor(Year)]
FinalClusterResults<-FinalClusterResults[,Year:=as.factor(Year)]
FinalClusterResults<-FinalClusterResults[,cluster3:=as.factor(cluster3)]

ggplot(data=FinalCountryResults,aes(x=Year, y=PovertyHCR, group=name, colour=name))+geom_line()

ggplot(data=FinalCountryResults,aes(x=Year, y=PovertyHCR))+geom_line()

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")