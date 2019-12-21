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
inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/inflation.xlsx",col_names = T))
for(year in (Settings$endyear:Settings$startyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  if(year!=97){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year+1,"EngleD.rda"))
  
  #MD<-MD[Region=="Rural"]
  E<-EngleD
  
  
  I<-inflation[Year==year]
  i<-as.double(I$Inflation)
  EngleD$FPLine<-E$FPLine
  EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
                .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                  FPLine=mean(FPLine)),by=.(Region,cluster3)]
  i<-i/100
  EngleD[,PovertyLine:=FPLine*(1-i)/Engel]
  }else{
    EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
                  .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                    FPLine=mean(FPLine)),by=.(Region,cluster3)]
    EngleD[,PovertyLine:=FPLine/Engel]
  }
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,Engel)],by=c("Region","cluster3"))
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
  save(EngleD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleD.rda"))
  
  
}
save(FinalClusterResults,file=paste0(Settings$HEISProcessedPath,"FinalClusterResults.rda"))
save(FinalCountryResults,file=paste0(Settings$HEISProcessedPath,"FinalCountryResults.rda"))

ggplot(data=FinalCountryResults,aes(x=Year, y=PovertyHCR))+geom_line()

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")