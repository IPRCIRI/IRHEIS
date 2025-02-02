# Poverty Line.R
# 
# Copyright © 2020:Arin Shahbazian
# Licence: GPL-3

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

FinalCountryResults <- data.table(Year=NA_integer_,PovertyLine=NA_real_,
                                  Engle=NA_real_,Bundle_Value=NA_real_,
                                  Total_Exp_Month_Per=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalRegionResults <- data.table(Year=NA_integer_,Region=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                 PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]
FinalClusterResults <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                                  House_Share=NA_real_,FoodKCaloriesHH_Per=NA_real_,
                                  SampleSize=NA_integer_,
                                  Engle=NA_integer_,FPLine=NA_integer_,
                                  PovertyLine=NA_real_,PovertyHCR=NA_real_,
                                  PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

FinalClusterDiff <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                               House_Share=NA_real_,Food_Share=NA_real_,Durable_Share=NA_real_,
                               SampleSize=NA_integer_,Clusterdiff=NA_integer_,
                               Engle=NA_integer_,FPLine=NA_integer_,FoodKCaloriesHH_Per=NA_real_,
                               Total_Exp_Month_Per_nondurable=NA_real_,
                               Total_Exp_Month_Per=NA_real_,
                               PovertyLine=NA_real_,PovertyHCR=NA_real_,
                               PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

HighEngles<-data.table(Year=NA_integer_,Region=NA_integer_,cluster3=NA_real_,N40=NA_real_,
                       N45=NA_real_,N50=NA_real_,N=NA_real_)[0]



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  MD<-MD[cluster3==7]
  MD<-MD[,Clusterdiff:=ifelse(cluster3==7,1,0)]
  
  HighEngle1 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (TOriginalFoodExpenditure/Total_Exp_Month) > 0.4,
                    .(.N),by=.(Region,cluster3)]
  names(HighEngle1)<-c("Region", "cluster3","N40")
  
  HighEngle2 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (TOriginalFoodExpenditure/Total_Exp_Month) > 0.45,
                    .(.N),by=.(Region,cluster3)]
  names(HighEngle2)<-c("Region", "cluster3","N45")
  
  HighEngle3 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (TOriginalFoodExpenditure/Total_Exp_Month) > 0.5,
                    .(.N),by=.(Region,cluster3)]
  names(HighEngle3)<-c("Region", "cluster3","N50")
  
  HighEngle4 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine,
                    .(.N),by=.(Region,cluster3)]
  
  HighEngle<-merge(HighEngle1,HighEngle2)
  HighEngle<-merge(HighEngle,HighEngle3)
  HighEngle<-merge(HighEngle,HighEngle4)
  
  #MD<-MD[,Durable_Exp_mean:=weighted.mean(Durable_Exp,Weight),by=.(Region,NewArea_Name)]
  #MD<-MD[,Durable_Exp_median:=weighted.median(Durable_Exp,Weight),by=.(Region,NewArea_Name)]
  #MD<-MD[,Durable_Exp_mean_Per:=weighted.mean(Durable_Exp/EqSizeOECD,Weight),by=.(Region,NewArea_Name)]
  #MD<-MD[,Durable_Exp_median_Per:=weighted.median(Durable_Exp/EqSizeOECD,Weight),by=.(Region,NewArea_Name)]
  #MD[,mean(Durable_Exp_median_Per),by=.(Region,NewArea_Name)][order(Region,V1)]
  #EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
   #              TOriginalFoodExpenditure_Per<1.2*FPLine,
   #            .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month_nondurable,Weight),
   #              FPLine=mean(FPLine)),by=.(Region,cluster3)]
  
  EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                 TOriginalFoodExpenditure_Per<1.2*FPLine,
               .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month_nondurable,Weight),
                 FPLine=mean(FPLine),
                 DurableMedical_to_NonDurable_mean_Share=weighted.mean((Durable_Exp+Medical_Exp)/Total_Exp_Month_nondurable,Weight),
                 Durable_to_NonDurable_mean_Share=weighted.mean(Durable_Exp/Total_Exp_Month_nondurable,Weight),
                 Durable_to_NonDurable_median_Share=weighted.median(Durable_Exp/Total_Exp_Month_nondurable,Weight),
                 Durable_Exp_mean=weighted.mean(Durable_Exp,Weight),
                 Durable_Exp_median=weighted.median(Durable_Exp,Weight),
                 Durable_Exp_mean_Per=weighted.mean(Durable_Exp/EqSizeOECD,Weight),
                 Durable_Exp_median_Per=weighted.median(Durable_Exp/EqSizeOECD,Weight)),by=.(Region,cluster3)]
  
  ggplot(EngleD, aes(fill=factor(cluster3), y=Durable_to_NonDurable_mean_Share, x=cluster3)) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + ylim(0,0.55)
  
  
  EngleD[,PovertyLine_NonDurable:=FPLine/Engel]
  EngleD[,PovertyLine:=PovertyLine_NonDurable*(1+Durable_to_NonDurable_mean_Share)]
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,Engel)],by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  
  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per)/PovertyLine)^2]
  
  ################High Engle##################
  
  X1 <- HighEngle
  
  X1[,Year:=year]
  
  HighEngles <- rbind(HighEngles,X1)
  
  ################Country##################
  
  X1 <- MD[,.(PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Engle=weighted.mean(HHEngle,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight))]
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
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=cluster3]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=cluster3]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","cluster3"))
  FinalClusterResults <- rbind(FinalClusterResults,X)
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)],"\t")
  cat(MD[, weighted.mean(PovertyLine,Weight*Size)])
  
  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
  
  Poors<-MD[FinalPoor==1]
  
  a<-MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
           TOriginalFoodExpenditure_Per<1.2*FPLine &
           cluster3==7,
         .(x=TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  MD[as.numeric(Decile)<2,weighted.mean(FinalPoor,Weight)]
  
  MD5<-MD[ProvinceCode!=11]
  MD5<-MD5[,Group:=ifelse(FinalPoor==0 & FinalFoodPoor==0,"NoPoor",
                          ifelse(FinalPoor==1 & FinalFoodPoor==0,"OnlyFinalPoor",
                                 ifelse(FinalPoor==0 & FinalFoodPoor==1,"OnlyFoodPoor","Both")))]
  a<-ggplot(MD5,aes(x=HHEngle, fill=factor(Group))) +
    geom_density(alpha=0.25)+
    xlim(0,1)+ylim(0,5)+
    ggtitle(year)
  plot(a)
  

  
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")