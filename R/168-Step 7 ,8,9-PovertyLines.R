# 168- Step 8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
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


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS_Engle.rda"))
  
 # MD<-MD[Region=="Urban"]

 # EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
 #               TOriginalFoodExpenditure_Per<1.2*FPLine,
 #              .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month_nondurable,Weight),
  #               FPLine=mean(FPLine)),by=.(Region,cluster3)]


  EngleD[,EngleFinal:=mean(EngleFinal)]
  EngleD[,PovertyLine:=FPLine/EngleFinal]
  EngleD[,PovertyLine2:=FPLine/Engel]
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,PovertyLine2,Engel)],by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine,1,0 )]
  MD[,FinalPoor2:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine2,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  


  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per)/PovertyLine)^2]
  

  
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
  cat(MD[, weighted.mean(FinalPoor2,Weight*Size)],"\t")
  cat(MD[, weighted.mean(PovertyLine,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FPLine,Weight*Size)],"\t")
  #cat(MD[, sum(Weight*Size)],"\t")

  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))

  MD[, weighted.mean(FinalPoor,Weight*Size)]
  MD[, weighted.mean(FinalPoor,Weight*Size),by=Region]
  MD[, weighted.mean(FinalPoor,Weight*Size),by=cluster3]
  MD[Region=="Rural",weighted.mean(FinalPoor,Weight),by=ProvinceName][order(ProvinceName)]
    
    
  DurableD<- MD[ Total_Exp_Month_Per_nondurable>0.8*PovertyLine &
                   Total_Exp_Month_Per_nondurable<1.2*PovertyLine,
               .(.N,Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight),
                 PovertyLine=mean(PovertyLine)),
               by=.(Region,cluster3)]
  
  DurableD[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]
  MD <- merge(MD,DurableD[,.(cluster3,Region,PovertyLine_Final)],by=c("Region","cluster3"))
  
  cat(MD[, weighted.mean(PovertyLine_Final,Weight*Size)])
  MD[FinalPoor==1,weighted.mean(Total_Exp_Month_Per,Weight)]
  MD[FinalPoor==1,weighted.mean(Total_Exp_Month_Per,Weight),by=Region]
  
}

#ggplot(FinalClusterResults)+
 # geom_line(mapping = aes(x=Year,y=PovertyHCR,col=factor(cluster3),linetype=factor(cluster3)))


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")