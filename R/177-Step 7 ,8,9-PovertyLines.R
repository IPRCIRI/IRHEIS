# 177- Step 7,8,9-Poverty Line.R
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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))
  
  MD<-merge(MD,Value,by="HHID")
  
  A1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight),by=cluster3]
  A2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Auto2_rani>0 | Auto1_Khareji>0 | 
                               Auto2_Khareji>0 | Auto1_Irani>0),
           weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight)]
  
  B1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight),by=cluster3]
  B2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (TV_Rangi_Irani>0 | TV_Rangi_Khareji>0),weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]
  
  C1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(freezer2,Weight),by=cluster3]
  C2<-    MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (freezer2>0),weighted.mean(freezer2,Weight)]
  
  D1<-   MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                           weighted.mean(OjaghGaz,Weight),by=cluster3]
  D2<-    MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (OjaghGaz>0),weighted.mean(OjaghGaz,Weight)]
  
  E1<-    MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                            weighted.mean(Mashin_Lebasshooyi,Weight),by=cluster3]
  E2<-    MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (Mashin_Lebasshooyi>0),weighted.mean(Mashin_Lebasshooyi,Weight)]
  
  F1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Mobile,Weight),by=cluster3]
  F2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Mobile>0),weighted.mean(Mobile,Weight)]
  
  G1<- MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                         weighted.mean(Cooler_Gaz,Weight),by=cluster3]
  G2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Cooler_Gaz>0),weighted.mean(Cooler_Gaz,Weight)]
  
  H1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(PC,Weight),by=cluster3]
  H2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (PC>0),weighted.mean(PC,Weight)]
  
  I1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Lastik_Mashin,Weight),by=cluster3]
  I2<- MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                           (Lastik_Mashin>0),weighted.mean(Lastik_Mashin,Weight)]
  
  J1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Motor_Machin,Weight),by=cluster3]
  J2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Motor_Machin>0),weighted.mean(Motor_Machin,Weight)]
  
  K1<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Tamirat_Asasi,Weight),by=cluster3]
  K2<-  MD[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Tamirat_Asasi>0),weighted.mean(Tamirat_Asasi,Weight)]
  
  MD[,Added_Dep:=0.1*(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani)+
       0.033*(TV_Rangi_Irani>0 | TV_Rangi_Khareji>0)+
       0.033*(freezer2)+
       0.033*(OjaghGaz)+
       0.033*(Mashin_Lebasshooyi)+
       0.11*(Mobile)+
       0.05*(Cooler_Gaz)+
       0.06*(PC)+
       0.5*(Lastik_Mashin)+
       0.033*(Motor_Machin)+
       0.05*(Tamirat_Asasi) ]
  
  
  names(A1)<-c("cluster3","A1")
  names(B1)<-c("cluster3","B1")
  names(C1)<-c("cluster3","C1")
  names(D1)<-c("cluster3","D1")
  names(E1)<-c("cluster3","E1")
  names(F1)<-c("cluster3","F1")
  names(G1)<-c("cluster3","G1")
  names(H1)<-c("cluster3","H1")
  names(I1)<-c("cluster3","I1")
  names(J1)<-c("cluster3","J1")
  names(K1)<-c("cluster3","K1")
  
  
V<-merge(A1,B1,by="cluster3")
V<-merge(V,C1,by="cluster3")
V<-merge(V,D1,by="cluster3")
V<-merge(V,E1,by="cluster3")
V<-merge(V,F1,by="cluster3")
V<-merge(V,G1,by="cluster3")
V<-merge(V,H1,by="cluster3")
V<-merge(V,I1,by="cluster3")
V<-merge(V,J1,by="cluster3")
V<-merge(V,K1,by="cluster3")
V[,Added:=A1+B1+C1+D1+E1+F1+G1+H1+I1+J1+H1+K1]
MD<-merge(MD,V,by="cluster3")

  EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                  TOriginalFoodExpenditure_Per<1.2*FPLine,
               .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month_nondurable,Weight),
                 FPLine=mean(FPLine),
                 Added=mean(Added)),by=.(Region,cluster3)]


  
  EngleD[,PovertyLine:=FPLine/Engel]
  EngleD[,PovertyLine2:=PovertyLine+Added]
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,PovertyLine2,Engel)],by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per_nondurable+Added_Dep < PovertyLine2,1,0 )]
  MD<-MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  MD[,weighted.mean(Added,Weight)]
  MD[,weighted.mean(Added,Weight),by=Region]
  
  MD[,weighted.mean(PovertyLine2,Weight)]
  MD[,weighted.mean(PovertyLine2,Weight),by=Region]

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
  cat(MD[, weighted.mean(PovertyLine2,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FPLine,Weight*Size)],"\t")
  cat(MD[, sum(Weight*Size)],"\t")

  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))

   

    MD[,weighted.mean(FinalPoor,Weight),by=ProvinceCode][order(ProvinceCode)]
}

MD[,sum(Weight),by=Decile][order(Decile)]
MD[,sum(Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,sum(Weight)]
MD[,sum(Weight*Size)]

# compare Engle & Engle_prime in 178
FinalClusterEngel <- FinalClusterResults[,.(Year,cluster3,FPLine,PovertyHCR)]
save(FinalClusterEngel,file=paste0(Settings$HEISProcessedPath,"FINALPOORS_normal.rda"))

#ggplot(HighEngles)+
#  geom_line(mapping = aes(x=Year,y=N50/N,col=factor(cluster3)))

#ggplot(FinalClusterResults)+
#  geom_line(mapping = aes(x=Year,y=PovertyHCR,col=factor(cluster3)))

#write.csv(FinalClusterResults,file = FinalClusterResults.csv)


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")