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

FinalClusterDiff <- data.table(Year=NA_integer_,cluster3=NA_integer_,MetrPrice=NA_real_,
                               House_Share=NA_real_,Food_Share=NA_real_,Durable_Share=NA_real_,
                               SampleSize=NA_integer_,Clusterdiff=NA_integer_,
                               Engle=NA_integer_,FPLine=NA_integer_,FoodKCaloriesHH_Per=NA_real_,
                               Total_Exp_Month_Per_nondurable=NA_real_,
                               Total_Exp_Month_Per=NA_real_,
                               PovertyLine=NA_real_,PovertyHCR=NA_real_,
                               PovertyGap=NA_real_,PovertyDepth=NA_real_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  #MD<-MD[cluster3==7]
  MD<-MD[,Clusterdiff:=ifelse(cluster3==7,1,0)]
  MD<-MD[,EngleH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  
  HighEngle1 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (EngleH) > 0.4,
                    .(.N),by=.(Region,cluster3)]
  
  HighEngle2 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (EngleH) > 0.45,
                    .(.N),by=.(Region,cluster3)]
  
  HighEngle3 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                      TOriginalFoodExpenditure_Per<1.2*FPLine &
                      (EngleH) > 0.5,
                    .(.N),by=.(Region,cluster3)]
  
  HighEngle<-rbind(HighEngle1,HighEngle2,HighEngle3)
  
  EngleD1 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                   TOriginalFoodExpenditure_Per<1.2*FPLine &
                   (EngleH) < 0.5 &
                   (cluster3!=6 & cluster3!=7 & cluster3!=11 &
                      cluster3!=12 & cluster3!=13),
                 .(.N,Engel=weighted.mean(EngleH,Weight),
                   FPLine=mean(FPLine)),by=.(Region,cluster3)]
  
  EngleD2 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                   TOriginalFoodExpenditure_Per<1.2*FPLine &
                   (EngleH) < 0.45 &
                   (cluster3==6 | cluster3==7 | cluster3==11 |
                   cluster3==12 | cluster3==13),
                   .(.N,Engel=weighted.mean(EngleH,Weight),
                   FPLine=mean(FPLine)),by=.(Region,cluster3)]
  
  
  EngleD<-rbind(EngleD1,EngleD2)
  EngleD<-EngleD[,Year:=year]
  EngleDD <- EngleD
  EngleDD <- subset(EngleDD, select = c(Year,cluster3,Engel,Region))
  save(EngleDD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleDD.rda"))
  save(EngleD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleD.rda"))
}
for(year in (84:Settings$endyear)){
  
  #year=97
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  MD<-MD[,Clusterdiff:=ifelse(cluster3==7,1,0)]
  MD<-MD[,EngleH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
    cat(paste0("\nYear:",year,"\t"))
  nyear=year+1
  if(Settings$endyear==year){nyear=year}
  load(file=paste0(Settings$HEISProcessedPath,"Y",nyear,"EngleDD.rda"))
  EngleND <- EngleDD
  colnames(EngleND)[colnames(EngleND) == 'Engel'] <- 'Engeln'
  colnames(EngleND)[colnames(EngleND) == 'Year'] <- 'Yearn'
  nnyear=year+2
  if(year >Settings$endyear-1){nnyear=year}
  load(file=paste0(Settings$HEISProcessedPath,"Y",nnyear,"EngleDD.rda"))
  EngleNND <- EngleDD
  colnames(EngleNND)[colnames(EngleNND) == 'Engel'] <- 'Engelnn'
  colnames(EngleNND)[colnames(EngleNND) == 'Year'] <- 'Yearnn'
  
  pyear=year-2
  load(file=paste0(Settings$HEISProcessedPath,"Y",pyear,"EngleDD.rda"))
  EnglePPD <- EngleDD
  colnames(EnglePPD)[colnames(EnglePPD) == 'Engel'] <- 'Engelpp'
  colnames(EnglePPD)[colnames(EnglePPD) == 'Year'] <- 'Yearpp'
  pyear=year-1
  load(file=paste0(Settings$HEISProcessedPath,"Y",pyear,"EngleDD.rda"))
  EnglePD <- EngleDD
  colnames(EnglePD)[colnames(EnglePD) == 'Engel'] <- 'Engelp'
  colnames(EnglePD)[colnames(EnglePD) == 'Year'] <- 'Yearp'
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleDD.rda"))
  
  
  EngleMD<-merge(EnglePPD,EnglePD)
  EngleMD<-merge(EngleMD,EngleDD)
  EngleMD<-merge(EngleMD,EngleND)
  EngleMD<-merge(EngleMD,EngleNND)
  
  
  EngleMD <- EngleMD[,Engelm:=ifelse(year==97,(Engel+Engelp+Engelpp)/3,
                              ifelse(year==96,(Engel+Engelp+Engelpp+Engeln)/4,
                                     (Engel+Engelp+Engelpp+Engelnn+Engeln)/5))]
  EngleMD <- subset(EngleMD, select = c(cluster3,Engelm,Region,Year))
  
  
  EngleMD[,Engelm:=     ifelse(Year==84,Engelm*0.9937,
                        ifelse(Year==85,Engelm*0.9938,
                        ifelse(Year==86,Engelm*0.9936,
                        ifelse(Year==87,Engelm*1.0012,
                        ifelse(Year==88,Engelm*0.9963,
                        ifelse(Year==89,Engelm*0.9898,
                        ifelse(Year==90,Engelm*0.9664,
                        ifelse(Year==91,Engelm*1.0269,
                        ifelse(Year==92,Engelm*1.0250,
                        ifelse(Year==93,Engelm*1.0080,
                        ifelse(Year==94,Engelm*1.0006,
                        ifelse(Year==95,Engelm*0.9996,
                        ifelse(Year==96,Engelm*1.0000,
                        ifelse(Year==97,Engelm*0.9448,0))))))))))))))]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleD.rda"))
    EngleD  <- subset(EngleD,select = c(1,2,3,5,6))
  
    EngleMD <- EngleMD[,Engel:=Engelm]
  EngleDD <- subset(EngleMD, select = c(cluster3,Engel,Region))
  
  EngleD<-merge(EngleD,EngleDD,by=c("cluster3","Region"))
  
  EngleD[,PovertyLine:=FPLine/Engel]
  
  
  
  MD <- merge(MD,EngleD[,.(cluster3,Region,Engel,PovertyLine)],by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD<-MD[,HHEngle:=EngleH,Weight]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS_Engle.rda"))
  
  
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
              Engle=weighted.mean(EngleH,Weight),
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
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)])
  
  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS_Engle.rda"))
  
  Poors<-MD[FinalPoor==1]
  
  a<-MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
           TOriginalFoodExpenditure_Per<1.2*FPLine &
           cluster3==7,
         .(x=EngleH)]
  
}
FinalClusterEngelPrime <- FinalClusterResults[,.(Year,cluster3,FPLine,PovertyHCR)]
colnames(FinalClusterEngelPrime)[colnames(FinalClusterEngelPrime) == 'FPLine'] <- 'FPLine_p'
colnames(FinalClusterEngelPrime)[colnames(FinalClusterEngelPrime) == 'PovertyHCR'] <- 'PovertyHCR_p'

load(file=paste0(Settings$HEISProcessedPath,"FINALPOORS_normal.rda"))
FinalClusterEngelPrime <- merge(FinalClusterEngelPrime,
                                FinalClusterEngel[,.(Year,cluster3,FPLine,PovertyHCR)])


library(xlsx)
write.xlsx(FinalClusterEngelPrime, "E:/FinalClusterEngelPrime.xlsx")
ggplot(FinalClusterResults)+
  geom_line(mapping = aes(x=Year,y=MetrPrice,col=factor(cluster3),line=factor(cluster3)))

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")