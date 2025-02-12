# 178- Other related calculation.R
# 
# Copyright © 2020:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Other related calculation =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(sm)

FinalCountryResults <- data.table(Year=NA_integer_,PovertyLine=NA_real_,PovertyHCR=NA_real_,
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

ProvinceResults <- data.table(Year=NA_integer_,ProvinceCode=NA_integer_,
                              Total_Exp_Per=NA_real_,HSex=NA_character_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  #MD<-MD[cluster3==7]
  MD<-MD[,Clusterdiff:=ifelse(cluster3==7,1,0)]
  
  EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
                .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                  FPLine=mean(FPLine)),by=.(Region,cluster3)]
  
  
  EngleD[,PovertyLine:=FPLine/Engel]
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
  
  ################Cluster Diff##################
  X1 <- MD[,.(SampleSize=.N,MetrPrice=weighted.mean(MetrPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(ServiceExp/Total_Exp_Month,Weight),
              Food_Share=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
              Durable_Share=weighted.mean(Durable_Exp/Total_Exp_Month,Weight),
              Engle=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              cluster3=weighted.mean(cluster3,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),by=Clusterdiff]
  X2 <- MD[FinalPoor==1,.(PovertyGap=weighted.mean(FGT1M,Weight*Size),
                          PovertyDepth=weighted.mean(FGT2M,Weight*Size)),by=Clusterdiff]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","Clusterdiff"))
  FinalClusterDiff <- rbind(FinalClusterDiff,X)
  
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
  
  
  ################Province##################
  X1 <- MD[,.(Total_Exp_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm = TRUE)),by=.(ProvinceCode,HSex)]
 
    X1[,Year:=year]
  ProvinceResults <- rbind(ProvinceResults,X1)
  
  write.csv(ProvinceResults,file = "ProvinceResults.csv")
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)])
  #cat(MD[TOriginalFoodExpenditure_Per>0.8*FPLine &
  #         TOriginalFoodExpenditure_Per<1.2*FPLine &
  #        Region=="Rural" & NewArea==11,
  #      weighted.mean(Engel,Weight)])
  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
  
  Poors<-MD[FinalPoor==1]
  

  MDR<-MD[Region=="Rural"]
  #MDRP<-MDR[FinalPoor==1]
  MDRN<-MDR[TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine]
  a<-ggplot(MDRN,aes(x=HHEngle, fill=factor(cluster3))) + geom_density(alpha=0.25)+
     ggtitle(year)

#plot(a)
}
save(FinalClusterResults,file=paste0(Settings$HEISProcessedPath,"FinalClusterResults.rda"))
save(FinalCountryResults,file=paste0(Settings$HEISProcessedPath,"FinalCountryResults.rda"))

ggplot(FinalClusterResults)+
  geom_line(mapping = aes(x=Year,y=log(FoodKCaloriesHH_Per),col=factor(cluster3)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=MetrPrice,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=MetrPrice,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=FPLine,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=Engle,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=PovertyLine,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=House_Share,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=Food_Share,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=Durable_Share,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=Total_Exp_Month_Per_nondurable,col=factor(Clusterdiff)))

ggplot(FinalClusterDiff)+
  geom_line(mapping = aes(x=Year,y=Total_Exp_Month_Per,col=factor(Clusterdiff)))

MD7<-MD[cluster3==7]
plot(MD7$TOriginalFoodExpenditure_Per~MD7$HHEngle)
abline(h = MD7$FPLine*1.2, col="blue")
abline(h = MD7$FPLine*0.8, col="blue")
plot(MD7$Total_Exp_Month_Per~MD7$HHEngle)
abline(h = MD7$PovertyLine, col="blue")

plot(log(MD7$TOriginalFoodExpenditure_Per)~log(MD7$Total_Exp_Month_Per))
smoothScatter(log(MD$TOriginalFoodExpenditure_Per),log(MD$Total_Exp_Month_Per))
smoothScatter(log(MD$Total_Exp_Month_Per),(MD$TOriginalFoodExpenditure_Per/MD$Total_Exp_Month_Per))

Area7<-MD[,.(HHID,Size,Decile,Region,ProvinceCode,FinalPoor,Area,Weight)]
Area7[FinalPoor==1,weighted.mean(Area,Weight,na.rm = TRUE)]



MD77<-MD7[,.(.N,Total_Exp_Month_Per_nondurable=mean(Total_Exp_Month_Per_nondurable),
             HHEngle=mean(HHEngle)),by="Percentile"]
plot(MD77$HHEngle~MD77$Percentile)


###Engle distribution###
MDU<-MD[Region=="Urban"]
sm.density.compare(MDU$HHEngle, MDU$cluster3==7)

MDUP<-MDU[FinalPoor==1]
sm.density.compare(MDUP$HHEngle, MDUP$cluster3==7)

MDUN<-MDU[TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine]
sm.density.compare(MDUN$HHEngle, MDUN$cluster3==7)
sm.density.compare(MDUN$HHEngle, MDUN$cluster3)

MDR<-MD[Region=="Rural"]
sm.density.compare(MDR$HHEngle, MDR$cluster3==13)

MDRP<-MDR[FinalPoor==1]
sm.density.compare(MDRP$HHEngle, MDRP$cluster3==13)

MDRN<-MDR[TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine]
sm.density.compare(MDRN$HHEngle, MDRN$cluster3==13)

D1 <- MD[Decile==1 & Region=="Urban",.(N1=sum(Weight)),by=NewArea_Name]
D2 <- MD[Decile==2 & Region=="Urban",.(N2=sum(Weight)),by=NewArea_Name]
D3 <- MD[Decile==3 & Region=="Urban",.(N3=sum(Weight)),by=NewArea_Name]
D4 <- MD[Decile==4 & Region=="Urban",.(N4=sum(Weight)),by=NewArea_Name]
D5 <- MD[Decile==5 & Region=="Urban",.(N5=sum(Weight)),by=NewArea_Name]
D6 <- MD[Decile==6 & Region=="Urban",.(N6=sum(Weight)),by=NewArea_Name]
D7 <- MD[Decile==7 & Region=="Urban",.(N7=sum(Weight)),by=NewArea_Name]
D8 <- MD[Decile==8 & Region=="Urban",.(N8=sum(Weight)),by=NewArea_Name]
D9 <- MD[Decile==9 & Region=="Urban",.(N9=sum(Weight)),by=NewArea_Name]
D10 <- MD[Decile==10 & Region=="Urban",.(N10=sum(Weight)),by=NewArea_Name]
Dx <- MD[Region=="Urban",.(Nx=sum(Weight)),by=NewArea_Name]
Dxx <- merge(Dx,D1)
Dxx <- merge(Dxx,D2)
Dxx <- merge(Dxx,D3)
Dxx <- merge(Dxx,D4)
Dxx <- merge(Dxx,D5)
Dxx <- merge(Dxx,D6)
Dxx <- merge(Dxx,D7)
Dxx <- merge(Dxx,D8)
Dxx <- merge(Dxx,D9)
Dxx <- merge(Dxx,D10)
Dxx[,p1:=N1/Nx*100]
Dxx[,p2:=N2/Nx*100]
Dxx[,p3:=N3/Nx*100]
Dxx[,p4:=N4/Nx*100]
Dxx[,p5:=N5/Nx*100]
Dxx[,p6:=N6/Nx*100]
Dxx[,p7:=N7/Nx*100]
Dxx[,p8:=N8/Nx*100]
Dxx[,p9:=N9/Nx*100]
Dxx[,p10:=N10/Nx*100]

#write.csv(ProvinceResults,file = ProvinceResults.csv)

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
