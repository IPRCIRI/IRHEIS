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

HighEngles<-data.table(Year=NA_integer_,Region=NA_integer_,cluster3=NA_real_,N40=NA_real_,
                    N45=NA_real_,N50=NA_real_,N=NA_real_)[0]



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  #MD<-MD[Region=="Rural"]
  #MD<-MD[cluster3==13]
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

  EngleD<- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                  TOriginalFoodExpenditure_Per<1.2*FPLine,
               .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,cluster3)]


  
  EngleD[,PovertyLine:=FPLine/Engel]
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
  
  y<-MD[,.(.N),by=c("Region","NewArea2")][order(Region,NewArea2)]
    x<-MD[as.numeric(Decile)==10,.(.N),by=c("Region","NewArea2")][order(Region,NewArea2)]
  z<-merge(x,y,by=c("Region","NewArea2"))
  z[,share:=N.x/N.y]
  
    MD[as.numeric(Decile)<2,weighted.mean(FinalPoor,Weight)]
    
    MD5<-MD[ProvinceCode==11,Group:=ifelse(FinalPoor==0 & FinalFoodPoor==0,"NoPoor",
                          ifelse(FinalPoor==1 & FinalFoodPoor==0,"OnlyFinalPoor",
                           ifelse(FinalPoor==0 & FinalFoodPoor==1,"OnlyFoodPoor","Both")))]
    a<-ggplot(MD5,aes(x=HHEngle, fill=factor(Group))) + geom_density(alpha=0.25)+
      ggtitle(year)
    
    plot(a)
    
    
    D1 <- MD[Decile==1 & Region=="Urban",.(N1=sum(Weight)),by=NewArea2]
    D2 <- MD[Decile==2 & Region=="Urban",.(N2=sum(Weight)),by=NewArea2]
    D3 <- MD[Decile==3 & Region=="Urban",.(N3=sum(Weight)),by=NewArea2]
    D4 <- MD[Decile==4 & Region=="Urban",.(N4=sum(Weight)),by=NewArea2]
    D5 <- MD[Decile==5 & Region=="Urban",.(N5=sum(Weight)),by=NewArea2]
    D6 <- MD[Decile==6 & Region=="Urban",.(N6=sum(Weight)),by=NewArea2]
    D7 <- MD[Decile==7 & Region=="Urban",.(N7=sum(Weight)),by=NewArea2]
    D8 <- MD[Decile==8 & Region=="Urban",.(N8=sum(Weight)),by=NewArea2]
    D9 <- MD[Decile==9 & Region=="Urban",.(N9=sum(Weight)),by=NewArea2]
    D10 <- MD[Decile==10 & Region=="Urban",.(N10=sum(Weight)),by=NewArea2]
    Dx <- MD[Region=="Urban",.(Nx=sum(Weight)),by=NewArea2]
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
}
# compare Engle & Engle_prime in 178
FinalClusterEngel <- FinalClusterResults[,.(Year,cluster3,FPLine,PovertyHCR)]
save(FinalClusterEngel,file=paste0(Settings$HEISProcessedPath,"FINALPOORS_normal.rda"))

ggplot(HighEngles)+
  geom_line(mapping = aes(x=Year,y=N50/N,col=factor(cluster3)))

ggplot(FinalClusterResults)+
  geom_line(mapping = aes(x=Year,y=PovertyHCR,col=factor(cluster3)))

#write.csv(FinalClusterResults,file = FinalClusterResults.csv)


Job<-MD[FinalPoor==1,weighted.mean(HActivityState=="Employed",Weight),by=ProvinceCode]
IncomeWithoutWork<-MD[FinalPoor==1,weighted.mean(HActivityState=="Income without Work",Weight),by=ProvinceCode]

Widow<-MD[FinalPoor==1,weighted.mean(HSex=="Female",Weight),by=ProvinceCode]
Widow2<-MD[HSex=="Female",weighted.mean(FinalPoor,Weight),by=ProvinceCode]

load(file=paste0(Settings$HEISProcessedPath,"Y",97,"job.rda"))
MD<-merge(MD,job,by="HHID")
Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Pub==9,Weight),
           by=ProvinceCode]

Prvjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv==9,Weight),
           by=ProvinceCode]

Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv<3 & Job_Main_Code_Prv>0,Weight)+
             weighted.mean(Job_Main_Code_Pub<3 & Job_Main_Code_Pub>0,Weight),by=ProvinceCode]

sub_share<-MD[FinalPoor==1,.(HHID,Region.x,ProvinceCode,Subsidy,Decile,Decile_Nominal,
                             sub_share=Subsidy/(12*Total_Exp_Month_nondurable))]

sub_share2<-MD[FinalPoor==1,.( sub_share=weighted.mean(Subsidy/(12*Total_Exp_Month_nondurable),Weight),
                               Edu=weighted.mean(HEduYears,Weight),
                               NKids=weighted.mean(NKids,Weight),
                               OtherExp=weighted.mean((Total_Exp_Month-OriginalFoodExpenditure-ServiceExp)/Total_Exp_Month,Weight)),by=ProvinceCode]




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")