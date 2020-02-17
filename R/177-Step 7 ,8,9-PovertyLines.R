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
  

    MD[as.numeric(Decile)<2,weighted.mean(FinalPoor,Weight)]
    
    MD5<-MD[NewArea==11]
    MD5<-MD5[,Group:=ifelse(FinalPoor==0 & FinalFoodPoor==0,"NoPoor",
                          ifelse(FinalPoor==1 & FinalFoodPoor==0,"OnlyFinalPoor",
                           ifelse(FinalPoor==0 & FinalFoodPoor==1,"OnlyFoodPoor","Both")))]
    a<-ggplot(MD5,aes(x=HHEngle, fill=factor(Group))) + geom_density(alpha=0.25)+
      ggtitle(year)
    plot(a)
    
    MD[ProvinceCode==0,ProvinceName:="Markazi"]
    MD[ProvinceCode==1,ProvinceName:="Gilan"]
    MD[ProvinceCode==2,ProvinceName:="Mazandaran"]
    MD[ProvinceCode==3,ProvinceName:="Az_Sharghi"]
    MD[ProvinceCode==4,ProvinceName:="Az_Gharbi"]
    MD[ProvinceCode==5,ProvinceName:="Kermanshah"]
    MD[ProvinceCode==6,ProvinceName:="Khoozestan"]
    MD[ProvinceCode==7,ProvinceName:="Fars"]
    MD[ProvinceCode==8,ProvinceName:="Kerman"]
    MD[ProvinceCode==9,ProvinceName:="Khorasan_Razavi"]
    MD[ProvinceCode==10,ProvinceName:="Esfahan"]
    MD[ProvinceCode==11,ProvinceName:="Sistan"]
    MD[ProvinceCode==12,ProvinceName:="Kordestan"]
    MD[ProvinceCode==13,ProvinceName:="Hamedan"]
    MD[ProvinceCode==14,ProvinceName:="Chaharmahal"]
    MD[ProvinceCode==15,ProvinceName:="Lorestan"]
    MD[ProvinceCode==16,ProvinceName:="Ilam"]
    MD[ProvinceCode==17,ProvinceName:="Kohkilooye"]
    MD[ProvinceCode==18,ProvinceName:="Booshehr"]
    MD[ProvinceCode==19,ProvinceName:="Zanjan"]
    MD[ProvinceCode==20,ProvinceName:="Semnan"]
    MD[ProvinceCode==21,ProvinceName:="Yazd"]
    MD[ProvinceCode==22,ProvinceName:="Hormozgan"]
    MD[ProvinceCode==23,ProvinceName:="Tehran"]
    MD[ProvinceCode==24,ProvinceName:="Ardebil"]
    MD[ProvinceCode==25,ProvinceName:="Ghom"]
    MD[ProvinceCode==26,ProvinceName:="Ghazvin"]
    MD[ProvinceCode==27,ProvinceName:="Golestan"]
    MD[ProvinceCode==28,ProvinceName:="Khorasan_Shomali"]
    MD[ProvinceCode==29,ProvinceName:="Khorasan_Jonoobi"]
    MD[ProvinceCode==30,ProvinceName:="Alborz"]
    
    
    MDD<-MD[,.(Population=sum(Weight*Size)), by=.(Decile,ProvinceName)][order(ProvinceName,Decile)]
      ggplot(MDD, aes(fill=factor(Decile), y=Population, x=ProvinceName)) + 
      geom_bar(position="fill", stat="identity") + theme_bw() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

    
    
    Decile1<-MD[as.numeric(Decile)<2,.(HHID,Region,ProvinceName,NewArea2,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile1Poors<-Decile1[,.(Decile1Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile1Poors, aes( y=Decile1Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    Decile2<-MD[as.numeric(Decile)<3,.(HHID,Region,ProvinceName,NewArea2,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile2Poors<-Decile2[,.(Decile2Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile2Poors, aes( y=Decile2Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    Decile5<-MD[as.numeric(Decile)>3,.(HHID,Region,ProvinceName,NewArea2,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
    Decile5Poors<-Decile5[,.(Decile5Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
    ggplot(Decile5Poors, aes( y=Decile5Poors, x=ProvinceName)) + 
      geom_bar(position="dodge", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    MDP<-MD[FinalPoor==1,.(Population=sum(Weight*Size)), by=.(Decile,ProvinceName)][order(ProvinceName,Decile)]
    ggplot(MDP, aes(fill=factor(Decile), y=Population, x=ProvinceName)) + 
      geom_bar(position="fill", stat="identity") + theme_bw() +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    

    
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
MD<-merge(MD,job,by=c("HHID","Region","HActivityState"))
Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Pub==9,Weight),
           by=ProvinceCode]

Prvjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv==9,Weight),
           by=ProvinceCode]

Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv<3 & Job_Main_Code_Prv>0,Weight)+
             weighted.mean(Job_Main_Code_Pub<3 & Job_Main_Code_Pub>0,Weight),by=ProvinceCode]

sub_share<-MD[FinalPoor==1,.(HHID,Region,ProvinceCode,Subsidy,Decile,Decile_Nominal,
                             sub_share=Subsidy/(12*Total_Exp_Month_nondurable))]

sub_share2<-MD[FinalPoor==1,.( sub_share=weighted.mean(Subsidy/(12*Total_Exp_Month_nondurable),Weight),
                               Edu=weighted.mean(HEduYears,Weight),
                               NKids=weighted.mean(NKids,Weight),
                               OtherExp=weighted.mean((Total_Exp_Month-OriginalFoodExpenditure-ServiceExp)/Total_Exp_Month,Weight)),by=ProvinceCode]




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")