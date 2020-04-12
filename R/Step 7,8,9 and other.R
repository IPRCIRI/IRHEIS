# Step 7,8,9 and other
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
  cat(MD[, weighted.mean(PovertyLine,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FPLine,Weight*Size)],"\t")
  cat(MD[, sum(Weight*Size)],"\t")
  
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
  
  
  
  MDD<-MD[,.(Population=sum(Weight*Size)), by=.(Decile,ProvinceName)][order(ProvinceName,Decile)]
  ggplot(MDD, aes(fill=factor(Decile), y=Population, x=ProvinceName)) + 
    geom_bar(position="fill", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  
  Decile1<-MD[as.numeric(Decile)<2,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
  Decile1Poors<-Decile1[,.(Decile1Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
  ggplot(Decile1Poors, aes( y=Decile1Poors, x=ProvinceName)) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  Decile2<-MD[as.numeric(Decile)<3,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
  Decile2Poors<-Decile2[,.(Decile2Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
  ggplot(Decile2Poors, aes( y=Decile2Poors, x=ProvinceName)) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  Decile5<-MD[as.numeric(Decile)>3,.(HHID,Region,ProvinceName,NewArea_Name,Weight,FinalFoodPoor,FinalPoor,Durable_Exp)]
  Decile5Poors<-Decile5[,.(Decile5Poors=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")][order(ProvinceName)]
  ggplot(Decile5Poors, aes( y=Decile5Poors, x=ProvinceName)) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  MDP<-MD[FinalPoor==1,.(Population=sum(Weight*Size)), by=.(Decile,cluster3)][order(cluster3,Decile)]
  ggplot(MDP, aes(fill=factor(Decile), y=Population, x=cluster3)) + 
    geom_bar(position="fill", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  MD[,weighted.mean(FinalPoor,Weight),by=ProvinceCode][order(ProvinceCode)]
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
#MD<-merge(MD,job,by=c("HHID","Region","HActivityState"))
#Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Pub==9,Weight),
#           by=ProvinceCode]

#Prvjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv==9,Weight),
#           by=ProvinceCode]

#Pubjob<-MD[FinalPoor==1,weighted.mean(Job_Main_Code_Prv<3 & Job_Main_Code_Prv>0,Weight)+
#             weighted.mean(Job_Main_Code_Pub<3 & Job_Main_Code_Pub>0,Weight),by=ProvinceCode]

#sub_share<-MD[FinalPoor==1,.(HHID,Region,ProvinceCode,Subsidy,Decile,Decile_Nominal,
#                             sub_share=Subsidy/(12*Total_Exp_Month_nondurable))]

#sub_share2<-MD[FinalPoor==1,.( sub_share=weighted.mean(Subsidy/(12*Total_Exp_Month_nondurable),Weight),
#                               Edu=weighted.mean(HEduYears,Weight),
#                               NKids=weighted.mean(NKids,Weight),
#                               OtherExp=weighted.mean((Total_Exp_Month-OriginalFoodExpenditure-ServiceExp)/Total_Exp_Month,Weight)),by=ProvinceCode]

###Durable- Cluster
DExp<-MD[,.(Durable_Exp_Per=weighted.mean(Durable_Exp/EqSizeOECD,Weight)),by="cluster3"]
ggplot(DExp, aes(fill=factor(cluster3), y=Durable_Exp_Per, x=cluster3)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

DSale<-MD[,.(Durable_Sale_Per=weighted.mean(Durable_Sale/EqSizeOECD,Weight)),by="cluster3"]
ggplot(DSale, aes(fill=factor(cluster3), y=Durable_Sale_Per, x=cluster3)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

#DPure<-MD[,.(Durable_Pure_Exp_Per=weighted.mean(Durable_Pure_Exp/EqSizeOECD,Weight)),by="cluster3"]
#ggplot(DPure, aes(fill=factor(cluster3), y=Durable_Pure_Exp_Per, x=cluster3)) + 
#  geom_bar(position="dodge", stat="identity") + theme_bw() +
#  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

###Durables share- Cluster
DExp<-MD[,.(Durable_Exp_Share=weighted.mean(Durable_Exp/Total_Exp_Month,Weight)),by="cluster3"]
ggplot(DExp, aes(fill=factor(cluster3), y=Durable_Exp_Share, x=cluster3)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

DSale<-MD[,.(Durable_Sale_Share=weighted.mean(Durable_Sale/Total_Exp_Month,Weight)),by="cluster3"]
ggplot(DSale, aes(fill=factor(cluster3), y=Durable_Sale_Share, x=cluster3)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

#DPure<-MD[,.(Durable_Pure_Exp_Share=weighted.mean(Durable_Pure_Exp/Total_Exp_Month,Weight)),by="cluster3"]
#ggplot(DPure, aes(fill=factor(cluster3), y=Durable_Pure_Exp_Share, x=cluster3)) + 
#  geom_bar(position="dodge", stat="identity") + theme_bw() +
#  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

#DRatio<-MD[,.(Durable_Exp_Ratio=weighted.mean(Durable_Exp/Total_Exp_Month_nondurable,Weight)),by="cluster3"]
#ggplot(DRatio, aes(fill=factor(cluster3), y=Durable_Pure_Exp_Ratio, x=cluster3)) + 
#  geom_bar(position="dodge", stat="identity") + theme_bw() +
#  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

####Urban
x2<-MD[Region=="Urban",.(Durable_Exp_Per=weighted.mean(Durable_Exp/EqSizeOECD,Weight)),by="NewArea_Name"]
x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Exp)])
ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Exp)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


x2<-MD[Region=="Urban",.(Durable_Sale_Per=weighted.mean(Durable_Sale/EqSizeOECD,Weight)),by="NewArea_Name"]
x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Sale_Per)])
ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Sale_Per)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


#x2<-MD[Region=="Urban",.(Durable_Pure_Exp_Per=weighted.mean(Durable_Pure_Exp/EqSizeOECD,Weight)),by="NewArea_Name"]
#x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Pure_Exp_Per)])
#ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Pure_Exp_Per)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


####Rural
x2<-MD[Region=="Rural",.(Durable_Exp_Per=weighted.mean(Durable_Exp/EqSizeOECD,Weight)),by="NewArea_Name"]
x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Exp)])
ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Exp)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


x2<-MD[Region=="Rural",.(Durable_Sale_Per=weighted.mean(Durable_Sale/EqSizeOECD,Weight)),by="NewArea_Name"]
x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Sale_Per)])
ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Sale_Per)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


#x2<-MD[Region=="Rural",.(Durable_Pure_Exp_Per=weighted.mean(Durable_Pure_Exp/EqSizeOECD,Weight)),by="NewArea_Name"]
#x2$NewArea_Name <- factor(x2$NewArea_Name, levels = x2$NewArea_Name[order(x2$Durable_Pure_Exp_Per)])
#ggplot(x2, aes(x = x2$NewArea_Name, y = x2$Durable_Pure_Exp_Per)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


MD[FinalPoor==1,weighted.mean(Total_Exp_Month_Per,Weight),by="Region"]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per>0,
   sum((PovertyLine-Total_Exp_Month_Per)*Size*Weight),by="Region"]

MD[FinalPoor==1,
   sum((PovertyLine-Total_Exp_Month_Per)*Size*Weight)]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per_nondurable>0,
   sum((PovertyLine-Total_Exp_Month_Per_nondurable)*Size*Weight),by="Region"]

MD[FinalPoor==1 & PovertyLine-Total_Exp_Month_Per_nondurable>0,
   sum((PovertyLine-Total_Exp_Month_Per_nondurable)*Size*Weight)]

MD[NewArea==2301,sum(Weight*Size)]

MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)][order(Region)]

MD[,weighted.mean(Bundle_Value,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(Bundle_Value,Weight),by=.(Region)][order(Region)]

MD[,weighted.mean(HHEngle,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(HHEngle,Weight),by=.(Decile)][order(Decile)]
MD[,weighted.mean(HHEngle,Weight),by=.(Region)][order(Region)]

load(file = paste0(Settings$HEISProcessedPath,"Y",year,"RetirementWage.rda"))
MD<-merge(MD,RetirementWageData,by="HHID",all.x = TRUE)

RT<-MD[,.(HHID,Region,HAge,HActivityState,Decile,retirement,Weight)]
RT[is.na(RT)] <- 0
RT[,Old:=ifelse(retirement>0 & HAge>48,1,0)]
RT[,weighted.mean(Old,Weight)]
RT[,weighted.mean(Old,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old,Weight),by=Region][order(Region)]

RT[,Old1:=ifelse(retirement>0 & HAge>48 & 
                   (PubWageNetIncomeY==0 & PrvWageNetIncomeY==0 & 
                      BussNetIncomeY==0 & AgriNetIncomeY==0),1,0)]
RT[,weighted.mean(Old1,Weight)]
RT[,weighted.mean(Old1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old1,Weight),by=Region][order(Region)]

RT[,Old2:=ifelse(retirement>0 & HAge>48 & HActivityState!="Employed" &
                   (PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                      BussNetIncomeY>0 | AgriNetIncomeY>0),1,0)]
RT[,weighted.mean(Old2,Weight)]
RT[,weighted.mean(Old2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old2,Weight),by=Region][order(Region)]

RT[,Old3:=ifelse(retirement>0 & HAge>48 & HActivityState=="Employed" &
                   (PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                      BussNetIncomeY>0 | AgriNetIncomeY>0),1,0)]
RT[,weighted.mean(Old3,Weight)]
RT[,weighted.mean(Old3,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(Old3,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(Old3,Weight),by=Region][order(Region)]


load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
RT<-merge(RT,TotalDurable[,.(HHID,Premium_gheyredarmani_mostakhdem,
                             Premium_gheyredarmani_karfarma,Premium_retirement_mostakhdem,
                             Premium_retirement_karfarma,Premium_retirement_general,
                             Premium_retirement_Rural_Household,Premium_retirement_Rural_Govern,
                             Premium_retirement_bank)])

RT[,pub1:=ifelse(HActivityState=="Employed" &
                   PubWageNetIncomeY>0 ,1,0)]
RT[,weighted.mean(pub1,Weight)]
RT[,weighted.mean(pub1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(pub1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(pub1,Weight),by=Region][order(Region)]


RT[,pub2:=ifelse(PubWageNetIncomeY>0 ,1,0)]
RT[,weighted.mean(pub2,Weight)]
RT[,weighted.mean(pub2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(pub2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(pub2,Weight),by=Region][order(Region)]


load(file = paste0(Settings$HEISProcessedPath,"Y",year,"PubWage.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"PrvWages.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BussIncome.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"AgriWages.rda"))

RT<-merge(RT,PubWageData[,.(HHID,PubWageNetIncomeY)],all.x = TRUE)
RT<-merge(RT,PrvWageData[,.(HHID,PrvWageNetIncomeY)],all.x = TRUE)
RT<-merge(RT,BussIncomeData[,.(HHID,BussNetIncomeY)],all.x = TRUE)
RT<-merge(RT,AgriIncomeData[,.(HHID,AgriNetIncomeY)],all.x = TRUE)
RT[is.na(RT)] <- 0

a<-RT[AgriNetIncomeY!=0,.(HHID,Region,AgriNetIncomeY,
                          Premium_gheyredarmani_mostakhdem,Weight)]
a[,weighted.mean(Premium_gheyredarmani_mostakhdem>0,Weight),by=Region]

b<-RT[BussNetIncomeY!=0,.(HHID,Region,BussNetIncomeY,
                          Premium_gheyredarmani_mostakhdem,Weight)]
b[,weighted.mean(Premium_gheyredarmani_mostakhdem>0,Weight),by=Region]

RT[,P:=ifelse(HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]

RT[,P:=ifelse(HActivityState=="Income without Work",1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]


RT[,P:=ifelse(PubWageNetIncomeY>0 | PrvWageNetIncomeY>0 | 
                BussNetIncomeY>0 | AgriNetIncomeY>0,1 ,0)]
RT[,weighted.mean(P,Weight)]
RT[,weighted.mean(P,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P,Weight),by=Region][order(Region)]


RT[,P1:=ifelse((Premium_gheyredarmani_mostakhdem>0 |
                  Premium_gheyredarmani_karfarma>0) & 
                 HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P1,Weight)]
RT[,weighted.mean(P1,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P1,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P1,Weight),by=Region][order(Region)]

RT[,P2:=ifelse(Premium_gheyredarmani_karfarma>0 & HActivityState=="Employed",1 ,0)]
RT[,weighted.mean(P2,Weight)]
RT[,weighted.mean(P2,Weight),by=.(Region,Decile)][order(Region,Decile)]
RT[,weighted.mean(P2,Weight),by=Decile][order(Decile)]
RT[,weighted.mean(P2,Weight),by=Region][order(Region)]

RT[,P3:=ifelse(Premium_retirement_mostakhdem>0,1 ,0)]
RT[,weighted.mean(P3,Weight)]
RT[,weighted.mean(P3,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P4:=ifelse(Premium_retirement_karfarma>0,1 ,0)]
RT[,weighted.mean(P4,Weight)]
RT[,weighted.mean(P4,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P5:=ifelse(Premium_retirement_general>0,1 ,0)]
RT[,weighted.mean(P5,Weight)]
RT[,weighted.mean(P5,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P6:=ifelse(Premium_retirement_Rural_Household>0,1 ,0)]
RT[,weighted.mean(P6,Weight)]
RT[,weighted.mean(P6,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P7:=ifelse(Premium_retirement_Rural_Govern>0,1 ,0)]
RT[,weighted.mean(P7,Weight)]
RT[,weighted.mean(P7,Weight),by=.(Region,Decile)][order(Region,Decile)]

RT[,P8:=ifelse(Premium_retirement_bank>0,1 ,0)]
RT[,weighted.mean(P8,Weight)]
RT[,weighted.mean(P8,Weight),by=.(Region,Decile)][order(Region,Decile)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")