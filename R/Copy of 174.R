#Copy of 174.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,ProvinceCode,
               ServiceExp,FoodExpenditure,Total_Exp_Month,
               NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               Durable_Exp,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
  #Real Prices
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                     +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea2)]
  
  X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
              weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea2)]
  X[,V:=V1+V2]
  
  SMD <- merge(SMD,X,by=c("Region","NewArea2"))
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2*V2)/V
      ,by=.(Region,NewArea2)]
  
  Compare1<-SMD[,.(Old=mean(PriceIndex2),
                   New=mean(PriceIndex)),by=.(Region,NewArea2)]
  
  SMD[,V1:=NULL]
  SMD[,V2:=NULL]
  SMD[,V:=NULL]
  
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  ###Real- Urban & Rural
  #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]  #Deciling in Urban- Rural
  #SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  #SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  #SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  ###Real- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  ###Nominal-  Urban & Rural
  #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable)]  #Deciling in Urban- Rural(Nominal)
  #SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  #SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  #SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  ###Nominal- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable)]  #Deciling in Country(Nominal)
  SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]
  
  
  # C<-SMD[,.(.N,Max=max(Total_Exp_Month_Per_nondurable),
  #           Min=min(Total_Exp_Month_Per_nondurable),
  #           Mean=mean(Total_Exp_Month_Per_nondurable)),
  #        by=.(Region,NewArea,NewArea2,Decile)]
  
  # D<-SMD[,.(.N,Mean=weighted.mean(Durable_Exp/Size,Weight)),
  #       by=.(Region,NewArea,NewArea2,Decile)]
  
  # B<-SMD[,.(.N,Mean=mean(MetrPrice)),
  #       by=.(Region,NewArea,NewArea2)]
  
  # write.csv(B,file = "B.csv")
  # write.csv(C,file = "C.csv")
  # write.csv(D,file = "D.csv")
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    SMDIterationPoor[,sum(ThisIterationPoor),by=.(Region,NewArea2)][order(Region,NewArea2)]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
    
    X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                             weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea2)]
    X[,V:=V1+V2]
    
    SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea2"))
    SMDIterationPoor[,PriceIndex:=NULL]  
    
    Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                                               weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2*V2)/V)
                              ,by=.(Region,NewArea2)]
    Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea2)]
    
    
    SMD[,PriceIndex:=NULL]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea2"))
    
    SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    
    
    ###Real- Urban & Rural
    #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]  #Deciling in Urban- Rural
    #SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
    #SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
    #SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
    
    ###Real- Country
    SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
    SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
    SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
    SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
    
    SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
    
    cat("\n",sum(SMD[ProvinceCode==2,.N]))
    #cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
  }
  
  
  
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile,Decile_Nominal,Percentile_Nominal)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  
  
  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea2,Region)]
  MD[,sum(Weight*Size), by=.(Decile,Region)][order(Region,Decile)]
  MD[,sum(Weight*Size), by=.(Decile_Nominal,Region)][order(Region,Decile_Nominal)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  ###Total
  MDG1<-MD[,.(Population=sum(Weight*Size)), by=.(Decile,Region)][order(Region,Decile)]
  ggplot(MDG1, aes(fill=Region, y=Population, x=Decile)) + 
    geom_bar(position="stack", stat="identity")
  
  ###Tehran
  MDG2<-MD[,Tehran:=ifelse(NewArea==2301,1,0)]
  MDG2<-MDG2[,.(Population=sum(Weight*Size)), by=.(Decile,Tehran)][order(Tehran,Decile)]
  ggplot(MDG2, aes(fill=factor(Tehran), y=Population, x=Decile)) + 
    geom_bar(position="stack", stat="identity")
  
  ###Sistan
  MDG3<-MD[,Sistan:=ifelse(ProvinceCode==11,1,0)]
  MDG3<-MDG3[,.(Population=sum(Weight*Size)), by=.(Decile,Sistan)][order(Sistan,Decile)]
  ggplot(MDG3, aes(fill=factor(Sistan), y=Population, x=Decile)) + 
    geom_bar(position="stack", stat="identity")
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  MD<-merge(MD,HHHouseProperties)
  
  FractioninData<-MD[,.(Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
                        tenure=weighted.mean(tenure=="OwnLandandBuilding",Weight),
                        HActivityState=weighted.mean(HActivityState=="Employed",Weight),
                        Car=weighted.mean(car=="True",Weight)),
                     by=c("Percentile_Nominal")]
  
  #write.csv(FractioninData,file = "FractioninData.csv")
  
  Deciles<-MD[,.(HHID,Decile,Decile_Nominal,Region,ProvinceCode,HIndivNo,Weight)]
  Deciles<-Deciles[,diff:=as.numeric(Decile)-as.numeric(Decile_Nominal)]
  decileTest<-Deciles[,.(Positive=weighted.mean(diff>0,Weight),
                         Zero=weighted.mean(diff==0,Weight),
                         Minus=weighted.mean(diff<0,Weight)),by=ProvinceCode]
  
  decile7<-Deciles[,.(before=weighted.mean(as.numeric(Decile_Nominal)>7,Weight),
                      after=weighted.mean(as.numeric(Decile)>7,Weight)),by=ProvinceCode]
  
  ###Exp
  x2<-FractioninData[,.(Total_Exp_Month_Per_nondurable,Percentile_Nominal)]
  x2$Percentile_Nominal <- factor(x2$Percentile_Nominal, levels = x2$Percentile_Nominal[order(x2$Percentile_Nominal)])
  ggplot(x2, aes(x = x2$Percentile_Nominal, y = x2$Total_Exp_Month_Per_nondurable)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  
  ###Tenure
  x2<-FractioninData[,.(tenure,Percentile_Nominal)]
  x2$Percentile_Nominal <- factor(x2$Percentile_Nominal, levels = x2$Percentile_Nominal[order(x2$tenure)])
  ggplot(x2, aes(x = x2$Percentile_Nominal, y = x2$tenure)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  
  ###HActivity
  x2<-FractioninData[,.(HActivityState,Percentile_Nominal)]
  x2$Percentile_Nominal <- factor(x2$Percentile_Nominal, levels = x2$Percentile_Nominal[order(x2$HActivityState)])
  ggplot(x2, aes(x = x2$Percentile_Nominal, y = x2$HActivityState)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  ###Car
  x2<-FractioninData[,.(Car,Percentile_Nominal)]
  x2$Percentile_Nominal <- factor(x2$Percentile_Nominal, levels = x2$Percentile_Nominal[order(x2$Car)])
  ggplot(x2, aes(x = x2$Percentile_Nominal, y = x2$Car)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  decile7<-Deciles[,.(before=weighted.mean(as.numeric(Decile_Nominal)>7,Weight),
                      after=weighted.mean(as.numeric(Decile)>7,Weight)),by=ProvinceCode]
  
  Deciles<-MD[,.(HHID,Decile,Decile_Nominal,Region,ProvinceCode,HIndivNo,Weight,
                 Total_Exp_Month,Total_Exp_Month_nondurable)]
  Deciles<-Deciles[,diff:=as.numeric(Decile)-as.numeric(Decile_Nominal)]
  decileTest<-Deciles[,.(Positive=weighted.mean(diff>0,Weight),
                         Zero=weighted.mean(diff==0,Weight),
                         Minus=weighted.mean(diff<0,Weight)),by=ProvinceCode]
  
  SMD[,TIncome:=sum(Total_Exp_Month_Per_nondurable*Weight)]
  Share<-SMD[,Income_Share:=Total_Exp_Month_Per_nondurable/TIncome]
  Share<-Share[,.(Income_Share=sum(Income_Share*Weight)),by=Decile]
  save(Share,file = "Share.rda")
  
  Deciles<-Deciles[,Low:=ifelse(as.numeric(Decile)<4,1,0)]
  Low<-Deciles[,.(weighted.mean(Low,Weight)),by=ProvinceCode]
  save(Low,file = "Low.rda")
  
  
  Deciles<-Deciles[,L5:=ifelse(Total_Exp_Month_nondurable<5000000,1,0)]
  L5<-Deciles[,.(sum(L5*Weight)),by=ProvinceCode]
  save(L5,file = "L5.rda")
  
  #UnEmp<-MD[,UnEmp:=ifelse(HActivityState=="Unemployed" & HAge<41,1,0)]
  #UnEmp<-UnEmp[,.(weighted.mean(UnEmp,Weight)),by=ProvinceCode]
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
