#174-FindInitialPoor.R
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
#library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,
               ServiceExp,FoodExpenditure,Total_Exp_Month,
               NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
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
  
  Compare<-SMD[,.(Old=mean(PriceIndex2),
                  New=mean(PriceIndex)),by=.(Region,NewArea2)]

  SMD[,V1:=NULL]
  SMD[,V2:=NULL]
  SMD[,V:=NULL]
  
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  C<-SMD[,.(.N,Max=max(Total_Exp_Month_Per_nondurable),
           Min=min(Total_Exp_Month_Per_nondurable),
           Mean=mean(Total_Exp_Month_Per_nondurable)),
        by=.(Region,NewArea,NewArea2,Decile)]
  

  
  # FirstSMD<-SMD[,.(HHID,Region,NewArea2,NewArea2,Percentile,Decile)]
  # FirstSMD<-FirstSMD[,Realfirstpoor:=ifelse(Decile %in% 1:2,1,0)]
  # save(FirstSMD, file=paste0(Settings$HEISProcessedPath,"Y",year,"FirstSMD.rda"))
  

  
  B<-MD[,.(.N,Mean=mean(MetrPrice)),
        by=.(Region,NewArea2)]
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
    # Index2 <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
    #                                             weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2)/2)
    #                           ,by=.(Region,NewArea2)]
    
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
    
    SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
    SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
    
    #Calculate deciles by weights
    SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
    SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
    SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
    
    
    cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
  }
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  

  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea2,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

  MDD2<-MD[,.(HHID,Decile)]
  names(MDD2)<-c("HHID","Decile_F")
  save(MDD2,file = "MDD2.rda")
  
  #load( file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  #MD<-merge(MD,HHHouseProperties)
  
#MD[,weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
    #    by=.(Region,Decile)][order(Region,Decile)]
  
#MD[,weighted.mean(car=="True",Weight),
 #  by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(Cloth_Exp/Total_Exp_Month,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(Size,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(Region=="Urban",Weight),
   by=.(Decile)][order(Decile)]

MD[,weighted.mean(HActivityState=="Employed",Weight),
   by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(HEduYears>10,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(HEduYears>13,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

#MD[,weighted.mean(area/Size,Weight),
 #  by=.(Region,Decile)][order(Region,Decile)]

MD[,weighted.mean(Amusement_Exp/Total_Exp_Month,Weight),
   by=.(Region,Decile)][order(Region,Decile)]

  A<-MD[,.(.N,Max=max(Total_Exp_Month_Per_nondurable),
           Min=min(Total_Exp_Month_Per_nondurable),
           Mean=mean(Total_Exp_Month_Per_nondurable)),
        by=.(Region,NewArea,NewArea2,Decile)]
  
  D<-MD[,.(.N,Mean=weighted.median(Durable_Exp/Size,Weight)),
         by=.(Region,NewArea2,Decile)]
  
#write.csv(A,file = "A.csv")
#write.csv(B,file = "B.csv")
#write.csv(C,file = "C.csv")
#write.csv(D,file = "D.csv")
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
