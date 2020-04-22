#174-FindInitialPoor (version 2).R
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
               NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeOECD,Area)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  SMD[,Bundle_Value_H:=MetrPrice*16]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  SMD <- SMD[Bundle_Value_H<=15000000] #arbitrary measures, TODO: check in diff years
  
  
  #Real Prices
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,Bundle_Value_H,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                     +weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea_Name)]
  
  X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
              weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]
  
  SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                    +weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)/TBV2*V2)/V
      ,by=.(Region,NewArea_Name)]
  
  Compare<-SMD[,.(Old=mean(PriceIndex2),
                  New=mean(PriceIndex)),by=.(Region,NewArea_Name)]
  
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
         by=.(Region,NewArea,NewArea_Name,Decile)]
  
  
  
  # FirstSMD<-SMD[,.(HHID,Region,NewArea_Name,NewArea_Name,Percentile,Decile)]
  # FirstSMD<-FirstSMD[,Realfirstpoor:=ifelse(Decile %in% 1:2,1,0)]
  # save(FirstSMD, file=paste0(Settings$HEISProcessedPath,"Y",year,"FirstSMD.rda"))
  
  
  
  B<-MD[,.(.N,Mean=mean(MetrPrice)),
        by=.(Region,NewArea_Name)]
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,Bundle_Value_H,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)]
    
    # Index2 <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
    #                                             weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)/TPBV2)/2)
    #                           ,by=.(Region,NewArea_Name)]
    
    X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                             weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
    X[,V:=V1+V2]
    
    SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
    SMDIterationPoor[,PriceIndex:=NULL]  
    
    Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                                               weighted.mean(Bundle_Value_H,Weight,na.rm = TRUE)/TPBV2*V2)/V)
                              ,by=.(Region,NewArea_Name)]
    Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea_Name)]
    
    
    SMD[,PriceIndex:=NULL]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea_Name"))
    
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
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,Bundle_Value_H,NewPoor,Decile,Percentile)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  
  
  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea_Name,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  MDD1<-MD[,.(HHID,Decile)]
  names(MDD1)<-c("HHID","Decile_FH")
  save(MDD1,file = "MDD1.rda")
  
  load(file = "MDD2.rda")
  MDD<-merge(MDD1,MDD2)
  MDD<-as.data.table(MDD)
  MDD<-MDD[,Decile_F:=as.numeric(Decile_F)]
  MDD<-MDD[,Decile_FH:=as.numeric(Decile_FH)]
  MDD[,diff:=Decile_F-Decile_FH]
  
  diff1<-MDD[diff==1]
  diff2<-MDD[diff==-1]
  diff<-MDD[diff==0]
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
        by=.(Region,NewArea,NewArea_Name,Decile)]
  
  D<-MD[,.(.N,Mean=weighted.median(Durable_Exp/Size,Weight)),
        by=.(Region,NewArea_Name,Decile)]
  
  #write.csv(A,file = "A.csv")
  #write.csv(B,file = "B.csv")
  #write.csv(C,file = "C.csv")
  #write.csv(D,file = "D.csv")
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
