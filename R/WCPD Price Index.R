# WCPD Price Index
# 
# Zahra Shahidi
# 2020

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
      ,by=.(Region,NewArea_Name)]
  
  X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
              weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]
  VT<-X[NewArea_Name=="Sh_Tehran"]
  V1T<-VT$V1
  V2T<-VT$V2
  SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))
  
  SMD[,PriceIndex:=((exp((V1+V1T)/2))*(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1))*
        ((exp((V2+V2T)/2))*(weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)) ,by=.(Region,NewArea_Name)]
  
  
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
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
    # Index2 <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
    #                                             weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2)/2)
    #                           ,by=.(Region,NewArea_Name)]
    
    X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
                             weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
    X[,V:=V1+V2]
    VT<-X[NewArea_Name=="Sh_Tehran"]
    V1T<-VT$V1
    V2T<-VT$V2
    SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
    SMDIterationPoor[,PriceIndex:=NULL]  
    
    Index <- SMDIterationPoor[,.(PriceIndex=(exp((V1+V1T)/2)*(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1))*
                                   (exp((V2+V2T)/2)*(weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2))) ,by=.(Region,NewArea_Name)]
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
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  
  
  MD[,weighted.mean(InitialPoor,Weight,na.rm = TRUE), by=.(NewArea_Name,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")