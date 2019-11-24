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
  
  SMD <- MD[,.(HHID,Region,NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeRevOECD)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  #SMD[,Bundle_Value2:=TOriginalFoodExpenditure_Per/Relative_Calorie1]
  #SMD[,Bundle_Value1:=TOriginalFoodExpenditure_Per/Relative_Calorie2]
  #SMD <- MD[,.(HHID,Region,NewArea,Total_Exp_Month_Per_nondurable,
  #FoodExpenditure_Per,FoodKCalories_Per, Weight,MetrPrice)]
  
  #SMD[,Bundle_Value:=FoodExpenditure_Per*Settings$KCaloryNeed_Adult/FoodKCalories_Per]
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300]
  
  #Real Prices
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea)]
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  FirstSMD<-SMD[,.(HHID,Region,NewArea,NewArea2,Percentile,Decile)]
  FirstSMD<-FirstSMD[,Realfirstpoor:=ifelse(Decile %in% 1:2,1,0)]
  save(FirstSMD, file=paste0(Settings$HEISProcessedPath,"Y",year,"FirstSMD.rda"))
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=30 & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
    Index <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
                                                weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2)/2)
                              ,by=.(Region,NewArea)]
    
    SMD[,PriceIndex:=NULL]
    SMD<- SMD[order(Region,NewArea)]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea"))
    
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
  

  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

  

}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")