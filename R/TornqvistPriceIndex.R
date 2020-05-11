# Tornqvist Price Index
# 
# Copyright 2020 Zahra Shahidi & Majid Einian


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
#library(ggplot2)

# Function Defs ---------------------------------------------------------------------------------
CalcTornqvistIndex <- function(DataTable){
    X <- DataTable[,.(wi1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
              wi2=weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE),
              pi1=weighted.mean(Bundle_Value,Weight,na.rm = TRUE),
              pi2=weighted.mean(MetrPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea2)]
  
  X[,wi:=wi1+wi2]
  X[,wi1:=wi1/wi]
  X[,wi2:=wi2/wi]
  XTeh<-X[NewArea2=="Sh_Tehran"]
  wk1<-XTeh$wi1   # k == Sh_Tehran
  wk2<-XTeh$wi2
  pk1<-XTeh$pi1
  pk2<-XTeh$pi2
  
  X[,SimpleIndex:= .5 * pi1/pk1 + .5 * pi2/pk2]
  X[,AnotherIndex:= wi1 * pi1/pk1 + wi2 * pi2/pk2]
  
  X[,TornqvistIndex:= exp(   (wk1+wi1)/2 * log(pi1/pk1) + (wk2+wi2)/2 * log(pi2/pk2)  )      ]
  
  return(X[,.(Region,NewArea2,PriceIndex=TornqvistIndex)])
}


DoDeciling_SetInitialPoor <- function(DataTable,PriceIndexDT){

  if("PriceIndex" %in% names(DataTable)){
      DataTable <- DataTable[,PriceIndex:=NULL]
  }
  DataTable <- merge(DataTable,PriceIndexDT,by=c("Region","NewArea2"))
  
  
  DataTable <- DataTable[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  DataTable <- DataTable[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  DataTable <- DataTable[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  DataTable <- DataTable[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  DataTable <- DataTable[,InitialPoorBasedOnPercentile:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  
  return(DataTable)
}

 # Main loop for years ----------------------------------------------------------------
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
  
  PriceDT <- CalcTornqvistIndex(SMD)
  SMD <- DoDeciling_SetInitialPoor(SMD,PriceDT)
  

  
  SMD[,InitialPoorBasedOnPercentileLastIteration:=1]
  i <- 0
  while(SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)]>0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,InitialPoorBasedOnPercentileLastIteration:=InitialPoorBasedOnPercentile]
    
    SMDIterationPoor<-SMD[InitialPoorBasedOnPercentileLastIteration==1]
    
    if(nrow(SMDIterationPoor[,.N,by=.(Region,NewArea2)])<78)
      stop("HERE Some Area goes missing!")
    if(min(SMDIterationPoor[,.N,by=.(Region,NewArea2)]$N)==0)
      stop("HERE Some Area goes missing!")
    
    PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)
    SMD <- DoDeciling_SetInitialPoor(SMD,PriceDTBasedOnThisIterationPoor)
    
    cat("\n",i,":",SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)])
  }
  
  save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
  
  
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile)],by="HHID")
  setnames(MD,"InitialPoorBasedOnPercentile","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !
  
  
  MD[,weighted.mean(InitialPoor,Weight,na.rm = TRUE), by=.(NewArea2,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
}


endtime <- proc.time()
cat("\n\n============================\nIt took ",(endtime-starttime)["elapsed"]," seconds")
