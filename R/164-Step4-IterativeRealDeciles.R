#164-Step 4-FindInitialPoor.R
# 
# Copyright © 2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
#library(ggplot2)
#library(compare)
source("142-Calculate_OwnedDurableItemsDepreciation_FunctionDef.R")
# Function Defs ---------------------------------------------------------------------------------
CalcTornqvistIndex <- function(DataTable){
  
  X <- DataTable[,.(N=.N,wi1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
                    wi2=weighted.mean(House_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
                    pi1=weighted.mean(Bundle_Value,Weight,na.rm = TRUE),
                    pi2=weighted.mean(MeterPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
  
  X[,wi:=wi1+wi2]
  X[,wi1:=wi1/wi]
  X[,wi2:=wi2/wi]
  XTeh<-X[NewArea_Name=="Sh_Tehran"]
  wk1<-XTeh$wi1   # k == Sh_Tehran
  wk2<-XTeh$wi2
  pk1<-XTeh$pi1
  pk2<-XTeh$pi2
  
  X[,SimpleIndex:= .5 * pi1/pk1 + .5 * pi2/pk2]
  X[,AnotherIndex:= wi1 * pi1/pk1 + wi2 * pi2/pk2]
  
  X[,TornqvistIndex:= exp(   (wk1+wi1)/2 * log(pi1/pk1) + (wk2+wi2)/2 * log(pi2/pk2)  )      ]
  
  return(X[,.(Region,NewArea_Name,PriceIndex=TornqvistIndex)])
}


DoDeciling <- function(DataTable,PriceIndexDT){
  
  if("PriceIndex" %in% names(DataTable)){
    DataTable <- DataTable[,PriceIndex:=NULL]
  }
  DataTable <- merge(DataTable,PriceIndexDT,by=c("Region","NewArea_Name"))
  
  
  DataTable <- DataTable[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
   
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  DataTable <- DataTable[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  DataTable <- DataTable[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
  DataTable <- DataTable[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
  #Calculate deciles by weights
  DataTable <- DataTable[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  DataTable <- DataTable[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  DataTable[,crw:=NULL]
  DataTable[,xr25th:=NULL]
  
  return(DataTable)
}

UpdateForDurableDepr <- function(DataTable,ODIDep){
  DataTable[,OwnedDurableItemsDepreciation:=NULL]
  DataTable <- merge(DataTable,ODIDep)
  
  for (col in Settings$w)
    DataTable[is.na(get(col)), (col) := 0]
  
  DataTable[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=Settings$w]
  DataTable[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=Settings$nw]
  
  DataTable[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  DataTable[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
}

DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
                                      sheet=Settings$MDS_DurableItemsDepr))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_Detail.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "OwnsDurableItems.rda"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,
                 House_Exp,FoodExpenditure,Total_Exp_Month,
                 NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
                 TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_NutritionInstitute,
                 Weight,MeterPrice,Size,EqSizeOECD
                 ,OriginalFoodExpenditure,FoodOtherExpenditure, Cigar_Exp, Cloth_Exp,
                 Amusement_Exp, Communication_Exp, 
                 Energy_Exp, Furniture_Exp, Hotel_Exp,Restaurant_Exp, Hygiene_Exp, 
                 Transportation_Exp, Other_Exp
                 ,Add_to_NonDurable,Medical_Exp,
                 Durable_NoDep,Durable_Emergency,OwnedDurableItemsDepreciation)]


  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_NutritionInstitute/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_NutritionInstitute/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
 # S1<-SMD[,.(HHID,Region,NewArea_Name,TFoodKCaloriesHH_Per,Bundle_Value)]
  
  
  PriceDT <- CalcTornqvistIndex(SMD)

  SMD <- DoDeciling(SMD,PriceDT)
  
  OwnedDurableItemsDepreciation <- 
    Calculate_OwnedDurableItemsDepreciation(
      DurableData_ExpDetail = DurableData_Detail,
      DurableItems_OwningDetail = OwnsDurableItems,
      by = c("Item","Decile"),
      Decile = SMD[,.(HHID,Decile)],
      DurableItems = DurableItems)
  
  SMD <- UpdateForDurableDepr(SMD,OwnedDurableItemsDepreciation)

  SMD <- SMD[,IPboPct:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  # IPboPct : InitialPoorBasedOnRealIterativePercentile
  # IPboPctLI: InitialPoorBasedOnRealIterativePercentileLastIteration

  SMD[,IPboPctLI:=1]
  i <- 0
  while(SMD[,sum((IPboPct-IPboPctLI)^2)]>0.001*nrow(SMD) & i <15){
    i <- i+1
    SMD[,IPboPctLI:=IPboPct]
    
    SMDIterationPoor <- SMD[IPboPctLI==1 | First25==1 ]
    
    if(nrow(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)])<78)
      stop("HERE Some Area goes missing!")
    if(min(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)]$N)==0)
      stop("HERE Some Area goes missing!")
    
    PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)

    #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
    SMD <- DoDeciling(SMD,PriceDTBasedOnThisIterationPoor)
    
    OwnedDurableItemsDepreciation <- 
      Calculate_OwnedDurableItemsDepreciation(
        DurableData_ExpDetail = DurableData_Detail,
        DurableItems_OwningDetail = OwnsDurableItems,
        by = c("Item","Decile"),
        Decile = SMD[,.(HHID,Decile)],
        DurableItems = DurableItems)
    
    SMD <- UpdateForDurableDepr(SMD,OwnedDurableItemsDepreciation)
    

    SMD <- SMD[,IPboPct:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    
    cat("\n",i,":",SMD[,sum((IPboPct-IPboPctLI)^2)])
  }
  
  setnames(SMD,"IPboPct","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !
  
  SMD <- SMD[,setdiff(names(SMD),c("First25","IPboPctLI")),with=FALSE]
  
  mdset <- setdiff(names(MD),names(SMD))
  MD <- merge(MD[,c("HHID",mdset),with=FALSE],SMD,by="HHID")
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
}
 
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
