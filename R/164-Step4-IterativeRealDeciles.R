#164-Step4-IterativeRealDeciles.R
# 
# Copyright © 2020-2022 :Majid Einian & Arin Shahbazian
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
source("000-FunctionDefs.R")

DurableItemsDepr <- data.table(read_excel(Settings$MetaDataFilePath,
                                          sheet=Settings$MDS_DurableItemsDepr))
DurableGroups <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_DurableGroups))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,";"))
  
  # load data --------------------------------------
  load(file = paste0(Settings$HEISProcessedPath,"Y",
                     year,"DurableData_NetDetail.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,
                   "OwnsDurableItems.rda"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  SMD <- MD[,c("HHID", "Region", "NewArea", 
               "NewArea_Name",
               union(Settings$ExpenditureCols,Settings$ConsumptionCols),
               "FoodExpenditure", 
               "TOriginalFoodExpenditure_Per",
               "Total_Expenditure_Month",
               "Total_Expenditure_Month_per",
               "Total_Consumption_Month", 
               "Total_Consumption_Month_per", 
               "TFoodKCaloriesHH_Per", 
               "Calorie_Need_WorldBank",
               "Calorie_Need_NutritionInstitute", 
               "Weight", "MeterPrice", "Size", "EqSizeOECD"),with=FALSE]
  SMD <- merge(SMD,HHHouseProperties[,.(HHID,tenure)],by="HHID")


  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_NutritionInstitute/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_NutritionInstitute/TFoodKCaloriesHH_Per]
  
  SMD <- SMD[TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
  PriceDTBasedOnTotalSample <- CalcTornqvistIndex(SMD)

  GDC <- DoDeciling(HHDT = SMD,
                    PriceIndexDT = PriceDTBasedOnTotalSample,
                    OrderingVar = "Consumption",
                    Size = "_per")[,.(HHID,
                                      First25=First25,
                                      Dcil_Gen_Cons_PAdj=Decile,         #Decile_General_Consumption_PriceAdj
                                      Pctl_Gen_Cons_PAdj=Percentile)]
  GDX <- DoDeciling(HHDT = SMD,
                    PriceIndexDT = PriceDTBasedOnTotalSample,
                    OrderingVar = "Expenditure",
                    Size = "_per")[,.(HHID,
                                      Dcil_Gen_Exp_PAdj=Decile,
                                      Pctl_Gen_Exp_PAdj=Percentile)]
  SMD <- merge(SMD,GDC)
  SMD <- merge(SMD,GDX)
 
  #table(SMD[,.(Dcil_Gen_Cons_PAdj,Dcil_Gen_Exp_PAdj)])
  g2 <- DurableGroups[year >= StartYear & year <= EndYear & Group==2]$Code
  
  OwnedDurableItemsDepreciation <- 
    Calculate_OwnedDurableItemsDepreciation(
      DurableData_ExpDetail = DD,
      DurableItems_OwningDetail = OwnsDurableItems,
      by = c("Item","Decile"),
      Decile = SMD[,.(HHID,Decile=Dcil_Gen_Cons_PAdj)],
      DurableItems = DurableItemsDepr,
      g2 = g2)
  
  SMD <- UpdateForDurableDepr(SMD,OwnedDurableItemsDepreciation)

  SMD <- SMD[,IPboPct:=ifelse(Pctl_Gen_Cons_PAdj %in% 1:Settings$InitialPoorPercentileMax,1,0)]
  # IPboPct : InitialPoorBasedOnRealIterativePercentile
  # IPboPctLI: InitialPoorBasedOnRealIterativePercentileLastIteration
  
  SMD[,IPboPctLI:=1]
  i <- 0
  while(SMD[,sum((IPboPct-IPboPctLI)^2)]>0.002*nrow(SMD) & i <20){
    i <- i+1
    SMD[,IPboPctLI:=IPboPct]
    
    SMDIterationPoor <- SMD[IPboPctLI==1 | First25==1 ]
    
    if(nrow(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)])<78)
      stop("HERE Some Area goes missing!")
    if(min(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)]$N)==0)
      stop("HERE Some Area goes missing!")
    
    PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)

    #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
    IPDC <- DoDeciling(HHDT = SMD,
                      PriceIndexDT = PriceDTBasedOnThisIterationPoor,
                      OrderingVar = "Consumption",
                      Size = "_per")[,.(HHID,
                                        First25=First25,
                                        Dcil_TIP_Cons_PAdj=Decile,         #Decile_ThisIterationPoor_Consumption_PriceAdj
                                        Pctl_TIP_Cons_PAdj=Percentile)]
    SMD[,First25:=NULL]
    SMD[,Dcil_TIP_Cons_PAdj:=NULL]
    SMD[,Pctl_TIP_Cons_PAdj:=NULL]
    SMD <- merge(SMD,IPDC)
    
    
    OwnedDurableItemsDepreciation <- 
      Calculate_OwnedDurableItemsDepreciation(
        DurableData_ExpDetail = DD,
        DurableItems_OwningDetail = OwnsDurableItems,
        by = c("Item","Decile"),
        Decile = SMD[,.(HHID,Decile=Dcil_TIP_Cons_PAdj)],
        DurableItems = DurableItemsDepr,
        g2 = g2)

    SMD <- UpdateForDurableDepr(SMD,OwnedDurableItemsDepreciation)
    

    SMD <- SMD[,IPboPct:=ifelse(Pctl_TIP_Cons_PAdj %in% 1:Settings$InitialPoorPercentileMax,1,0)]
    
    cat(",\t",i,":",SMD[,sum((IPboPct-IPboPctLI)^2)])
  }
  
  setnames(SMD,"IPboPct","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !
  
  setnames(SMD,"Dcil_TIP_Cons_PAdj","Dcil_IP_Cons_PAdj") # This Iteration Poor => Initial Poor
  setnames(SMD,"Pctl_TIP_Cons_PAdj","Pctl_IP_Cons_PAdj") 
  
  
  IPDX <- DoDeciling(HHDT = SMD,
                     PriceIndexDT = PriceDTBasedOnThisIterationPoor,
                     OrderingVar = "Expenditure",
                     Size = "_per")[,.(HHID,
                                       Dcil_IP_Exp_PAdj=Decile,        
                                       Pctl_IP_Exp_PAdj=Percentile)]
  SMD <- merge(SMD,IPDX)
  
  SMD <- SMD[,setdiff(names(SMD),c("First25","IPboPctLI")),with=FALSE]
  
  mdset <- setdiff(names(MD),names(SMD))
  MD <- merge(MD[,c("HHID",mdset),with=FALSE],SMD,by="HHID")
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  Deciles <- MD[,.(HHID,Weight,Size,
                   Total_Consumption_Month_per,Total_Expenditure_Month_per,
                   Dcil_Gen_Cons_PAdj, Pctl_Gen_Cons_PAdj, 
                   Dcil_Gen_Exp_PAdj, Pctl_Gen_Exp_PAdj,
                   Dcil_IP_Cons_PAdj, Pctl_IP_Cons_PAdj,  
                   Dcil_IP_Exp_PAdj, Pctl_IP_Exp_PAdj)]
  
  Deciles <- Deciles[order(Total_Consumption_Month_per)]  
  Deciles <- Deciles[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  Deciles <- Deciles[,Dcil_Gen_Cons_Nominal:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  Deciles <- Deciles[,Pctl_Gen_Cons_Nominal:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  Deciles <- Deciles[order(Total_Expenditure_Month_per)]  
  Deciles <- Deciles[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  Deciles <- Deciles[,Dcil_Gen_Exp_Nominal:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  Deciles <- Deciles[,Pctl_Gen_Exp_Nominal:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  Deciles[,crw:=NULL]
  
  save(Deciles,file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
}
 
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
