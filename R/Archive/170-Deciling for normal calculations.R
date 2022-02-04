#164-Step 4-FindInitialPoor.R
# 
# Copyright © 2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Deciling =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
#library(ggplot2)
#library(compare)
source("000-FunctionDefs.R")


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
               House_Exp,FoodExpenditure,Total_Expenditure_Month,
               NewArea,NewArea_Name,Total_Consumption_Month_per,TOriginalFoodExpenditure_Per,
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
  
  mdset <- setdiff(names(MD),names(SMD))
  Decile <- merge(MD[,c("HHID",mdset),with=FALSE],SMD,by="HHID")
  Decile <-Decile[,.(HHID,Decile)]
  save(Decile,file=paste0(Settings$HEISProcessedPath,"Y",year,"Decile.rda"))
  
  #Decile_MD <- MD [,.(HHID,Total_Consumption_Month_per,Size,Weight)]
  #Decile_MD <- Decile_MD[order(Total_Consumption_Month_per)]
  #Decile_MD <- Decile_MD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]
  #Decile_MD <- Decile_MD[,Decile_non_real:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  #MD <- merge(MD,Decile_MD[,c("HHID","Decile_non_real")],by="HHID")
  #save(Decile_MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"Decile_non_real.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
