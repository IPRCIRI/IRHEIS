# 169-Step9-PovertyStats.R
# 
# Copyright © 2018-2022:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)

#TODO: move this to the right position
ProvinceFarsiNames<-as.data.table(read_excel("../Data/ProvinceFarsiNames.xlsx",  
                                             sheet = "Sheet2"))

Groupings <- list(NULL,"Region","cluster3","Province","Dcil_Gen_Cons_Nominal")
names(Groupings) <- c("Country","Region","Cluster","Province","NominalDecile")
PovStatsList <- list()
for(GroupingVarName in names(Groupings)){
  PovStatsList[[GroupingVarName]] <- data.table()
}

for(year in ((Settings$startyear+2):Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  
  MD[,FGT1M:=(CMPovLine-Total_Consumption_Month_per)/CMPovLine]
  MD[,FGT2M:=((CMPovLine-Total_Consumption_Month_per)/CMPovLine)^2]
  
  MD <- merge(MD,ProvinceFarsiNames[,.(ProvinceName,Province)],by=c("ProvinceName"))
  
  for(GroupingVarName in names(Groupings)){
    GroupingVar <- Groupings[[GroupingVarName]]
    
    X1 <- MD[,.(SampleSize=.N,
                MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
                House_Share=weighted.mean(House_Exp/Total_Expenditure_Month,Weight),
                FPLine=weighted.mean(FPLine,Weight*Size),
                Bundle_Value=weighted.mean(Bundle_Value,Weight),
                FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight*Size),
                Engel=weighted.mean(TOriginalFoodExpenditure/Total_Expenditure_Month,Weight),
                Total_Expenditure_Month_per=weighted.mean(Total_Expenditure_Month_per,Weight*Size),
                Total_Expenditure_Month=weighted.mean(Total_Expenditure_Month,Weight),
                Total_Consumption_Month_per=weighted.mean(Total_Consumption_Month_per,Weight*Size),
                Total_Consumption_Month=weighted.mean(Total_Consumption_Month,Weight*Size),
                CMPovLine=weighted.mean(CMPovLine,Weight*Size),
                PovertyLine=weighted.mean(PovertyLine,Weight*Size),
                PovertyHCR=weighted.mean(FinalPoor,Weight*Size)),
             by=GroupingVar]
    
    X2 <- MD[FinalPoor==1,
             .(PoorSampleSize=.N,
               PovertyGap=weighted.mean(FGT1M,Weight*Size),
               PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
             by=GroupingVar]
    
    X1[,Year:=year]
    X2[,Year:=year]
    X <- merge(X1,X2,by=c("Year",GroupingVar),all.x = TRUE)
    
    PovStatsList[[GroupingVarName]] <- rbind(PovStatsList[[GroupingVarName]],X)
  }
}
cat("\n\n")
print(PovStatsList[["Country"]][,.(Year,PovertyLine,PovertyHCR,PovertyGap,PovertyDepth)])

write_xlsx(PovStatsList,path = paste0(Settings$HEISResultsPath,"/AllResults.xlsx"))

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")