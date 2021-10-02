#162- Step 2.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n============== Calculationg Calorie Equal size  ===================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)


for(year in (88:99)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  cat("\n")
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
  
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]

  MD[,EqSizeCalory:=Calorie_Need_WorldBank/
       Settings$KCaloryNeed_Adult_WorldBank]
  MD[,EqSizeCalory2:=Calorie_Need_NutritionInstitute/
       Settings$KCaloryNeed_Adult_NutritionInstitute]
  MD[,EqSizeCalory3 :=(Size-NKids) +
       NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
