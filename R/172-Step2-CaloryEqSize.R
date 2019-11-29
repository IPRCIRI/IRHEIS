#172- Step 2.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Calculationg Equal size calorie =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  
  #MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]
  #MD[,Home_Per_Metr:=MetrPrice/EqSizeOECD]
  
  #Calculate Per Values
  MD[,EqSizeCalory3 :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  
  MD[,EqSizeCalory :=
       NAge1B	*(Settings$	KCaloryNeed_B1	/Calorie_Need_WorldBank)+
       NAge2B	*(Settings$	KCaloryNeed_B2	/Calorie_Need_WorldBank)+
       NAge3B	*(Settings$	KCaloryNeed_B3	/Calorie_Need_WorldBank)+
       NAge4B	*(Settings$	KCaloryNeed_B4	/Calorie_Need_WorldBank)+
       NAge5B	*(Settings$	KCaloryNeed_B5	/Calorie_Need_WorldBank)+
       NAge6B	*(Settings$	KCaloryNeed_B6	/Calorie_Need_WorldBank)+
       NAge7B	*(Settings$	KCaloryNeed_B7	/Calorie_Need_WorldBank)+
       NAge8B	*(Settings$	KCaloryNeed_B8	/Calorie_Need_WorldBank)+
       NAge9B	*(Settings$	KCaloryNeed_B9	/Calorie_Need_WorldBank)+
       NAge10B	*(Settings$	KCaloryNeed_B10	/Calorie_Need_WorldBank)+
       NAge1G	*(Settings$	KCaloryNeed_G1	/Calorie_Need_WorldBank)+
       NAge2G	*(Settings$	KCaloryNeed_G2	/Calorie_Need_WorldBank)+
       NAge3G	*(Settings$	KCaloryNeed_G3	/Calorie_Need_WorldBank)+
       NAge4G	*(Settings$	KCaloryNeed_G4	/Calorie_Need_WorldBank)+
       NAge5G	*(Settings$	KCaloryNeed_G5	/Calorie_Need_WorldBank)+
       NAge6G	*(Settings$	KCaloryNeed_G6	/Calorie_Need_WorldBank)+
       NAge7G	*(Settings$	KCaloryNeed_G7	/Calorie_Need_WorldBank)+
       NAge8G	*(Settings$	KCaloryNeed_G8	/Calorie_Need_WorldBank)+
       NAge9G	*(Settings$	KCaloryNeed_G9	/Calorie_Need_WorldBank)+
       NAge10G	*(Settings$	KCaloryNeed_G10	/Calorie_Need_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_WorldBank)]
  
  MD[,EqSizeCalory2 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Calorie_Need_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Calorie_Need_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Calorie_Need_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Calorie_Need_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Calorie_Need_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Calorie_Need_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Calorie_Need_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Calorie_Need_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Calorie_Need_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Calorie_Need_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Calorie_Need_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Calorie_Need_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Calorie_Need_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Calorie_Need_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Calorie_Need_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Calorie_Need_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Calorie_Need_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Calorie_Need_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_Anstitoo)]
  
  MD[,EqSizeCalory4 :=
       NAge1B*(Settings$KCaloryNeed_B1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2B*(Settings$KCaloryNeed_B2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3B*(Settings$KCaloryNeed_B3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4B*(Settings$KCaloryNeed_B4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5B*(Settings$KCaloryNeed_B5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6B*(Settings$KCaloryNeed_B6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7B*(Settings$KCaloryNeed_B7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8B*(Settings$KCaloryNeed_B8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9B*(Settings$KCaloryNeed_B9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10B*(Settings$KCaloryNeed_B10/Settings$KCaloryNeed_Adult_WorldBank)+
       NAge1G*(Settings$KCaloryNeed_G1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2G*(Settings$KCaloryNeed_G2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3G*(Settings$KCaloryNeed_G3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4G*(Settings$KCaloryNeed_G4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5G*(Settings$KCaloryNeed_G5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6G*(Settings$KCaloryNeed_G6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7G*(Settings$KCaloryNeed_G7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8G*(Settings$KCaloryNeed_G8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9G*(Settings$KCaloryNeed_G9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10G*(Settings$KCaloryNeed_G10/Settings$KCaloryNeed_Adult_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_WorldBank)]
  
  MD[,EqSizeCalory5 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Settings$KCaloryNeed_Adult_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_Anstitoo)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))

  MD[,weighted.mean(EqSizeCalory,Weight)]
  MD[,weighted.mean(EqSizeCalory2,Weight)]
  MD[,weighted.mean(EqSizeCalory4,Weight)]
  MD[,weighted.mean(EqSizeCalory5,Weight)]
  MD[,weighted.mean(EqSizeCalory3,Weight)]
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)