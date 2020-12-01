# 168-Step8-PovertyStats.R
# 
# Copyright © 2018-2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  
  Total<-Total[,.(HHID,Job_Main_Code_Pub,Job_Main_Code_Cooperative,Job_Main_Code_Prv,
                  Job_Main_Code_Buss,Job_Main_Code_Agri)]

  

  Total[Total==0]<-NA
  
  MD<-merge(MD,HHHouseProperties,by="HHID")
  MD<-merge(MD,Total,by="HHID")
  MD[,Job_Code:=pmin(Job_Main_Code_Pub,Job_Main_Code_Cooperative,
                    Job_Main_Code_Prv,
                    Job_Main_Code_Buss,Job_Main_Code_Agri,na.rm = TRUE)]
  
  A<-MD[,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           HSex=weighted.mean(HSex=="Female",Weight),
           HActivityState=weighted.mean(HActivityState=="Employed",Weight),
           Job_Code=weighted.mean(Job_Code==9,Weight,na.rm = TRUE),
           TFoodKCaloriesHH_Per=weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE),
           FoodProtein_Per=weighted.mean(FoodProtein_Per,Weight,na.rm = TRUE)),by=FinalPoor]
  
  B<-MD[,weighted.mean(FinalPoor,Weight),by=HSex]
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")