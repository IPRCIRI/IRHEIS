#54-FoodBasicNeeds.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))

  #Determine Food (Equal 2100 KCal) Bundle
  #MDPoors<-MD[InitialPoor==1]
  MD[,NewPoor:=InitialPoor]
  #MD[,NewPoor:=ifelse(Decile %in% c(1,2,3,4),1,0)]
  MD[,OldPoor:=1]

  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>5  & i <=15){
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]    
    MDP <- MD[ThisIterationPoor==1,
              .(FPLine=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)),
              by=.(cluster3,Region)]
    MD <- merge(MD,MDP,by=c("Region","cluster3"))
#    print(MDP)
    x<-MD[,.(NewArea,Region,FPLine,InitialPoor)]
    MD[,NewPoor:=ifelse(TFoodExpenditure_Per < FPLine,1,0)]
    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }

  MD[,FinalFoodPoor:=OldPoor]

  MD <- MD[,.(HHID,Region,NewArea,NewArea_Name,cluster3,ProvinceCode,Size,HAge,HSex,
              HLiterate,HEduLevel0,HActivityState,Area,Rooms,MetrPrice,
              Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,
              FoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor,
              Total_Exp_Month_Per,TFoodTKCalories_Per,TFoodExpenditure,Total_Exp_Month,
            #  TFoodExpenditure2,Total_Exp_Month_nondurable2,Total_Exp_Month2,
             # Total_Exp_Month_Per2,
              EqSizeOECD,EqSizeCalory,Decile,Bundle_Value)]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  MDFinalfood<-MD[,.(HHID,Region,NewArea,cluster3,Percentile,FinalFoodPoor)]
  UrbanFinalfood<-MDFinalfood[Region=="Urban"]
  RuralFinalfood<-MDFinalfood[Region=="Rural"]
  save(UrbanFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanFinalfood.rda"))
  save(RuralFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralFinalfood.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")