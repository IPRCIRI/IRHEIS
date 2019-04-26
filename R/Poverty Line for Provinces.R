#Poverty Line for Provinces
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
  MD[,OldPoor:=1]
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>5  & i <=15){
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]    
    MDP <- MD[ThisIterationPoor==1,
              .(FPLine=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)),
              by=.(NewArea,Region)]
    MD <- merge(MD,MDP,by=c("Region","NewArea"))
    #    print(MDP)
    x<-MD[,.(NewArea,Region,FPLine,InitialPoor)]
    MD[,NewPoor:=ifelse(TFoodExpenditure_Per < FPLine,1,0)]
    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }
  
  MD[,FinalFoodPoor:=OldPoor]
  #MD <- MD[,.(HHID,Region,NewArea,cluster3,ProvinceCode,Size,
  #  Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,
  #  FoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor)]
  MD <- MD[,.(HHID,Region,NewArea,cluster3,ProvinceCode,Size,HAge,HSex,
              HLiterate,HEduLevel0,HActivityState,Area,Rooms,MetrPrice,
              Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,
              FoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor,
              TFoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month,
              Total_Exp_Month_Per,EqSizeRevOECD,EqSizeCalory,Decile)]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  MDFinalfood<-MD[,.(HHID,Region,NewArea,cluster3,Percentile,FinalFoodPoor)]
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  
  EngleD <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine,
               .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,NewArea)]
  EngleD[,PovertyLine:=FPLine/Engel]
  MD <- merge(MD,EngleD[,.(NewArea,Region,PovertyLine,Engel)],by=c("Region","NewArea"))
  #MD<-MD[Region=="Urban" & NewArea==2301]
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
 
   MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","NewArea")][order(Region,NewArea)]
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")