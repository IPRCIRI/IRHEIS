# 177- Step 7,8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
 
  EngleD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
               .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,cluster3)]
  MD <- MD[,PEngel:=TOriginalFoodExpenditure/Total_Exp_Month]
  MD2 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine]
  MD2 <- MD2[,PEngel:=TOriginalFoodExpenditure/Total_Exp_Month]
  MD2U <- MD2[Region=="Urban"]
  MD2R <- MD2[Region=="Rural"]


  #EngleD<-merge(EngleD,EngleX,by=c("Region","cluster3"))
  EngleD[,PovertyLine:=FPLine/Engel]
  EngleD[,PovertyLine2:=5580259]
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,PovertyLine2,Engel)],by=c("Region","cluster3"))
  

 #MD<-MD[Region=="Urban"]

  
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD[,FinalPoor2:=ifelse(Total_Exp_Month_Per < PovertyLine2,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
     # MD[,weighted.mean(PovertyLine2,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight)]
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region)]
    MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=.(Region,cluster3)]
  
  #MD[,crw:=sum(Size*Weight),by=Region]
  #MD[,sum((Size*Weight)/crw),by=.(Region,Decile)][order(Region,Decile)]
  #MD[,weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(Region,Decile)][order(Region,Decile)]
  
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=c("ProvinceCode")][order(ProvinceCode)]

  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("ProvinceCode")][order(ProvinceCode)]
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")][order(Region,cluster3)]
  MD[,weighted.mean(FinalPoor2,Weight*Size),by=c("Region")]
  MD[,weighted.mean(FinalPoor2,Weight*Size)]
  MD3<-MD[,.(HHID,FinalPoor,Weight)]
  save(MD3,file=paste0(Settings$HEISProcessedPath,"Y",year,"PoorsforMerge.rda"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

 # write.csv(EngleD,file ="Results.csv" )
  
  MDU2<-MD[Region=="Urban" ,.(HHID,cluster3)]
  save(MDU2,file=paste0(Settings$HEISProcessedPath,"Y",year,"MDU2.rda"))
  MDR3<-MD[Region=="Rural" ,.(HHID,cluster3)]
  save(MDR3,file=paste0(Settings$HEISProcessedPath,"Y",year,"MDR3.rda"))
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")