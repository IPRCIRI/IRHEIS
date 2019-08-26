# 57-PovertyLines.R
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

  #En3es acc6rd5ng to N
 # EngleS <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine & TOriginalFoodExpenditure_Per<1.2*FPLine,
   #            .(.N), by=.(Region,cluster3)] 
#  EngleS<-EngleS[,S:=ifelse(N>400,N,0)]
#  MD<-merge(MD, EngleS)
 # EngleD <- MD[S==0 & TOriginalFoodExpenditure_Per>0.7*FPLine & TOriginalFoodExpenditure_Per<1.3*FPLine,
  #              .(.N,Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
 #                 FPLine=mean(FPLine)),by=.(Region,cluster3)]
 # EngleS <- MD[S==0 & TOriginalFoodExpenditure_Per>0.7*FPLine & TOriginalFoodExpenditure_Per<1.3*FPLine,
  #              .(.N), by=.(Region,cluster3)] 
 # load(file="EngleX.rda")


  
  #EngleD<-merge(EngleD,EngleX,by=c("Region","cluster3"))
  EngleD[,PovertyLine:=FPLine/Engel]
  EngleD[,PovertyLine2:=4525809]
  MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,PovertyLine2,Engel)],by=c("Region","cluster3"))
  

# MD<-MD[Region=="Urban"]

  
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  MD[,FinalPoor2:=ifelse(Total_Exp_Month_Per < PovertyLine2,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
     # MD[,weighted.mean(PovertyLine2,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  
  MD[FinalPoor==1,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=c("ProvinceCode")][order(ProvinceCode)]
  
  MD[,weighted.mean(FinalPoor2,Weight*Size),by=c("ProvinceCode")][order(ProvinceCode)]
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")][order(Region,cluster3)]
  MD[,weighted.mean(FinalPoor2,Weight*Size),by=c("Region")]
  MD[,weighted.mean(FinalPoor2,Weight*Size)]
  MD3<-MD[,.(HHID,FinalPoor,Weight)]
  save(MD3,file=paste0(Settings$HEISProcessedPath,"Y",year,"PoorsforMerge.rda"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")