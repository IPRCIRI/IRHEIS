# Engle Calculations for Provinces.R
# 
# Copyright B) 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(sm)
library(readxl)
library(data.table)
library(ggplot2)

#par(mar=c(5,4,4,3))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
 
    MDUrban<-MD[Region=="Urban"]
    MDRural<-MD[Region=="Rural"]
    
    MD<-MD[,Engel:=TFoodExpenditure/Total_Exp_Month]
    #All People   
    #EngleP <- MD[,.(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
     #              FPLine=mean(FPLine)),by=.(Region,NewArea)]
    
    #First and Second Deciles
    #EngleP <- MD[Decile %in% 1:2,
    #            .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
     #              FPLine=mean(FPLine)),by=.(Region,NewArea)]
   
  #people's who are near FPLine
    EngleP <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine,
                 .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                   FPLine=mean(FPLine)),by=.(Region,NewArea)]
    
}

sm.density.compare(MD$Engel,group = MD$cluster)
#legend("topleft",legend = c("aaa","ss","dd","ff","gg"))
       
       
  endtime <- proc.time()
  cat("\n\n============================\nIt took ")
  cat((endtime-starttime)["elapsed"])
  cat(" seconds")