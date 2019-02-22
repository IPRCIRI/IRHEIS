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
year<-96
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
    MD<-MD[,Engel:=TFoodExpenditure/Total_Exp_Month]
    
    MDUrban<-MD[Region=="Urban" & Decile %in% 8:9]
    MDRural<-MD[Region=="Rural" & Decile %in% 8:9]
    

    #8th and 9th Deciles   
 #   EnglePR <- MDRural[Decile %in% 8:9,
 #                      .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
  #                       FPLine=mean(FPLine),cluster),by=.(Region,NewArea)]
    
   # EnglePU <- MDUrban[Decile %in% 8:9,
   #                    .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
    #                     FPLine=mean(FPLine),cluster),by=.(Region,NewArea)]
    
    #First and Second Deciles
    #EnglePR <- MDRural[Decile %in% 1:2,
     #                  .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
     #                    FPLine=mean(FPLine),cluster)]
    
   # EnglePU <- MDUrban[Decile %in% 1:2,
    #                   .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
     #                    FPLine=mean(FPLine),cluster)]
    
     
   #people's who are near FPLine
   # EnglePR <- MDRural[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine,
   #              .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
   #               FPLine=mean(FPLine),cluster),by=.(Region,NewArea)]
    
   # EnglePU <- MDUrban[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine,
   #                    .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
   #                     FPLine=mean(FPLine),cluster),by=.(Region,NewArea)]
#}
#geom_density(data = MD$Engel,show.legend = TRUE,
#inherit.aes = TRUE,inherit.aes = TRUE)

sm.density.compare(MDUrban$Engel,group = MDUrban$cluster)

sm.density.compare(MDRural$Engel,group = MDRural$cluster)
#legend("topleft",legend = c("aaa","ss","dd","ff","gg"))
       
       
  endtime <- proc.time()
  cat("\n\n============================\nIt took ")
  cat((endtime-starttime)["elapsed"])
  cat(" seconds")