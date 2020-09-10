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
year<-95
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
    MD<-MD[,Engel:=TFoodExpenditure/Total_Exp_Month]
    
    #MDUrban<-MD[Region=="Urban" & cluster==3]    
#MDUrban<-MD[Region=="Urban" & cluster==2 & (NewArea==1 | NewArea==2 | NewArea==303 | NewArea==502 )]
#MDUrban<-MD[Region=="Urban" & cluster==2 & (NewArea==3001 | NewArea==23 | NewArea==30 | NewArea==25 | NewArea==26)]
#MDUrban<-MD[Region=="Urban" & cluster==2 & (NewArea==603 | NewArea==707 | NewArea==916 | NewArea==1002 | NewArea==2202)]

#MDUrban<-MD[Region=="Urban" & cluster==3 & (NewArea==603 | NewArea==707 | NewArea==916 | NewArea==1002 | NewArea==2202)]
    #MDUrban[,weighted.mean(Engel,Weight),by=NewArea][order(V1)]

    MDRural<-MD[Region=="Rural" & cluster==3]    
    #MDRural<-MD[Region=="Rural" & cluster==2 & (NewArea==1 | NewArea==2 | NewArea==303 | NewArea==502 )]
    #MDRural<-MD[Region=="Rural" & cluster==1 & (NewArea==11 | NewArea==9 | NewArea==17 | NewArea==12 | NewArea==24 | NewArea==16)]
    #MDRural<-MD[Region=="Rural" & cluster==1 & (NewArea==3 | NewArea==20 | NewArea==19 | NewArea==7 | NewArea==0)]
    
    #MDRural<-MD[Region=="Rural" & cluster==3 & (NewArea==10 | NewArea==2 | NewArea==27 | NewArea==21 | NewArea==1)]
    MDRural<-MD[Region=="Rural" & cluster==3 & (NewArea==5 | NewArea==26 | NewArea==18 | NewArea==25)]
    
    
    #MDRural<-MD[Region=="Rural" & cluster==3 & (NewArea==603 | NewArea==707 | NewArea==916 | NewArea==1002 | NewArea==2202)]
    MDRural[,weighted.mean(Engel,Weight),by=NewArea][order(V1)]
    

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

#sm.density.compare(MDUrban$Engel,group = MDUrban$NewArea)
#sm.density.compare(MDUrban$Engel,group = MDUrban$NewArea==9)
#sm.density.compare(MDRural$Engel,group = MDRural$NewArea)
#legend("topleft",legend = c("aaa","ss","dd","ff","gg"))

   


  endtime <- proc.time()
  cat("\n\n============================\nIt took ")
  cat((endtime-starttime)["elapsed"])
  cat(" seconds")