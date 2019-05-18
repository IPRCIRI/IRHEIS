#56-Poverty Line for Provinces
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  #Determine Food (Equal 2100 KCal) Bundle
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

    x<-MD[,.(NewArea,Region,FPLine,InitialPoor)]
    MD[,NewPoor:=ifelse(TFoodExpenditure_Per < FPLine,1,0)]
    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }
  
  MD[,FinalFoodPoor:=OldPoor]


  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  MDFinalfood<-MD[,.(HHID,Region,NewArea,NewArea2,Percentile,FinalFoodPoor)]
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  

  EngleD <- MD[TFoodExpenditure_Per<1.2*FPLine & TFoodExpenditure_Per>0.8*FPLine,
               .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,NewArea2)]
  EngleD[,PovertyLine:=FPLine/Engel]
  
  MD[,EngelPersonal:=TFoodExpenditure/Total_Exp_Month]
  TD<-MD[,PersonalPLine:=FPLine/EngelPersonal]
  save(TD,file = paste0(Settings$HEISProcessedPath,"Y",year,"MD4test.rda"))
  
  MD <- merge(MD,EngleD[,.(NewArea2,Region,PovertyLine,Engel)],by=c("Region","NewArea2"))
  #MD<-MD[Region=="Urban" & NewArea==2301]
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight* Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("ProvinceCode")][order(ProvinceCode)]
 
   MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","NewArea2")][order(Region,NewArea2)]
  #MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

  y2<-EngleD[Region=="Urban",.(PovertyLine,NewArea2)]
  y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PovertyLine)])
  ggplot(y2, aes(x = y2$NewArea, y = y2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  x2<-EngleD[Region=="Rural",.(PovertyLine,NewArea2)]
  x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PovertyLine)])
  ggplot(x2, aes(x = x2$NewArea, y = x2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  if (year==93) {
  EngleD93<-EngleD[,PovertyLine9396:=PovertyLine*1.119*1.09*1.096]
  save(EngleD93,file="EngleD93.rda")
  }
  
  if (year==94) {
  EngleD94<-EngleD[,PovertyLine9496:=PovertyLine*1.09*1.096]
  save(EngleD94,file="EngleD94.rda")
  }
  
  if (year==95) {
  EngleD95<-EngleD[,PovertyLine9596:=PovertyLine*1.096]
  save(EngleD95,file="EngleD95.rda")
  }
  
  if (year==96) {
    load(file="EngleD95.rda" )
    load(file="EngleD94.rda" )
    load(file="EngleD93.rda" )
    EngleD96<-EngleD[,PovertyLine9696:=PovertyLine]
    save(EngleD96,file="EngleD96.rda")
    EngleD96[,N:=NULL]
    EngleD96[,Engel:=NULL]
    EngleD96[,FPLine:=NULL]
    EngleD96[,PovertyLine:=NULL]
    EngleD<-merge(EngleD96,EngleD95,by=c("Region","NewArea2"),all=TRUE)
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD<-merge(EngleD,EngleD94,by=c("Region","NewArea2"),all=TRUE)
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD<-merge(EngleD,EngleD93,by=c("Region","NewArea2"),all=TRUE)
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[is.na(EngleD)] <- 0
    w <- c( "PovertyLine9696", "PovertyLine9596",
            "PovertyLine9496", "PovertyLine9396")

    EngleD[, PovertyLineSum := Reduce(`+`, .SD), .SDcols=w]
    EngleD[, PovertyLineMean :=ifelse(NewArea2=="Chaharmahal" & Region=="Rural",
                                      PovertyLineSum/3,PovertyLineSum/4)]
    
    y2<-EngleD[Region=="Urban",.(PovertyLineMean,NewArea2)]
    y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PovertyLineMean)])
    ggplot(y2, aes(x = y2$NewArea, y = y2$PovertyLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    
    x2<-EngleD[Region=="Rural",.(PovertyLineMean,NewArea2)]
    x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PovertyLineMean)])
    ggplot(x2, aes(x = x2$NewArea, y = x2$PovertyLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
    }
  
  }


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")