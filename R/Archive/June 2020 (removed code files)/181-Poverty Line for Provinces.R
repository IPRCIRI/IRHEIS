#181-Poverty Line for Provinces
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
    MD[,NewPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]
    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }
  
  MD[,FinalFoodPoor:=OldPoor]
  
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  MDFinalfood<-MD[,.(HHID,Region,NewArea,NewArea_Name,Percentile,FinalFoodPoor)]
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  
  EngleD <- MD[TOriginalFoodExpenditure_Per<1.2*FPLine & TOriginalFoodExpenditure_Per>0.8*FPLine,
               .(.N,Engel=weighted.median(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine),ProvinceCode=mean(ProvinceCode)),by=.(Region,NewArea_Name)]
  EngleD[,PovertyLine:=FPLine/Engel]
  
  MD[,EngelPersonal:=TOriginalFoodExpenditure/Total_Exp_Month]
  TD<-MD[,PersonalPLine:=FPLine/EngelPersonal]
  save(TD,file = paste0(Settings$HEISProcessedPath,"Y",year,"MD4test.rda"))
  
  MD <- merge(MD,EngleD[,.(NewArea_Name,Region,PovertyLine,Engel)],by=c("Region","NewArea_Name"))
  #MD<-MD[Region=="Urban" & NewArea==2301]
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight* Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("ProvinceCode")][order(ProvinceCode)]
  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","NewArea_Name")][order(Region,NewArea_Name)]
  #MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","cluster3")]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  y2<-EngleD[Region=="Urban",.(PovertyLine,NewArea_Name)]
  y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PovertyLine)])
  ggplot(y2, aes(x = y2$NewArea, y = y2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  x2<-EngleD[Region=="Rural",.(PovertyLine,NewArea_Name)]
  x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PovertyLine)])
  ggplot(x2, aes(x = x2$NewArea, y = x2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  
  load(file="Inflation9096.rda")
  EngleD<-merge(EngleD,Inflation9096,by="ProvinceCode")
  
  if (year==90) {
    EngleD90<-EngleD[,PovertyLine9096:=PovertyLine*y9091*y9192*y9293*y9394*y9495*y9596]
    EngleD90[,y9091:=NULL]
    EngleD90[,y9192:=NULL]
    EngleD90[,y9293:=NULL]
    EngleD90[,y9394:=NULL]
    EngleD90[,y9495:=NULL]
    EngleD90[,y9596:=NULL]
    save(EngleD90,file="EngleD90.rda")
  }
  
  if (year==91) {
    EngleD91<-EngleD[,PovertyLine9196:=PovertyLine*y9192*y9293*y9394*y9495*y9596]
    EngleD91[,y9091:=NULL]
    EngleD91[,y9192:=NULL]
    EngleD91[,y9293:=NULL]
    EngleD91[,y9394:=NULL]
    EngleD91[,y9495:=NULL]
    EngleD91[,y9596:=NULL]
    save(EngleD91,file="EngleD91.rda")
  }
  
  if (year==92) {
    EngleD92<-EngleD[,PovertyLine9296:=PovertyLine*y9293*y9394*y9495*y9596]
    EngleD92[,y9091:=NULL]
    EngleD92[,y9192:=NULL]
    EngleD92[,y9293:=NULL]
    EngleD92[,y9394:=NULL]
    EngleD92[,y9495:=NULL]
    EngleD92[,y9596:=NULL]
    save(EngleD92,file="EngleD92.rda")
  }
  
  if (year==93) {
    EngleD93<-EngleD[,PovertyLine9396:=PovertyLine*y9394*y9495*y9596]
    EngleD93[,y9091:=NULL]
    EngleD93[,y9192:=NULL]
    EngleD93[,y9293:=NULL]
    EngleD93[,y9394:=NULL]
    EngleD93[,y9495:=NULL]
    EngleD93[,y9596:=NULL]
    save(EngleD93,file="EngleD93.rda")
  }
  
  if (year==94) {
    EngleD94<-EngleD[,PovertyLine9496:=PovertyLine*y9495*y9596]
    EngleD94[,y9091:=NULL]
    EngleD94[,y9192:=NULL]
    EngleD94[,y9293:=NULL]
    EngleD94[,y9394:=NULL]
    EngleD94[,y9495:=NULL]
    EngleD94[,y9596:=NULL]
    save(EngleD94,file="EngleD94.rda")
  }
  
  if (year==95) {
    EngleD95<-EngleD[,PovertyLine9596:=PovertyLine*y9596]
    EngleD95[,y9091:=NULL]
    EngleD95[,y9192:=NULL]
    EngleD95[,y9293:=NULL]
    EngleD95[,y9394:=NULL]
    EngleD95[,y9495:=NULL]
    EngleD95[,y9596:=NULL]
    save(EngleD95,file="EngleD95.rda")
  }
  
  if (year==96) {
    load(file="EngleD95.rda" )
    load(file="EngleD94.rda" )
    load(file="EngleD93.rda" )
    load(file="EngleD92.rda" )
    load(file="EngleD91.rda" )
    load(file="EngleD90.rda" )
    EngleD96<-EngleD[,PovertyLine9696:=PovertyLine]
    save(EngleD96,file="EngleD96.rda")
    EngleD96[,N:=NULL]
    EngleD96[,Engel:=NULL]
    EngleD96[,FPLine:=NULL]
    EngleD96[,PovertyLine:=NULL]
    EngleD96[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD96,EngleD95,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091:=NULL]
    EngleD[,y9192:=NULL]
    EngleD[,y9293:=NULL]
    EngleD[,y9394:=NULL]
    EngleD[,y9495:=NULL]
    EngleD[,y9596:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD,EngleD94,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091:=NULL]
    EngleD[,y9192:=NULL]
    EngleD[,y9293:=NULL]
    EngleD[,y9394:=NULL]
    EngleD[,y9495:=NULL]
    EngleD[,y9596:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD,EngleD93,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091:=NULL]
    EngleD[,y9192:=NULL]
    EngleD[,y9293:=NULL]
    EngleD[,y9394:=NULL]
    EngleD[,y9495:=NULL]
    EngleD[,y9596:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD,EngleD92,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091:=NULL]
    EngleD[,y9192:=NULL]
    EngleD[,y9293:=NULL]
    EngleD[,y9394:=NULL]
    EngleD[,y9495:=NULL]
    EngleD[,y9596:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD,EngleD91,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091:=NULL]
    EngleD[,y9192:=NULL]
    EngleD[,y9293:=NULL]
    EngleD[,y9394:=NULL]
    EngleD[,y9495:=NULL]
    EngleD[,y9596:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD<-merge(EngleD,EngleD90,by=c("Region","NewArea_Name"),all=TRUE)
    EngleD[,y9091.x:=NULL]
    EngleD[,y9192.x:=NULL]
    EngleD[,y9293.x:=NULL]
    EngleD[,y9394.x:=NULL]
    EngleD[,y9495.x:=NULL]
    EngleD[,y9091.y:=NULL]
    EngleD[,y9192.y:=NULL]
    EngleD[,y9293.y:=NULL]
    EngleD[,y9394.y:=NULL]
    EngleD[,y9495.y:=NULL]
    EngleD[,N:=NULL]
    EngleD[,Engel:=NULL]
    EngleD[,FPLine:=NULL]
    EngleD[,PovertyLine:=NULL]
    EngleD[,ProvinceCode:=NULL]
    EngleD[is.na(EngleD)] <- 0
    w <- c( "PovertyLine9696", "PovertyLine9596",
            "PovertyLine9496", "PovertyLine9396",
            "PovertyLine9296",
            "PovertyLine9196",
            "PovertyLine9096"
            )
    
    EngleD[, PovertyLineSum := Reduce(`+`, .SD), .SDcols=w]
    EngleD[, PovertyLineMean :=ifelse((NewArea_Name=="Khorasan_Jonoobi" & Region=="Urban") |
                                        (NewArea_Name=="Alborz"),
                                      PovertyLineSum/5,PovertyLineSum/7)]
    
    
    y2<-EngleD[Region=="Urban",.(PovertyLineMean,NewArea_Name)]
    y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PovertyLineMean)])
    ggplot(y2, aes(x = y2$NewArea, y = y2$PovertyLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    
    x2<-EngleD[Region=="Rural",.(PovertyLineMean,NewArea_Name)]
    x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PovertyLineMean)])
    ggplot(x2, aes(x = x2$NewArea, y = x2$PovertyLineMean)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
  }
  
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")