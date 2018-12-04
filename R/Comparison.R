# Comparison
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
year<-95

#####################Compare Engle#############################
#load old results
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanaboveEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralaboveEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanunderEngel.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralunderEngel.rda"))

#Engel<-rbind(UrbanEngel,RuralEngel)
#EngelUnder<-rbind(UrbanunderEngel,RuralunderEngel)
#EngelAbove<-rbind(UrbanaboveEngel,RuralaboveEngel)

Engel<-UrbanEngel
EngelUnder<-UrbanunderEngel
EngelAbove<-UrbanaboveEngel

EngelUnder<-EngelUnder[,Oldplace:=1]
Engel<-Engel[,Oldplace:=2]
EngelAbove<-EngelAbove[,Oldplace:=3]

RecentEngel<-rbind(Engel,EngelUnder,EngelAbove)

#load new results
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewRural.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUnderUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUnderRural.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewAboveUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewAboveRural.rda"))

#EngleNew<-rbind(EngleNewUrban,EngleNewRural)
#EngleNewUnder<-rbind(EngleNewUnderUrban,EngleNewUnderRural)
#EngleNewAbove<-rbind(EngleNewAboveUrban,EngleNewAboveRural)

EngleNew<-EngleNewUrban[NewArea==2301]
EngleNewUnder<-EngleNewUnderUrban[NewArea==2301]
EngleNewAbove<-EngleNewAboveUrban[NewArea==2301]

EngleNewUnder<-EngleNewUnder[,Newplace:=1]
EngleNew<-EngleNew[,Newplace:=2]
EngleNewAbove<-EngleNewAbove[,Newplace:=3]

AfterEngel<-rbind(EngleNew,EngleNewUnder,EngleNewAbove)

#merge old and new
Total<-merge(AfterEngel,RecentEngel,by="HHID",all.x = TRUE)
Total2<-Total[Oldplace==Newplace]

Total<-Total[,matrixplace:=ifelse(Oldplace==1 & Newplace==1,1,
                            ifelse(Oldplace==1 & Newplace==2,2,
                            ifelse(Oldplace==1 & Newplace==3,3,
                            ifelse(Oldplace==2 & Newplace==1,4,
                            ifelse(Oldplace==2 & Newplace==2,5,
                            ifelse(Oldplace==2 & Newplace==3,6,
                            ifelse(Oldplace==3 & Newplace==1,7,
                            ifelse(Oldplace==3 & Newplace==2,8,9))))))))]

Total<-Total[,Engel:=weighted.mean(FoodExpenditure_Per_total/Total_Exp_Month_Per_nondurable,Weight),by=matrixplace]   
Total<-Total[,.(Oldplace,Newplace,matrixplace,Engel)]                              
Total3<-Total[,.(.N,Engel=mean(Engel)),by=matrixplace]

#########################Compare Final poors#######################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFinalPoorUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFinalPoorRural.rda"))
OldFinalPoor<-rbind(OldFinalPoorUrban,OldFinalPoorRural)

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"NewFinalPoor.rda"))
NewFinalPoorUrban<-NewFinalPoor[Region=="Urban" & NewArea==2301]
NewFinalPoorRural<-NewFinalPoor[Region=="Rural"]

FinalPoor<-merge(NewFinalPoorUrban,OldFinalPoorUrban,by="HHID",all.x=TRUE)
FinalPoor<-FinalPoor[,matrixplace:=ifelse(Poor11==0 & FinalPoor==0,1,
                                   ifelse(Poor11==0 & FinalPoor==1,2,
                                   ifelse(Poor11==1 & FinalPoor==0,3,4)))]

FinalPoor<-FinalPoor[,.(Poor11,FinalPoor,matrixplace)]                              
FinalPoor<-FinalPoor[,.(.N),by=matrixplace]

#########################Compare initial poors#######################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldfirstUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldfirstRural.rda"))
Oldfirst<-rbind(OldfirstRural,OldfirstUrban)

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FirstSMD.rda"))
FirstUrban<-FirstSMD[Region=="Urban"]
FirstRural<-FirstSMD[Region=="Rural"]

Fisrt<-merge(FirstSMD,Oldfirst,by="HHID",all=TRUE)
Fisrt<-Fisrt[,matrixplace:=ifelse(Poor==0 & Realfirstpoor==0,1,
                           ifelse(Poor==0 & Realfirstpoor==1,2,
                           ifelse(Poor==1 & Realfirstpoor==0,3,4)))]

Fisrt<-Fisrt[,.(Poor,Realfirstpoor,matrixplace)]                              
Fisrt<-Fisrt[,.(.N),by=matrixplace]


######################Compare final food poors#############################
#Compare final food poors
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanFinalfood.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralFinalfood.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFoodUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFoodRural.rda"))

OldFood<-rbind(OldFoodRural,OldFoodUrban)
FinalFood<-rbind(UrbanFinalfood,RuralFinalfood)

Tehranfinalfood<-UrbanFinalfood[NewArea==2301]

FoodPoors<-merge(OldFoodRural,RuralFinalfood,by="HHID",all = TRUE)
FoodPoors<-FoodPoors[,matrixplace:=ifelse(Poor9==0 & FinalFoodPoor==0,1,
                                   ifelse(Poor9==0 & FinalFoodPoor==1,2,
                                   ifelse(Poor9==1 & FinalFoodPoor==0,3,4)))]

FoodPoors<-FoodPoors[,.(Poor9,FinalFoodPoor,matrixplace)]                              
FoodPoors<-FoodPoors[,.(.N),by=matrixplace]


#########################Compare initial poors- after realize#######################################
#Compare initial poors
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"CBNRural.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"CBNUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor2.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor3.rda"))

Oldpoors<-rbind(CBNUrban,CBNRural)
Newpoors<-rbind(MDU,MDR)
TotalPoors<-merge(Oldpoors,Newpoors,by="HHID",all = TRUE)
TotalPoors<-TotalPoors[,matrixplace:=ifelse(Poor2==0 & InitialPoor==0,1,
                                            ifelse(Poor2==0 & InitialPoor==1,2,
                                                   ifelse(Poor2==1 & InitialPoor==0,3,4)))]

TotalPoors<-TotalPoors[,.(Poor2,InitialPoor,matrixplace)]                              
TotalPoors<-TotalPoors[,.(.N),by=matrixplace]




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
