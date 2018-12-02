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

Engel<-rbind(UrbanEngel,RuralEngel)
EngelUnder<-rbind(UrbanunderEngel,RuralunderEngel)
EngelAbove<-rbind(UrbanaboveEngel,RuralaboveEngel)

#Engel<-RuralEngel
#EngelUnder<-RuralunderEngel
#EngelAbove<-RuralaboveEngel

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

EngleNew<-rbind(EngleNewUrban,EngleNewRural)
EngleNewUnder<-rbind(EngleNewUnderUrban,EngleNewUnderRural)
EngleNewAbove<-rbind(EngleNewAboveUrban,EngleNewAboveRural)

#EngleNew<-EngleNewRural
#EngleNewUnder<-EngleNewUnderRural
#EngleNewAbove<-EngleNewAboveRural

EngleNewUnder<-EngleNewUnder[,Newplace:=1]
EngleNew<-EngleNew[,Newplace:=2]
EngleNewAbove<-EngleNewAbove[,Newplace:=3]

AfterEngel<-rbind(EngleNew,EngleNewUnder,EngleNewAbove)

#merge old and new
Total<-merge(AfterEngel,RecentEngel,by="HHID",all = TRUE)
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

#########################Compare initial poors#######################################
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

######################Compare final food poors#############################
#Compare final food poors
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanFinalfood.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralFinalfood.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFoodUrban.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"OldFoodRural.rda"))

OldFood<-rbind(OldFoodRural,OldFoodUrban)
FinalFood<-rbind(UrbanFinalfood,RuralFinalfood)

FoodPoors<-merge(OldFood,FinalFood,by="HHID",all = TRUE)
FoodPoors<-FoodPoors[,matrixplace:=ifelse(Poor9==0 & FinalFoodPoor==0,1,
                                   ifelse(Poor9==0 & FinalFoodPoor==1,2,
                                   ifelse(Poor9==1 & FinalFoodPoor==0,3,4)))]

FoodPoors<-FoodPoors[,.(Poor9,FinalFoodPoor,matrixplace)]                              
FoodPoors<-FoodPoors[,.(.N),by=matrixplace]






endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
