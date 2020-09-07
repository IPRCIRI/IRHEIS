# Calculation of EV and CV
# 
#
# Copyright © 2017: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)
options(warn=-1)
################################################
################  Morgh  ########################
################################################
cat("\n\n================ Morgh =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","MorghPrice","MorghExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,MorghExpenditure:=as.numeric(MorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,MorghKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  MorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(MorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Morghs.rda"))
}

################################################
################  Cow  ########################
################################################
cat("\n\n================ Cow =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Cow))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }

  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","CowPrice","CowExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,CowExpenditure:=as.numeric(CowExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,CowKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  CowData <- TF[,lapply(.SD,sum),by=HHID]
  save(CowData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Cows.rda"))
}

################################################
################  Sheep  ########################
################################################
cat("\n\n================ Sheep =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sheep))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","SheepPrice","SheepExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,SheepExpenditure:=as.numeric(SheepExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,SheepKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  SheepData <- TF[,lapply(.SD,sum),by=HHID]
  save(SheepData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sheeps.rda"))
}
################################################
################  Berenj  ########################
################################################
cat("\n\n================ Berenj =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_BerenjF))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","BerenjPrice","BerenjFExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,BerenjFExpenditure:=as.numeric(BerenjFExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,BerenjKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  BerenjData <- TF[,lapply(.SD,sum),by=HHID]
  save(BerenjData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Berenjs.rda"))
}
################################################
################  Roghan  ########################
################################################
cat("\n\n================ Roghan =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","RoghanPrice","RoghanExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,RoghanExpenditure:=as.numeric(RoghanExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,RoghanKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  RoghanData <- TF[,lapply(.SD,sum),by=HHID]
  save(RoghanData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Roghans.rda"))
}
################################################
################  Tokhmemorgh  ########################
################################################
cat("\n\n================ Tokhmemorgh =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- FoodTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","TokhmemorghPrice","TokhmemorghExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode | Code==11511]
  if(year %in% 84:96){
    TF[,TokhmemorghExpenditure:=as.numeric(TokhmemorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[is.na(TF)] <- 0
  TF[,TokhmemorghKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  TokhmemorghData <- TF[,lapply(.SD,sum),by=HHID]
  save(TokhmemorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
}

################################################
#######  Load Additional Information  #############
################################################
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
HHBase<-HHBase[,.(HHID,Region,ProvinceCode,Month)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
SMD<-MD[,.(HHID,Decile,Weight,Size,EqSizeOECD,
           Total_Exp_Month,Total_Exp_Month_Per,TOriginalFoodExpenditure)]
           #Total_Exp_Month_nondurable,Total_Exp_Month_Per_nondurable)]
           
################################################
#######  Merge Information  #############
################################################

Total<-merge(HHBase,SMD,all = TRUE)
Total<-merge(Total,MorghData,all = TRUE)
Total<-merge(Total,SheepData,all = TRUE)
Total<-merge(Total,CowData,all = TRUE)
Total<-merge(Total,BerenjData,all = TRUE)
Total<-merge(Total,RoghanData,all = TRUE)
Total<-merge(Total,TokhmemorghData,all = TRUE)

for (col in c("MorghExpenditure","MorghKG",
              "SheepExpenditure","SheepKG",
              "CowExpenditure","CowKG",
              "BerenjFExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG",
              "TokhmemorghExpenditure","TokhmemorghKG")) Total[is.na(get(col)), (col) := 0]
#"MorghPrice","SheepPrice","CowPrice",
#"BerenjPrice","RoghanPrice","TokhmemorghPrice"

Total<-Total[Decile %in% 1:10]

#Price Indexes
load(file="Index_Dataset.rda")
Total<-merge(Total,Index_Dataset,by=c("Month"),all.x=TRUE)

Berenj_Azar97<-185.5
Morgh_Azar97<-151.3
Cow_Azar97<-188.5
Sheep_Azar97<-182.1
Roghan_Azar97<-159.9
Tokhmemorgh_Azar97<-218.5

Berenj_Elastisity<- -0.77
Morgh_Elastisity<- -0.89
Cow_Elastisity<- -0.89
Sheep_Elastisity<- -0.89
Roghan_Elastisity<- -0.81
Tokhmemorgh_Elastisity<- -0.81

############New Prices###########
#Initial Prices
#Total[,BerenjP1:=BerenjPrice*Berenj_Azar97/Bt]
#Total[,MorghP1:=MorghPrice*Morgh_Azar97/Mt]
#Total[,CowP1:=CowPrice*Cow_Azar97/Ct]
#Total[,SheepP1:=SheepPrice*Sheep_Azar97/St]
#Total[,RoghanP1:=RoghanPrice*Roghan_Azar97/Rt]
#Total[,TokhmemorghP1:=TokhmemorghPrice*Tokhmemorgh_Azar97/Tt]

Total[,BerenjP1:=BerenjPrice]
Total[,MorghP1:=MorghPrice]
Total[,CowP1:=CowPrice]
Total[,SheepP1:=SheepPrice]
Total[,RoghanP1:=RoghanPrice]
Total[,TokhmemorghP1:=TokhmemorghPrice]

#Second Prices
Total[,BerenjP2:=BerenjPrice*2]
Total[,MorghP2:=MorghPrice*2]
Total[,CowP2:=CowPrice*2]
Total[,SheepP2:=SheepPrice*2]
Total[,RoghanP2:=RoghanPrice*2]
Total[,TokhmemorghP2:=TokhmemorghPrice*2]

Total[,BerenjP3:=BerenjPrice*3]
Total[,MorghP3:=MorghPrice*3]
Total[,CowP3:=CowPrice*3]
Total[,SheepP3:=SheepPrice*3]
Total[,RoghanP3:=RoghanPrice*3]
Total[,TokhmemorghP3:=TokhmemorghPrice*3]

################################################
############  CV and EV  #############
################################################

Total<-Total[,GExpenditures:=BerenjFExpenditure+CowExpenditure+
               SheepExpenditure+MorghExpenditure+
               RoghanExpenditure+TokhmemorghExpenditure]
GExpenditures<-Total[,weighted.mean(GExpenditures,Weight)]


Total<-Total[,BerenjShare:=BerenjFExpenditure/GExpenditures]
Total<-Total[,CowShare:=CowExpenditure/GExpenditures]
Total<-Total[,SheepShare:=SheepExpenditure/GExpenditures]
Total<-Total[,MorghShare:=MorghExpenditure/GExpenditures]
Total<-Total[,RoghanShare:=RoghanExpenditure/GExpenditures]
Total<-Total[,TokhmemorghShare:=TokhmemorghExpenditure/GExpenditures]

Total<-Total[,EV2:=GExpenditures*(((CowP1/CowP2)^CowShare)*((SheepP1/SheepP2)^SheepShare)*
                            ((BerenjP1/BerenjP2)^BerenjShare)*((RoghanP1/RoghanP2)^RoghanShare)*
                            ((MorghP1/MorghP2)^MorghShare)*((TokhmemorghP1/TokhmemorghP2)^TokhmemorghShare)-1)]
Total[,weighted.mean(EV2,Weight,na.rm = TRUE)]
Total[,weighted.mean(EV2,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(EV2,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(EV2,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(EV2/TOriginalFoodExpenditure,Weight,na.rm = TRUE)]
Total[,weighted.mean(EV2/TOriginalFoodExpenditure,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(EV2/TOriginalFoodExpenditure,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]

Total<-Total[,CV2:=GExpenditures*(1-((CowP2/CowP1)^CowShare)*((SheepP2/SheepP1)^SheepShare)*
                                   ((BerenjP2/BerenjP1)^BerenjShare)*((RoghanP2/RoghanP1)^RoghanShare)*
                                   ((MorghP2/MorghP1)^MorghShare)*((TokhmemorghP2/TokhmemorghP1)^TokhmemorghShare))]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]

Total<-Total[,EV3:=GExpenditures*(((CowP1/CowP3)^CowShare)*((SheepP1/SheepP3)^SheepShare)*
                                   ((BerenjP1/BerenjP3)^BerenjShare)*((RoghanP1/RoghanP3)^RoghanShare)*
                                   ((MorghP1/MorghP3)^MorghShare)*((TokhmemorghP1/TokhmemorghP3)^TokhmemorghShare)-1)]
Total[,weighted.mean(EV3,Weight,na.rm = TRUE)]
Total[,weighted.mean(EV3,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(EV3,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(EV3,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(EV3/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(EV3/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(EV3/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]

Total<-Total[,CV3:=GExpenditures*(1-((CowP3/CowP1)^CowShare)*((SheepP3/SheepP1)^SheepShare)*
                                   ((BerenjP3/BerenjP1)^BerenjShare)*((RoghanP3/RoghanP1)^RoghanShare)*
                                   ((MorghP3/MorghP1)^MorghShare)*((TokhmemorghP3/TokhmemorghP1)^TokhmemorghShare))]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV3/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]


################################################
############  Other  #############
################################################
BerenjShare<-Total[,weighted.mean(BerenjFExpenditure,
                                  Weight)/weighted.mean(GExpenditures,Weight)]

CowShare<-Total[,weighted.mean(CowExpenditure,
                               Weight)/weighted.mean(GExpenditures,Weight)]

SheepShare<-Total[,weighted.mean(SheepExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

MorghShare<-Total[,weighted.mean(MorghExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

RoghanShare<-Total[,weighted.mean(RoghanExpenditure,
                                  Weight)/weighted.mean(GExpenditures,Weight)]

TokhmemorghShare<-Total[,weighted.mean(TokhmemorghExpenditure,
                                       Weight)/weighted.mean(GExpenditures,Weight)]


BerenjAP1<-Total[,weighted.mean(BerenjP1,Weight,na.rm = TRUE)]
BerenjAP2<-Total[,weighted.mean(BerenjP2,Weight,na.rm = TRUE)]
BerenjAP3<-Total[,weighted.mean(BerenjP3,Weight,na.rm = TRUE)]
BerenjAQ<-Total[,weighted.mean(BerenjKG,Weight)]

CowAP1<-Total[,weighted.mean(CowP1,Weight,na.rm = TRUE)]
CowAP2<-Total[,weighted.mean(CowP2,Weight,na.rm = TRUE)]
CowAP3<-Total[,weighted.mean(CowP3,Weight,na.rm = TRUE)]
CowAQ<-Total[,weighted.mean(CowKG,Weight)]


SheepAP1<-Total[,weighted.mean(SheepP1,Weight,na.rm = TRUE)]
SheepAP2<-Total[,weighted.mean(SheepP2,Weight,na.rm = TRUE)]
SheepAP3<-Total[,weighted.mean(SheepP3,Weight,na.rm = TRUE)]
SheepAQ<-Total[,weighted.mean(SheepKG,Weight)]

MorghAP1<-Total[,weighted.mean(MorghP1,Weight,na.rm = TRUE)]
MorghAP2<-Total[,weighted.mean(MorghP2,Weight,na.rm = TRUE)]
MorghAP3<-Total[,weighted.mean(MorghP3,Weight,na.rm = TRUE)]
MorghAQ<-Total[,weighted.mean(MorghKG,Weight)]

RoghanAP1<-Total[,weighted.mean(RoghanP1,Weight,na.rm = TRUE)]
RoghanAP2<-Total[,weighted.mean(RoghanP2,Weight,na.rm = TRUE)]
RoghanAP3<-Total[,weighted.mean(RoghanP3,Weight,na.rm = TRUE)]
RoghanAQ<-Total[,weighted.mean(RoghanKG,Weight)]

TokhmemorghAP1<-Total[,weighted.mean(TokhmemorghP1,Weight,na.rm = TRUE)]
TokhmemorghAP2<-Total[,weighted.mean(TokhmemorghP2,Weight,na.rm = TRUE)]
TokhmemorghAP3<-Total[,weighted.mean(TokhmemorghP3,Weight,na.rm = TRUE)]
TokhmemorghAQ<-Total[,weighted.mean(TokhmemorghKG,Weight)]



EV2<-GExpenditures*(((CowAP1/CowAP2)^CowShare)*((SheepAP1/SheepAP2)^SheepShare)*
                      ((BerenjAP1/BerenjAP2)^BerenjShare)*((RoghanAP1/RoghanAP2)^RoghanShare)*
                      ((MorghAP1/MorghAP2)^MorghShare)*((TokhmemorghAP1/TokhmemorghAP2)^TokhmemorghShare)-1)

CV2<-GExpenditures*(1-((CowAP2/CowAP1)^CowShare)*((SheepAP2/SheepAP1)^SheepShare)*
                      ((BerenjAP2/BerenjAP1)^BerenjShare)*((RoghanAP2/RoghanAP1)^RoghanShare)*
                      ((MorghAP2/MorghAP1)^MorghShare)*((TokhmemorghAP2/TokhmemorghAP1)^TokhmemorghShare)-1)

EV3<-GExpenditures*(((CowAP1/CowAP3)^CowShare)*((SheepAP1/SheepAP3)^SheepShare)*
                      ((BerenjAP1/BerenjAP3)^BerenjShare)*((RoghanAP1/RoghanAP3)^RoghanShare)*
                      ((MorghAP1/MorghAP3)^MorghShare)*((TokhmemorghAP1/TokhmemorghAP3)^TokhmemorghShare)-1)

CV3<-GExpenditures*(1-((CowAP3/CowAP1)^CowShare)*((SheepAP3/SheepAP1)^SheepShare)*
                      ((BerenjAP3/BerenjAP1)^BerenjShare)*((RoghanAP3/RoghanAP1)^RoghanShare)*
                      ((MorghAP3/MorghAP1)^MorghShare)*((TokhmemorghAP3/TokhmemorghAP1)^TokhmemorghShare)-1)


Total[,weighted.mean(Size,Weight)]
Total[,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]
Total[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]

Total[,weighted.mean(BerenjKG,Weight)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(RoghanKG,Weight)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(MorghKG,Weight)]
Total[,weighted.mean(MorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(MorghKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(CowKG,Weight)]
Total[,weighted.mean(CowKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(CowKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(SheepKG,Weight)]
Total[,weighted.mean(SheepKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(SheepKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(TokhmemorghKG,Weight)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Decile)][order(Decile)]

#x<-Total[,.(Total_Exp_Month_Per_nondurable,Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
