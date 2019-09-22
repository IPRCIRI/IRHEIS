# 91-Calculation of EV and CV- 2nd edition
# 
#
# Copyright © 2019: Arin Shahbazian
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
  if(year %in% 84:97){
    TF[,MorghExpenditure:=as.numeric(MorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,MorghPrice:=as.numeric(MorghPrice)]
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
################  Ghand  ########################
################################################
cat("\n\n================ Ghand =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))

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
  
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","GhandPrice","GhandExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:97){
    TF[,GhandExpenditure:=as.numeric(GhandExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,GhandPrice:=as.numeric(GhandPrice)]
  TF[is.na(TF)] <- 0
  TF[,GhandKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  GhandData <- TF[,lapply(.SD,sum),by=HHID]
  save(GhandData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Ghands.rda"))
}

################################################
################  Shekar  ########################
################################################
cat("\n\n================ Shekar =====================================\n")

FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shekar))

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
  pcols <- intersect(names(TF),c("HHID","Code","Kilos","Grams","ShekarPrice","ShekarExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:97){
    TF[,ShekarExpenditure:=as.numeric(ShekarExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,ShekarPrice:=as.numeric(ShekarPrice)]
  TF[is.na(TF)] <- 0
  TF[,ShekarKG:=Kilos+(Grams*0.001)]
  
  TF[,Code:=NULL]
  TF[,Kilos:=NULL]
  TF[,Grams:=NULL]
  TF[is.na(TF)] <- 0
  ShekarData <- TF[,lapply(.SD,sum),by=HHID]
  save(ShekarData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Shekars.rda"))
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
  if(year %in% 84:97){
    TF[,BerenjFExpenditure:=as.numeric(BerenjFExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,BerenjPrice:=as.numeric(BerenjPrice)]
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
  if(year %in% 84:97){
    TF[,RoghanExpenditure:=as.numeric(RoghanExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,RoghanPrice:=as.numeric(RoghanPrice)]
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
  if(year %in% 84:97){
    TF[,TokhmemorghExpenditure:=as.numeric(TokhmemorghExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
  TF[,TokhmemorghPrice:=as.numeric(TokhmemorghPrice)]
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
SMD<-MD[,.(HHID,Decile,Weight,Size,EqSizeRevOECD,
           Total_Exp_Month,Total_Exp_Month_Per,TOriginalFoodExpenditure)]
#Total_Exp_Month_nondurable,Total_Exp_Month_Per_nondurable)]

################################################
#######  Merge Information  #############
################################################

Total<-merge(HHBase,SMD,all = TRUE)
Total<-merge(Total,MorghData,all = TRUE)
Total<-merge(Total,ShekarData,all = TRUE)
Total<-merge(Total,GhandData,all = TRUE)
Total<-merge(Total,BerenjData,all = TRUE)
Total<-merge(Total,RoghanData,all = TRUE)
Total<-merge(Total,TokhmemorghData,all = TRUE)

for (col in c("MorghExpenditure","MorghKG",
              "ShekarExpenditure","ShekarKG",
              "GhandExpenditure","GhandKG",
              "BerenjFExpenditure","BerenjKG",
              "RoghanExpenditure","RoghanKG",
              "TokhmemorghExpenditure","TokhmemorghKG")) Total[is.na(get(col)), (col) := 0]
#"MorghPrice","ShekarPrice","GhandPrice",
#"BerenjPrice","RoghanPrice","TokhmemorghPrice"

Total<-Total[Decile %in% 1:10]

#Price Indexes
load(file="Index_Dataset97.rda")
Total<-merge(Total,Index_Dataset97,by=c("Region","Month"),all.x=TRUE)

Berenj_Esfand96<-114.4
Morgh_Esfand96<-122.1
Ghand_Esfand96<-108.4
Shekar_Esfand96<-108.4
Roghan_Esfand96<-113.7
Tokhmemorgh_Esfand96<-120.9 

Berenj_Esfand97<-145.6
Morgh_Esfand97<-265.6
Ghand_Esfand97<-170.3
Shekar_Esfand97<-170.3
Roghan_Esfand97<-168
Tokhmemorgh_Esfand97<-175.3




############New Prices###########
Total[,BerenjP0:=BerenjPrice*Berenj_Esfand96/BerenjIndex]
Total[,MorghP0:=MorghPrice*Morgh_Esfand96/MorghIndex]
Total[,GhandP0:=GhandPrice*Ghand_Esfand96/GhandIndex]
Total[,ShekarP0:=ShekarPrice*Ghand_Esfand96/GhandIndex]
Total[,RoghanP0:=RoghanPrice*Roghan_Esfand96/RoghanIndex]
Total[,TokhmemorghP0:=TokhmemorghPrice*Tokhmemorgh_Esfand96/EggIndex]

Total[,BerenjP1:=BerenjPrice*Berenj_Esfand97/BerenjIndex]
Total[,MorghP1:=MorghPrice*Morgh_Esfand97/MorghIndex]
Total[,GhandP1:=GhandPrice*Ghand_Esfand97/GhandIndex]
Total[,ShekarP1:=ShekarPrice*Ghand_Esfand97/GhandIndex]
Total[,RoghanP1:=RoghanPrice*Roghan_Esfand97/RoghanIndex]
Total[,TokhmemorghP1:=TokhmemorghPrice*Tokhmemorgh_Esfand97/EggIndex]

Total[,BerenjP2:=BerenjP0*2.57]
Total[,MorghP2:=MorghP0*2.57]
Total[,GhandP2:=GhandP0*2.57]
Total[,ShekarP2:=ShekarP0*2.57]
Total[,RoghanP2:=RoghanP0*2.57]
Total[,TokhmemorghP2:=TokhmemorghP0*2.57]

################################################
################  CV and EV  #################
################################################

Total<-Total[,GExpenditures:=BerenjFExpenditure+GhandExpenditure+
               ShekarExpenditure+MorghExpenditure+
               RoghanExpenditure+TokhmemorghExpenditure]
GExpenditures<-Total[,weighted.mean(GExpenditures,Weight)]


Total<-Total[,BerenjShare:=BerenjFExpenditure/GExpenditures]
Total<-Total[,GhandShare:=GhandExpenditure/GExpenditures]
Total<-Total[,ShekarShare:=ShekarExpenditure/GExpenditures]
Total<-Total[,MorghShare:=MorghExpenditure/GExpenditures]
Total<-Total[,RoghanShare:=RoghanExpenditure/GExpenditures]
Total<-Total[,TokhmemorghShare:=TokhmemorghExpenditure/GExpenditures]


Total<-Total[,CV2:=GExpenditures*(1-((GhandP2/GhandP1)^GhandShare)*((ShekarP2/ShekarP1)^ShekarShare)*
                                    ((BerenjP2/BerenjP1)^BerenjShare)*((RoghanP2/RoghanP1)^RoghanShare)*
                                    ((MorghP2/MorghP1)^MorghShare)*((TokhmemorghP2/TokhmemorghP1)^TokhmemorghShare))]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV2/GExpenditures,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]


Total<-Total[,CV3:=GExpenditures*(1-((GhandP2/GhandP0)^GhandShare)*((ShekarP2/ShekarP0)^ShekarShare)*
                                    ((BerenjP2/BerenjP0)^BerenjShare)*((RoghanP2/RoghanP0)^RoghanShare)*
                                    ((MorghP2/MorghP0)^MorghShare)*((TokhmemorghP2/TokhmemorghP0)^TokhmemorghShare))]
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

GhandShare<-Total[,weighted.mean(GhandExpenditure,
                               Weight)/weighted.mean(GExpenditures,Weight)]

ShekarShare<-Total[,weighted.mean(ShekarExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

MorghShare<-Total[,weighted.mean(MorghExpenditure,
                                 Weight)/weighted.mean(GExpenditures,Weight)]

RoghanShare<-Total[,weighted.mean(RoghanExpenditure,
                                  Weight)/weighted.mean(GExpenditures,Weight)]

TokhmemorghShare<-Total[,weighted.mean(TokhmemorghExpenditure,
                                       Weight)/weighted.mean(GExpenditures,Weight)]


#x<-Total[,.(Total_Exp_Month_Per_nondurable,Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
