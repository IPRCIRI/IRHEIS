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
  if(year %in% 84:96){
    TF[,GhandExpenditure:=as.numeric(GhandExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
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
  if(year %in% 84:96){
    TF[,ShekarExpenditure:=as.numeric(ShekarExpenditure)]
  }
  for (col in c("Kilos","Grams")) TF[is.na(get(col)), (col) := 0]
  TF[,Kilos:=as.numeric(Kilos)]
  TF[,Grams:=as.numeric(Grams)]
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
load(file="Index_Dataset.rda")
Total<-merge(Total,Index_Dataset,by=c("Month"),all.x=TRUE)

Berenj_Azar97<-185.5
Morgh_Azar97<-151.3
Ghand_Azar97<-188.5
Shekar_Azar97<-182.1
Roghan_Azar97<-159.9
Tokhmemorgh_Azar97<-218.5


############New Prices###########
#Initial Prices
#Total[,BerenjP1:=BerenjPrice*Berenj_Azar97/Bt]
#Total[,MorghP1:=MorghPrice*Morgh_Azar97/Mt]
#Total[,GhandP1:=GhandPrice*Ghand_Azar97/Ct]
#Total[,ShekarP1:=ShekarPrice*Shekar_Azar97/St]
#Total[,RoghanP1:=RoghanPrice*Roghan_Azar97/Rt]
#Total[,TokhmemorghP1:=TokhmemorghPrice*Tokhmemorgh_Azar97/Tt]

Total[,BerenjP1:=BerenjPrice]
Total[,MorghP1:=MorghPrice]
Total[,GhandP1:=GhandPrice]
Total[,ShekarP1:=ShekarPrice]
Total[,RoghanP1:=RoghanPrice]
Total[,TokhmemorghP1:=TokhmemorghPrice]

#Second Prices
Total[,BerenjP2:=BerenjPrice*2]
Total[,MorghP2:=MorghPrice*2]
Total[,GhandP2:=GhandPrice*2]
Total[,ShekarP2:=ShekarPrice*2]
Total[,RoghanP2:=RoghanPrice*2]
Total[,TokhmemorghP2:=TokhmemorghPrice*2]

Total[,BerenjP3:=BerenjPrice*3]
Total[,MorghP3:=MorghPrice*3]
Total[,GhandP3:=GhandPrice*3]
Total[,ShekarP3:=ShekarPrice*3]
Total[,RoghanP3:=RoghanPrice*3]
Total[,TokhmemorghP3:=TokhmemorghPrice*3]

################################################
############  CV and EV  #############
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


Total<-Total[,CV3:=GExpenditures*(1-((GhandP3/GhandP1)^GhandShare)*((ShekarP3/ShekarP1)^ShekarShare)*
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


  GhandAP1<-41021
  GhandAP2<-67828
  GhandAP3<-105422
  ShekarAP1<-32847
  ShekarAP2<-58547
  ShekarAP3<-84417
  BerenjAP1<-71855
  BerenjAP2<-85299
  BerenjAP3<-184667
  RoghanAP1<-53988
  RoghanAP2<-78253
  RoghanAP3<-138749
  MorghAP1<-76896
  MorghAP2<-148903
  MorghAP3<-197623
  TokhmemorghAP1<-90889
  TokhmemorghAP2<-95418
  TokhmemorghAP3<-233585
  
CV2<-GExpenditures*(1-((GhandAP3/GhandAP1)^GhandShare)*((ShekarAP3/ShekarAP1)^ShekarShare)*
                      ((BerenjAP3/BerenjAP1)^BerenjShare)*((RoghanAP3/RoghanAP1)^RoghanShare)*
                      ((MorghAP3/MorghAP1)^MorghShare)*((TokhmemorghAP3/TokhmemorghAP1)^TokhmemorghShare))


CV3<-GExpenditures*(1-((GhandAP3/GhandAP2)^GhandShare)*((ShekarAP3/ShekarAP2)^ShekarShare)*
                      ((BerenjAP3/BerenjAP2)^BerenjShare)*((RoghanAP3/RoghanAP2)^RoghanShare)*
                      ((MorghAP3/MorghAP2)^MorghShare)*((TokhmemorghAP3/TokhmemorghAP2)^TokhmemorghShare))

Total<-Total[,CV2:=GExpenditures*(1-((GhandAP3/GhandAP1)^GhandShare)*((ShekarAP3/ShekarAP1)^ShekarShare)*
                                    ((BerenjAP3/BerenjAP1)^BerenjShare)*((RoghanAP3/RoghanAP1)^RoghanShare)*
                                    ((MorghAP3/MorghAP1)^MorghShare)*((TokhmemorghAP3/TokhmemorghAP1)^TokhmemorghShare))]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV2,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]

Total<-Total[,CV3:=GExpenditures*(1-((GhandAP3/GhandAP2)^GhandShare)*((ShekarAP3/ShekarAP2)^ShekarShare)*
                                    ((BerenjAP3/BerenjAP2)^BerenjShare)*((RoghanAP3/RoghanAP2)^RoghanShare)*
                                    ((MorghAP3/MorghAP2)^MorghShare)*((TokhmemorghAP3/TokhmemorghAP2)^TokhmemorghShare))]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV3,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]


Total[,weighted.mean(Size,Weight)]
Total[,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]
Total[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]

Total[,weighted.mean(TokhmemorghPrice,Weight,na.rm=TRUE)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(BerenjKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(RoghanKG,Weight)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(RoghanKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(MorghKG,Weight)]
Total[,weighted.mean(MorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(MorghKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(GhandKG,Weight)]
Total[,weighted.mean(GhandKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(GhandKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(ShekarKG,Weight)]
Total[,weighted.mean(ShekarKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(ShekarKG,Weight),by=.(Decile)][order(Decile)]

Total[,weighted.mean(TokhmemorghKG,Weight)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Region)][order(Region)]
Total[,weighted.mean(TokhmemorghKG,Weight),by=.(Decile)][order(Decile)]

#x<-Total[,.(Total_Exp_Month_Per_nondurable,Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
