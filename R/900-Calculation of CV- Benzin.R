# Calculation of CV- Benzin
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
################  Benzin  ########################
################################################
cat("\n\n================ Benzin =====================================\n")

BenzinTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Benzin))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- BenzinTables[Year==year]
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
  
  pcols <- intersect(names(TF),c("HHID","Code","Benzin_Exp"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,BenzinExpenditure:=as.numeric(BenzinExpenditure)]
  }

  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  BenzinData <- TF[,lapply(.SD,sum),by=HHID]
  save(BenzinData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Benzins.rda"))
}

################################################
################  Transportation  ########################
################################################
cat("\n\n================ Transportation =====================================\n")

TransportationTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Transportation))

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- TransportationTables[Year==year]
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
  
  pcols <- intersect(names(TF),c("HHID","Code","Transportation_Exp"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:96){
    TF[,TransportationExpenditure:=as.numeric(TransportationExpenditure)]
  }
  
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TransportationData <- TF[,lapply(.SD,sum),by=HHID]
  save(TransportationData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Transportations.rda"))
}
################################################
################  Other Exp  ########################
################################################
cat("\n\n================ Other =====================================\n")


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-MD[,.(HHID,Region,Total_Exp_Month,Total_Exp_Month_nondurable,
            Size,EqSizeRevOECD,Weight, Decile)]
  save(MD, file = paste0(Settings$HEISProcessedPath,"Y",year,"MD.rda"))
}


cat("\n\n================ Merge =====================================\n")
################################################
#######  Merge Information  #############
################################################

Total<-merge(MD,BenzinData,all = TRUE)
Total<-merge(Total,TransportationData,all = TRUE)
Total[is.na(Total)] <- 0
Total<-Total[,Other_Exp:=Total_Exp_Month-Benzin_Exp-Transportation_Exp]
Total<-Total[,Benzin_Exp_Add:=ifelse(Benzin_Exp<600000,0,Benzin_Exp-600000)]

Total<-Total[Decile %in% 1:10]

#Price Indexes
#load(file="Index_Dataset97.rda")
#Total<-merge(Total,Index_Dataset97,by=c("Region","Month"),all.x=TRUE)

Berenj_Esfand96<-114.4
Morgh_Esfand96<-122.1
Ghand_Esfand96<-108.4


Berenj_Esfand97<-145.6
Morgh_Esfand97<-265.6
Ghand_Esfand97<-170.3


############New Prices###########
Total[,BenzinP1:=Benzin_Exp]
Total[,TransportationP1:=Transportation_Exp]
Total[,Other_ExpP1:=Other_Exp]

#Total[,BenzinP2:=ifelse(Benzin_Exp<600000, Benzin_Exp*1.5,Benzin_Exp*3)]
Total[,BenzinP2:=ifelse(Benzin_Exp<600000, Benzin_Exp*1.5,
                        600000*1.5+Benzin_Exp_Add*3)]
Total[,TransportationP2:=Transportation_Exp*1.11]
Total[,Other_ExpP2:=Other_Exp*1.005]

################################################
################  CV  #################
################################################

Total<-Total[,BenzinShare:=Benzin_Exp/Total_Exp_Month]
Total<-Total[,TransportationShare:=Transportation_Exp/Total_Exp_Month]
Total<-Total[,Other_ExpShare:=Other_Exp/Total_Exp_Month]


Total<-Total[,CV:=Total_Exp_Month*(1-((BenzinP2/BenzinP1)^BenzinShare)*((TransportationP2/TransportationP1)^TransportationShare)*
                                    ((Other_ExpP2/Other_ExpP1)^Other_ExpShare))]

Total[,weighted.mean(CV,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
Total[,weighted.mean(CV,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
Total[,weighted.mean(CV,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
