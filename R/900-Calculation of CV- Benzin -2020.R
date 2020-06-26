# Calculation of CV- Benzin
# Copyright © 2020: Arin Shahbazian
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
  MD<-MD[,.(HHID,HIndivNo,Region,ProvinceCode,NewArea_Name,Total_Exp_Month,Total_Exp_Month_nondurable,
            Size,EqSizeOECD,Weight,HSex,HEduLevel,
            HEduYears,HActivityState)]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  MD<-merge(MD,Deciles)
  
  save(MD, file = paste0(Settings$HEISProcessedPath,"Y",year,"MD.rda"))
}



MD[,weighted.mean(Total_Exp_Month,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[,weighted.mean(Size,Weight),by=.(Region,Decile)][order(Region,Decile)]
MD[NewArea_Name=="Sh_Tehran" ,weighted.mean(Total_Exp_Month,Weight),by=.(Decile)][order(Decile)]
MD[NewArea_Name=="Sh_Tehran" ,weighted.mean(Size,Weight),by=.(Decile)][order(Decile)]

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
load(file="Index98.rda")
Total<-merge(Total,Index98,by="ProvinceCode")
Total[,Decile:=as.numeric(Decile)]
Total<-Total[,Yaraneh2:=ifelse(Size==1,550000,
                              ifelse(Size==2,1030000,
                              ifelse(Size==3,1380000,
                              ifelse(Size==4,1720000,2050000))))]
Total<-Total[,Yaraneh:=ifelse(Decile>7,0,Yaraneh2)]

#Total<-Total[,Yaraneh:=Yaraneh2]
Total[is.na(Total)] <- 0
#Total<-Total[Benzin_Exp==0]


############New Prices###########
Total[,BenzinP1:=Benzin_Exp]
Total[,TransportationP1:=Transportation_Exp*Mehr98/137]
Total[,Other_ExpP1:=Other_Exp*Mehr98/137]

#Total[,BenzinP2:=ifelse(Benzin_Exp<600000, Benzin_Exp*1.5,Benzin_Exp*3)]
Total[,BenzinP2:=ifelse(Benzin_Exp<600000, Benzin_Exp*1.5,
                        600000*1.5+Benzin_Exp_Add*3)]
Total[,TransportationP2:=TransportationP1*1.11]
Total[,Other_ExpP2:=Other_ExpP1*1.02]

################################################
################  CV  #################
################################################

Total<-Total[,BenzinShare:=Benzin_Exp/Total_Exp_Month]
Total<-Total[,TransportationShare:=Transportation_Exp/Total_Exp_Month]
Total<-Total[,Other_ExpShare:=Other_Exp/Total_Exp_Month]


Total<-Total[,CV:=Total_Exp_Month*(1-((BenzinP2/BenzinP1)^BenzinShare)*((TransportationP2/TransportationP1)^TransportationShare)*
                                    ((Other_ExpP2/Other_ExpP1)^Other_ExpShare))]

Total<-Total[,Loss:=ifelse(Yaraneh<abs(CV),1,0)]
Total<-Total[Loss==1,Loss_Amount:=abs(CV)-Yaraneh]
Total[is.na(Total)] <- 0

#####Table 5
a<-Total[,weighted.mean(Benzin_Exp,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


#####Table 6
Total[,weighted.mean(CV,Weight,na.rm = TRUE)]
a<-Total[,weighted.mean(CV,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
b<-Total[,weighted.mean(Yaraneh,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


###Table 7
a<-Total[,weighted.mean(Loss,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
b<-Total[,sum(Loss*Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

a<-Total[,weighted.mean(Loss,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
b<-Total[,sum(Loss*Weight,na.rm = TRUE),by=.(Region)][order(Region)]

a<-Total[Decile<8,weighted.mean(Loss,Weight,na.rm = TRUE),by=.(Region)][order(Region)]
b<-Total[Decile<8,sum(Loss*Weight,na.rm = TRUE),by=.(Region)][order(Region)]


###Table 8
Total[,weighted.mean(Loss==1,Weight)]
Total[Decile<8,weighted.mean(Loss,Weight)]

Total[,sum(Loss*Weight)]
Total[Decile<8,sum(Loss*Weight)]

Total[,sum(Loss*Weight*Size)]
Total[Decile<8,sum(Loss*Weight*Size)]


###Table 9
Total[NewArea_Name=="Sh_Tehran",weighted.mean(Total_Exp_Month,Weight),by=.(Region,Decile)][order(Region,Decile)]

###Table 10
Total[Benzin_Exp>0,weighted.mean(Loss==1,Weight)]
Total[Benzin_Exp>0 & Decile<8,weighted.mean(Loss,Weight)]
Total[Benzin_Exp>0,sum(Loss*Weight)]
Total[Benzin_Exp>0 & Decile<8,sum(Loss*Weight)]
Total[Benzin_Exp>0,sum(Loss*Weight*Size)]
Total[Benzin_Exp>0 & Decile<8,sum(Loss*Weight*Size)]

Total[Benzin_Exp==0,weighted.mean(Loss==1,Weight)]
Total[Benzin_Exp==0 & Decile<8,weighted.mean(Loss,Weight)]
Total[Benzin_Exp==0,sum(Loss*Weight)]
Total[Benzin_Exp==0 & Decile<8,sum(Loss*Weight)]
Total[Benzin_Exp==0,sum(Loss*Weight*Size)]
Total[Benzin_Exp==0 & Decile<8,sum(Loss*Weight*Size)]


###Table11
taxi <- read_excel("C:/Users/pc1/Desktop/taxi.xlsx")
taxi<-as.data.table(taxi)
Total[,HHID:=as.character(HHID)]
taxi[,HHID:=as.character(HHID)]
Total<- merge(Total,taxi,by="HHID",all.x = TRUE)
for (col in c("taxicode"))  Total[is.na(get(col)), (col) := 0]

Total[Decile<8,sum(Loss*Weight),by=taxicode]
Total[Decile<8,sum(Weight),by=taxicode]


#Graph 1
a<-Total[,weighted.mean(Loss,Weight),by=.(ProvinceCode)]
b<-Total[Decile<8,weighted.mean(Loss,Weight),by=.(ProvinceCode)]

#Graph 2
Loss<-Total[Loss==1]
Loss[,weighted.mean(Loss_Amount,Weight,na.rm = TRUE)]
Loss[,weighted.mean(Loss_Amount,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

Loss[,weighted.mean(Loss_Amount/Total_Exp_Month,Weight,na.rm = TRUE)]
Loss[,weighted.mean(Loss_Amount/Total_Exp_Month,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Loss[,Loss_Share:=Loss_Amount/Total_Exp_Month]
Loss2<-Loss[Loss_Share<0.05]

Loss[,Loss_Section:=ifelse(Loss_Share<0.01,1,
                            ifelse(Loss_Share<0.02 & Loss_Share>0.01,2,
                            ifelse(Loss_Share<0.03 & Loss_Share>0.02,3,
                            ifelse(Loss_Share<0.04 & Loss_Share>0.03,4,
                            ifelse(Loss_Share<0.05 & Loss_Share>0.04,5,6)))))]

a<-Loss[,sum(Weight),by=Loss_Section][order(Loss_Section)]
b<-Loss[Decile<8,sum(Weight),by=Loss_Section][order(Loss_Section)]



### Graph 3
Win<-Total[Loss==0]
Win<-Win[,Win_Amount:=Yaraneh-abs(CV)]
Win[,weighted.mean(Win_Amount,Weight,na.rm = TRUE)]
Win[,weighted.mean(Win_Amount,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

Win[,weighted.mean(Win_Amount/Total_Exp_Month,Weight,na.rm = TRUE)]
Win[,weighted.mean(Win_Amount/Total_Exp_Month,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Win[,Win_Share:=Win_Amount/Total_Exp_Month]
Win2<-Win[Win_Share<0.3]

Win[,Win_Section:=ifelse(Win_Share<0.025,1,
                   ifelse(Win_Share<0.05 & Win_Share>0.025,2,
                   ifelse(Win_Share<0.075 & Win_Share>0.05,3,
                   ifelse(Win_Share<0.1 & Win_Share>0.075,4,
                   ifelse(Win_Share<0.125 & Win_Share>0.1,5,
                   ifelse(Win_Share<0.15 & Win_Share>0.125,6,
                   ifelse(Win_Share<0.175 & Win_Share>0.15,7,
                   ifelse(Win_Share<0.2 & Win_Share>0.175,8,
                   ifelse(Win_Share<0.225 & Win_Share>0.2,9,
                   ifelse(Win_Share<0.25 & Win_Share>0.225,10,
                   ifelse(Win_Share<0.275 & Win_Share>0.25,11,
                   ifelse(Win_Share<0.3 & Win_Share>0.275,12,13))))))))))))]

a<-Win[,sum(Weight),by=Win_Section][order(Win_Section)]
b<-Win[Decile<8,sum(Weight),by=Win_Section][order(Win_Section)]


### Graph 4
Win[,Pop:=sum(Weight),by=Decile]
a<-Win[,sum(Weight/Pop),by=.(Win_Section,Decile)]



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
