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
Total[,TransportationP2:=TransportationP1*3]
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
Total[,weighted.mean(Benzin_Exp,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


#####Table 6
Total[,weighted.mean(CV,Weight,na.rm = TRUE)]
Total[,weighted.mean(CV,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,weighted.mean(Yaraneh,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


###Table 7
Total[,weighted.mean(Loss,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total[,sum(Loss*Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]


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
Total[,weighted.mean(Loss,Weight),by=.(ProvinceCode)]


#Graph 2


Total[,weighted.mean(Size),by=.(Region,Decile)][order(Region,Decile)]
Total[,sum(Weight),by=.(Region,Decile)][order(Region,Decile)]
Total[,sum(Weight*Size),by=.(Region,Decile)][order(Region,Decile)]

Total[,weighted.mean(Loss,Weight,na.rm = TRUE)]
Total[Decile<8,weighted.mean(Loss,Weight,na.rm = TRUE)]
Total[,weighted.mean(Loss,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

Total[,sum(Loss*Weight,na.rm = TRUE)]
Total[Decile<8,sum(Loss*Weight,na.rm = TRUE)]

Total[,sum(Loss*Weight*Size,na.rm = TRUE)]
Total[Decile<8,sum(Loss*Weight*Size,na.rm = TRUE)]

Total[,weighted.mean(Yaraneh,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Total<-Total[Decile<8]

#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TpubW.rda"))
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TprvW.rda"))
#load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TbussW.rda"))
#TpubW<-TpubW[,.(HHID,JobCode)]
#TprvW<-TprvW[,.(HHID,JobCode)]
#TbussW<-TbussW[,.(Address,DYCOL03)]
#names(TbussW)<-c("HHID","JCode")
#Total<-merge(Total,TpubW,by="HHID",all.x = TRUE)
#Total<-merge(Total,TprvW,by="HHID",all.x = TRUE)
#Total<-merge(Total,TbussW,by="HHID",all.x = TRUE)

taxi <- read_excel("C:/Users/pc1/Desktop/taxi.xlsx")
taxi<-as.data.table(taxi)
Total[,HHID:=as.character(HHID)]
taxi[,HHID:=as.character(HHID)]
Total<- merge(Total,taxi,by="HHID",all.x = TRUE)
for (col in c("taxicode"))  Total[is.na(get(col)), (col) := 0]

Loss<-Total[Loss==1]


Loss[,weighted.mean(Loss_Amount,Weight,na.rm = TRUE)]
Loss[,weighted.mean(Loss_Amount,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

Loss[,weighted.mean(Loss_Amount/Total_Exp_Month,Weight,na.rm = TRUE)]
Loss[,weighted.mean(Loss_Amount/Total_Exp_Month,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
Loss[,Loss_Share:=Loss_Amount/Total_Exp_Month]
Loss2<-Loss[Loss_Share<0.05]

Loss2[,Loss_Section:=ifelse(Loss_Share<0.01,1,
                   ifelse(Loss_Share<0.02 & Loss_Share>0.01,2,
                   ifelse(Loss_Share<0.03 & Loss_Share>0.02,3,
                   ifelse(Loss_Share<0.04 & Loss_Share>0.03,4,5))))]

Loss2[,sum(HIndivNo*Weight),by=Loss_Section][order(Loss_Section)]
Loss2[,sum(HIndivNo*Weight*Size),by=Loss_Section][order(Loss_Section)]
Loss2[,sum(HIndivNo),by=Loss_Section][order(Loss_Section)]


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
                  ifelse(Win_Share<0.275 & Win_Share>0.25,11,12)))))))))))]

Win[,sum(HIndivNo*Weight),by=Win_Section][order(Win_Section)]
Win[,sum(HIndivNo*Weight*Size),by=Win_Section][order(Win_Section)]
Win[,sum(HIndivNo),by=Win_Section][order(Win_Section)]
Win[,sum(HIndivNo*Weight),by=.(Win_Section,Decile)][order(Decile,Win_Section)]

plot(density(Win2$Win_Share),weights =Win2$Weight)
sm.density.compare(Win2$Win_Share, Win2$Decile)

Win[,weighted.mean(Total_Exp_Month,Weight,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]

plot(density(Loss2$Loss_Share),weights =Loss2$Weight)
#lines(density(Loss$Age),weights =Loss$Weight)
sm.density.compare(Loss$Loss_Amount, Loss$Decile)

Pop<-Loss[,sum(Weight)]
Loss[,sum(Weight),by=.(ProvinceCode)]
Loss[,sum(Weight),by=.(HSex)]
Loss[,sum(Weight),by=.(HEduLevel)][order(HEduLevel)]
Loss[,sum(Weight),by=.(Size)][order(Size)]
Loss[,sum(Weight),by=.(HActivityState)]
Loss[,sum(Weight),by=.(taxicode)]

Pop2<-Total[,sum(Weight)]
Total[,sum(Weight),by=.(ProvinceCode)]
Total[,sum(Weight),by=.(HSex)]
Total[,sum(Weight),by=.(HEduLevel)][order(HEduLevel)]
Total[,sum(Weight),by=.(Size)][order(Size)]
Total[,sum(Weight),by=.(HActivityState)]
Total[,sum(Weight),by=.(taxicode)]


Loss[,sum(Weight*Size),by=.(ProvinceCode)]
Loss[,sum(Weight*Size),by=.(HSex)]
Loss[,sum(Weight*Size),by=.(HEduLevel)][order(HEduLevel)]
Loss[,sum(Weight*Size),by=.(Size)]
Loss[,sum(Weight*Size),by=.(HActivityState)]
Loss[,sum(Weight*Size),by=.(taxicode)]

Loss[,sum(HIndivNo),by=.(ProvinceCode)]
Loss[,sum(HIndivNo),by=.(HSex)]
Loss[,sum(HIndivNo),by=.(HEduLevel)][order(HEduLevel)]
Loss[,sum(HIndivNo),by=.(Size)]
Loss[,sum(HIndivNo),by=.(HActivityState)]
Loss[,sum(HIndivNo),by=.(taxicode)]


LossR1<-Loss[Region=="Rural" & Decile==1,.(HHID,Size,Total_Exp_Month,CV,
                               Yaraneh,Loss_Amount)]

LossU1<-Loss[Region=="Urban" & Decile==1,.(HHID,Size,Total_Exp_Month,CV,
                               Yaraneh,Loss_Amount)]

#write.csv(Loss2,file = "loss2.csv")
#write.csv(Loss,file = "loss.csv")
#write.csv(Win,file = "Win.csv")
#write.csv(Win2,file = "Win2.csv")

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
