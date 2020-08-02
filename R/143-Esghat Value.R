#143-Esghat Value
# 
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Esghat Value =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

Exp <- data.table(Year=NA_integer_,yakhchal_Exp=NA_real_,yakhchal_Sale=NA_real_,
                  Mobile_Exp=NA_real_,Mobile_Sale=NA_real_,
                  lebasshooyi_Exp=NA_real_,lebasshooyi_Sale=NA_real_,
                  ojaghgaz_Exp=NA_real_,ojaghgaz_Sale=NA_real_,
                  TV_Exp=NA_real_,TV_Sale=NA_real_,
                  Coolergazi_Exp=NA_real_,Coolergazi_Sale=NA_real_,
                  lastik_Exp=NA_real_,lastik_Sale=NA_real_,
                  tamirat_Exp=NA_real_,
                  #tamirat_Sale=NA_real_,
                  Motormashin_Exp=NA_real_,
                  #Motormashin_Sale=NA_real_,
                  Auto_Exp=NA_real_,Auto_Sale=NA_real_,
                  PC_Exp=NA_real_,PC_Sale=NA_real_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"DurableDataCodes.rda"))
  
  DurableDataCodes[is.na(DurableDataCodes)] <- 0
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  DurableDataCodes<-merge(DurableDataCodes,HHWeights)
  
yakhchal<-DurableDataCodes[Code==53111 | Code==53112]
Mobile<-DurableDataCodes[Code==82113]
lebasshooyi<-DurableDataCodes[Code==53113]
ojaghgaz<-DurableDataCodes[Code==53116]
TV<-DurableDataCodes[Code==91128 | Code==91129]
Coolergazi<-DurableDataCodes[Code==53125]
lastik<-DurableDataCodes[Code==72111]
tamirat<-DurableDataCodes[Code==72319]
Motormashin<-DurableDataCodes[Code==72118]
Auto<-DurableDataCodes[Code==71111 | Code==71112 | Code==71116 | Code==71117]
PC<-DurableDataCodes[Code==91311]
Auto<-DurableDataCodes[Code==71117]


X1<-yakhchal[Durable_Exp>0,.(yakhchal_Exp=weighted.mean(Durable_Exp,Weight))]
X2<-yakhchal[Durable_Sale>0,.(yakhchal_Sale=weighted.mean(Durable_Sale,Weight))]

X3<-Mobile[Durable_Exp>0,.(Mobile_Exp=weighted.mean(Durable_Exp,Weight))]
X4<-Mobile[Durable_Sale>0,.(Mobile_Sale=weighted.mean(Durable_Sale,Weight))]

X5<-lebasshooyi[Durable_Exp>0,.(lebasshooyi_Exp=weighted.mean(Durable_Exp,Weight))]
X6<-lebasshooyi[Durable_Sale>0,.(lebasshooyi_Sale=weighted.mean(Durable_Sale,Weight))]

X7<-ojaghgaz[Durable_Exp>0,.(ojaghgaz_Exp=weighted.mean(Durable_Exp,Weight))]
X8<-ojaghgaz[Durable_Sale>0,.(ojaghgaz_Sale=weighted.mean(Durable_Sale,Weight))]

X9<-TV[Durable_Exp>0,.(TV_Exp=weighted.mean(Durable_Exp,Weight))]
X10<-TV[Durable_Sale>0,.(TV_Sale=weighted.mean(Durable_Sale,Weight))]

X11<-tamirat[Durable_Exp>0,.(tamirat_Exp=weighted.mean(Durable_Exp,Weight))]
X12<-tamirat[Durable_Sale>0,.(tamirat_Sale=weighted.mean(Durable_Sale,Weight))]

X13<-lastik[Durable_Exp>0,.(lastik_Exp=weighted.mean(Durable_Exp,Weight))]
X14<-lastik[Durable_Sale>0,.(lastik_Sale=weighted.mean(Durable_Sale,Weight))]

X15<-Motormashin[Durable_Exp>0,.(Motormashin_Exp=weighted.mean(Durable_Exp,Weight))]
X16<-Motormashin[Durable_Sale>0,.(Motormashin_Sale=weighted.mean(Durable_Sale,Weight))]

X17<-Coolergazi[Durable_Exp>0,.(Coolergazi_Exp=weighted.mean(Durable_Exp,Weight))]
X18<-Coolergazi[Durable_Sale>0,.(Coolergazi_Sale=weighted.mean(Durable_Sale,Weight))]

X19<-Auto[Durable_Exp>0,.(Auto_Exp=weighted.mean(Durable_Exp,Weight))]
X20<-Auto[Durable_Sale>0,.(Auto_Sale=weighted.mean(Durable_Sale,Weight))]

X21<-PC[Durable_Exp>0,.(PC_Exp=weighted.mean(Durable_Exp,Weight))]
X22<-PC[Durable_Sale>0,.(PC_Sale=weighted.mean(Durable_Sale,Weight))]

X1[,Year:=year]
X2[,Year:=year]
X3[,Year:=year]
X4[,Year:=year]
X5[,Year:=year]
X6[,Year:=year]
X7[,Year:=year]
X8[,Year:=year]
X9[,Year:=year]
X10[,Year:=year]
X11[,Year:=year]
X12[,Year:=year]
X13[,Year:=year]
X14[,Year:=year]
X15[,Year:=year]
X16[,Year:=year]
X17[,Year:=year]
X18[,Year:=year]
X19[,Year:=year]
X20[,Year:=year]
X21[,Year:=year]
X22[,Year:=year]

X <- merge(X1,X2,by=c("Year"))
X <- merge(X,X3,by=c("Year"))
X <- merge(X,X4,by=c("Year"))
X <- merge(X,X5,by=c("Year"))
X <- merge(X,X6,by=c("Year"))
X <- merge(X,X7,by=c("Year"))
X <- merge(X,X8,by=c("Year"))
X <- merge(X,X9,by=c("Year"))
X <- merge(X,X10,by=c("Year"))
X <- merge(X,X11,by=c("Year"))
#X <- merge(X,X12,by=c("Year"))
X <- merge(X,X13,by=c("Year"))
X <- merge(X,X14,by=c("Year"))
X <- merge(X,X15,by=c("Year"))
#X <- merge(X,X16,by=c("Year"))
X <- merge(X,X17,by=c("Year"))
X <- merge(X,X18,by=c("Year"))
X <- merge(X,X19,by=c("Year"))
X <- merge(X,X20,by=c("Year"))
X <- merge(X,X21,by=c("Year"))
X <- merge(X,X22,by=c("Year"))

Exp <- rbind(Exp,X)
Esghat_Value<-Exp

save(Esghat_Value, file=paste0(Settings$HEISProcessedPath,"Y",year,"Esghat_Value.rda"))

 
}

#Exp<-Exp[,.(Year,Auto_Exp,Auto_Sale)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)