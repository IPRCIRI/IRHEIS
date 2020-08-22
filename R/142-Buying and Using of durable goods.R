# 142-Buying and Using of durable goods.R
# Builds the House Properties data.table for households
#
# Copyright © 2020:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Buying and Using of durable goods =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(spatstat)
library(scales)

###########################################################
######Copy code files from TotalDurable folder######
#####################to Data Processed folder##############
#########################################################

Table<-data.table(Year=NA_integer_,Auto=NA_real_,Mobile=NA_real_,
                  Refrigerator=NA_real_,TV=NA_real_)[0]

Dep<-data.table(Year=NA_integer_, Buy=NA_real_,Use=NA_real_)[0]

years <- Settings$startyear:Settings$endyear

for(year in Settings$startyear:Settings$endyear){    
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHHouseProperties<-merge(HHHouseProperties,HHWeights)
  HHHouseProperties<-merge(HHHouseProperties,HHBase)
  HHHouseProperties<-merge(HHHouseProperties,TotalDurable)
  
  X <- HHHouseProperties[,.(Auto=weighted.mean(`71117`>0 | `71111`>0 | 
                                                 `71112`>0 | `71116`>0 ,Weight),
                            Mobile=weighted.mean(`82113`>0,Weight),
                            Refrigerator=weighted.mean(`53111`>0 | `53112`>0,Weight),
                            TV=weighted.mean(`91128`>0 |
                                               `91129`>0,Weight))]
  X[,Year:=year]
  Table <- rbind(Table,X)
  
  HHHouseProperties[,weighted.mean(car=="True",Weight)]
  HHHouseProperties[,weighted.mean(`71117`>0 | `71111`>0 | 
                                     `71112`>0 | `71116`>0 ,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(car=="True",Weight),
  #                            Buy=weighted.mean(`71117`>0 | `71111`>0 | 
  #                           `71112`>0 | `71116`>0 ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
 # HHHouseProperties[,weighted.mean(motorcycle=="True",Weight)]
#  HHHouseProperties[,weighted.mean(`71211`>0 ,Weight)]
  
  
 # HHHouseProperties[,weighted.mean(bike=="True",Weight)]
  #HHHouseProperties[,weighted.mean(`71311`>0 ,Weight)]
  
  #HHHouseProperties[,weighted.mean(radio=="True",Weight)]
  #HHHouseProperties[,weighted.mean(cassette=="True",Weight)]
  #HHHouseProperties[,weighted.mean(Zabtesot>0 ,Weight)]
  
  
 # HHHouseProperties[,weighted.mean(tvbw=="True",Weight)]
  #HHHouseProperties[,weighted.mean(TV_SS>0 ,Weight)]
  
  
  
  HHHouseProperties[,weighted.mean(tvcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(`91128`>0 |
                                     `91129`>0 ,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(tvcr=="True",Weight),
  #                            Buy=weighted.mean(`91128`>0 |
  #                                               `91129`>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  # HHHouseProperties[,weighted.mean(vcr=="True",Weight)]
  #HHHouseProperties[,weighted.mean(Video_Player>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(computer=="True",Weight)]
  HHHouseProperties[,weighted.mean(`91311`>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(cellphone=="True",Weight)]
  HHHouseProperties[,weighted.mean(`82113`>0 ,Weight)]
  X1 <- HHHouseProperties[,.(Use=weighted.mean(cellphone=="True",Weight),
                             Buy=weighted.mean(`82113`>0  ,Weight))]
  X1[,Year:=year]
  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(freezer=="True",Weight)]
  HHHouseProperties[,weighted.mean(frez_refrig=="True",Weight)]
  HHHouseProperties[,weighted.mean(`53112`>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(refrigerator=="True",Weight)]
  HHHouseProperties[,weighted.mean(`53111`>0 ,Weight)]
  
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(freezer=="True" | 
  #                                                refrigerator=="True" |
  #                                               frez_refrig=="True",Weight),
  #                          Buy=weighted.mean(`53112`>0 |
  #                                             `53111`>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(oven=="True",Weight)]
  # HHHouseProperties[,weighted.mean(Microwave=="True",Weight)]
  HHHouseProperties[,weighted.mean(`53116`>0 ,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(oven=="True" ,Weight),
  #                            Buy=weighted.mean(OjaghGaz>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  #HHHouseProperties[,weighted.mean(vacuum=="True",Weight)]
  #HHHouseProperties[,weighted.mean(Jaroobarghi>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(washer=="True",Weight)]
  HHHouseProperties[,weighted.mean(dishwasher=="True",Weight)]
  HHHouseProperties[,weighted.mean(`53113`>0,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(washer=="True"  |
  #                                                dishwasher=="True",Weight),
  #                           Buy=weighted.mean(Mashin_Lebasshooyi>0  ,Weight))]
  #  X1[,Year:=year]
  # Dep <- rbind(Dep,X1)
  
 # HHHouseProperties[,weighted.mean(sewing=="True",Weight)]
 # HHHouseProperties[,weighted.mean(Charkh_Khayati>0,Weight)]
  
  #HHHouseProperties[,weighted.mean(fan=="True",Weight)]
  #HHHouseProperties[,weighted.mean(cooler_water_movable=="True",Weight)]
  #HHHouseProperties[,weighted.mean(Panke>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(cooler_gas_movable=="True",Weight)]
  HHHouseProperties[,weighted.mean(`53125`>0,Weight)]
  
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
