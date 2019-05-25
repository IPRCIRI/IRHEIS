rm(list=ls())

library(data.table)

load("D:/HEIS/DataProcessed/Y96HHBase.rda")
load("D:/HEIS/DataProcessed/Y96HHI.rda")
load("D:/GitHub/IRHEIS/Data/SamplingWeights/HHWeights96.rda")

DT <- merge(HHBase[ProvinceCode==23],HHI,by="HHID")
DT <- merge(DT,HHWeights)

names(DT)

DT[,sum(NElementary*Weight)]
DT[,sum(NMiddle*Weight)]
DT[,sum((NHigh+NPre)*Weight)]

DT[,sum(Weight)]
DT[,sum(Size*Weight)]


DT[CountyCode==2301 & Region=="Urban", sum(NElementary*Weight)]
DT[CountyCode==2301 & Region=="Urban", sum(NMiddle*Weight)]
DT[CountyCode==2301 & Region=="Urban", sum((NHigh+NPre)*Weight)]

DT[CountyCode==2301 & Region=="Urban", sum(Weight)]
DT[CountyCode==2301 & Region=="Urban", sum(Size*Weight)]