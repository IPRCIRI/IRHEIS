#Poors properties.R
# 
# Copyright © 2018:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())
starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)
library(xlsx)
library(haven)

#Load Data
load(file=paste0(Settings$HEISProcessedPath,"CBN_Urban","95.rda"))
load(file=paste0(Settings$HEISProcessedPath,"CBN_Rural","95.rda"))
CBN95<-rbind(CBN_Urban,CBN_Rural)

#Add house properties
T95P2 <- read_dta("D:/R/T95P2.dta")
R95P2 <- read_dta("D:/R/R95P2.dta")
U95P2 <- read_dta("D:/R/U95P2.dta")

CBN95<-merge(CBN95,T95P2,by =c("HHID"),all.x=TRUE)
CBN_Urban<-merge(CBN_Urban,U95P2,by =c("HHID"),all.x=TRUE)
CBN_Rural<-merge(CBN_Rural,R95P2,by =c("HHID"),all.x=TRUE)

load(file=paste0(Settings$HEISProcessedPath,"CBNPoor_Urban","95.rda"))
load(file=paste0(Settings$HEISProcessedPath,"CBNPoor_Rural","95.rda"))
CBN_Poor95<-rbind(CBNPoor_Urban,CBNPoor_Rural)

CBN_Poor95<-merge(CBN_Poor95,T95P2,by =c("HHID"),all.x=TRUE)
CBNPoor_Urban<-merge(CBNPoor_Urban,U95P2,by =c("HHID"),all.x=TRUE)
CBNPoor_Rural<-merge(CBNPoor_Rural,R95P2,by =c("HHID"),all.x=TRUE)


CBN_NonPoor_Urban<-CBN_Urban[Poor11==0]
CBN_NonPoor_Rural<-CBN_Rural[Poor11==0]
CBN_NonPoor95<-rbind(CBN_NonPoor_Urban,CBN_NonPoor_Rural)

CBN95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_Poor95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBNPoor_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBNPoor_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor95[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor_Urban[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]
CBN_NonPoor_Rural[,ProvinceCode2:=as.integer(str_sub(HHIDs,2,3))]


##### Hoese Conditions #####
Area_Per<-CBN_NonPoor_Rural[,Area_Per1:=weighted.mean(Area/Dimension,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Area_Per<-Area_Per[,.(Area_Per1,ProvinceCode2,Weight)]
Area_Per1<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Area_Per1[,Weight:=NULL]

Area_Per<-CBNPoor_Rural[,Area_Per2:=weighted.mean(Area/Dimension,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Area_Per<-Area_Per[,.(Area_Per2,ProvinceCode2,Weight)]
Area_Per2<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Area_Per2[,Weight:=NULL]

Area_Per1<-merge(Area_Per1,Area_Per2,by =c("ProvinceCode2"),all.x=TRUE)
write.xlsx(Area_Per1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Area_Per1")

Area_Per<-CBN_Rural[,Area_Per:=weighted.mean(Area/Dimension,Weight),by=Poor11][order(Poor11)]
Area_Per<-Area_Per[,.(Area_Per,Poor11,Weight)]
Area_Per<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Area_Per[,Weight:=NULL]
write.xlsx(Area_Per, "D:/R/Poors_Rural_home.xlsx",sheetName = "Area_Per2",append = TRUE)

Rooms_Per<-CBN_NonPoor_Rural[,Rooms_Per1:=weighted.mean(Rooms/Dimension,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per1,ProvinceCode2,Weight)]
Rooms_Per1<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Rooms_Per1[,Weight:=NULL]

Rooms_Per<-CBNPoor_Rural[,Rooms_Per2:=weighted.mean(Rooms/Dimension,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per2,ProvinceCode2,Weight)]
Rooms_Per2<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Rooms_Per2[,Weight:=NULL]

Rooms_Per1<-merge(Rooms_Per1,Rooms_Per2,by =c("ProvinceCode2"),all.x=TRUE)
write.xlsx(Rooms_Per1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Rooms_Per1",append = TRUE)

Rooms_Per<-CBN_Rural[,Rooms_Per:=weighted.mean(Rooms/Dimension,Weight),by=Poor11][order(Poor11)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per,Poor11,Weight)]
Rooms_Per<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Rooms_Per[,Weight:=NULL]
write.xlsx(Rooms_Per, "D:/R/Poors_Rural_home.xlsx",sheetName = "Rooms_Per2",append = TRUE)

MetrPrice<-CBN_NonPoor_Rural[,MetrPrice1:=weighted.mean(MetrPrice,Weight),by=ProvinceCode2][order(ProvinceCode2)]
MetrPrice<-MetrPrice[,.(MetrPrice1,ProvinceCode2,Weight)]
MetrPrice1<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
MetrPrice1[,Weight:=NULL]

MetrPrice<-CBNPoor_Rural[,MetrPrice2:=weighted.mean(MetrPrice,Weight),by=ProvinceCode2][order(ProvinceCode2)]
MetrPrice<-MetrPrice[,.(MetrPrice2,ProvinceCode2,Weight)]
MetrPrice2<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
MetrPrice2[,Weight:=NULL]

MetrPrice1<-merge(MetrPrice1,MetrPrice2,by =c("ProvinceCode2"),all.x=TRUE)
write.xlsx(MetrPrice1, "D:/R/Poors_Rural_home.xlsx",sheetName = "MetrPrice1",append = TRUE)

MetrPrice<-CBN_Rural[,MetrPrice:=weighted.mean(MetrPrice,Weight),by=Poor11][order(Poor11)]
MetrPrice<-MetrPrice[,.(MetrPrice,Poor11,Weight)]
MetrPrice<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
MetrPrice[,Weight:=NULL]
write.xlsx(MetrPrice, "D:/R/Poors_Rural_home.xlsx",sheetName = "MetrPrice2",append = TRUE)

##### Electricity #####
Electricity<-CBN_NonPoor_Rural[,Electricity_Poors:=weighted.mean(Electricity,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Electricity<-Electricity[,.(Electricity_Poors,ProvinceCode2,Weight)]
Electricity1<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Electricity1[,Weight:=NULL]

Electricity<-CBNPoor_Rural[,Electricity:=weighted.mean(Electricity,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Electricity<-Electricity[,.(Electricity,ProvinceCode2,Weight)]
Electricity2<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Electricity1<-merge(Electricity1,Electricity2,by =c("ProvinceCode2"),all.x=TRUE)
Electricity1[,Weight:=NULL]
write.xlsx(Electricity1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Electricity1",append = TRUE)

Electricity<-CBN_Rural[,Electricity:=weighted.mean(Electricity,Weight),by=Poor11][order(Poor11)]
Electricity<-Electricity[,.(Electricity,Poor11,Weight)]
Electricity3<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Electricity3[,Weight:=NULL]
write.xlsx(Electricity3, "D:/R/Poors_Rural_home.xlsx",sheetName = "Electricity2",append = TRUE)

##### PipedWater #####
PipedWater<-CBN_NonPoor_Rural[,PipedWater_Poors:=weighted.mean(PipedWater,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PipedWater<-PipedWater[,.(PipedWater_Poors,ProvinceCode2,Weight)]
PipedWater1<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
PipedWater1[,Weight:=NULL]

PipedWater<-CBNPoor_Rural[,PipedWater:=weighted.mean(PipedWater,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PipedWater<-PipedWater[,.(PipedWater,ProvinceCode2,Weight)]
PipedWater2<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

PipedWater1<-merge(PipedWater1,PipedWater2,by =c("ProvinceCode2"),all.x=TRUE)
PipedWater1[,Weight:=NULL]
write.xlsx(PipedWater1, "D:/R/Poors_Rural_home.xlsx",sheetName = "PipedWater1",append = TRUE)

PipedWater<-CBN_Rural[,PipedWater:=weighted.mean(PipedWater,Weight),by=Poor11][order(Poor11)]
PipedWater<-PipedWater[,.(PipedWater,Poor11,Weight)]
PipedWater3<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
PipedWater3[,Weight:=NULL]
write.xlsx(PipedWater3, "D:/R/Poors_Rural_home.xlsx",sheetName = "PipedWater2",append = TRUE)


##### PipedGas #####
PipedGas<-CBN_NonPoor_Rural[,PipedGas_Poors:=weighted.mean(PipedGas,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PipedGas<-PipedGas[,.(PipedGas_Poors,ProvinceCode2,Weight)]
PipedGas1<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
PipedGas1[,Weight:=NULL]

PipedGas<-CBNPoor_Rural[,PipedGas:=weighted.mean(PipedGas,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PipedGas<-PipedGas[,.(PipedGas,ProvinceCode2,Weight)]
PipedGas2<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

PipedGas1<-merge(PipedGas1,PipedGas2,by =c("ProvinceCode2"),all.x=TRUE)
PipedGas1[,Weight:=NULL]
write.xlsx(PipedGas1, "D:/R/Poors_Rural_home.xlsx",sheetName = "PipedGas1",append = TRUE)

PipedGas<-CBN_Rural[,PipedGas:=weighted.mean(PipedGas,Weight),by=Poor11][order(Poor11)]
PipedGas<-PipedGas[,.(PipedGas,Poor11,Weight)]
PipedGas3<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
PipedGas3[,Weight:=NULL]
write.xlsx(PipedGas3, "D:/R/Poors_Rural_home.xlsx",sheetName = "PipedGas2",append = TRUE)

##### Telephone #####
Telephone<-CBN_NonPoor_Rural[,Telephone_Poors:=weighted.mean(Telephone,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Telephone<-Telephone[,.(Telephone_Poors,ProvinceCode2,Weight)]
Telephone1<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Telephone1[,Weight:=NULL]

Telephone<-CBNPoor_Rural[,Telephone:=weighted.mean(Telephone,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Telephone<-Telephone[,.(Telephone,ProvinceCode2,Weight)]
Telephone2<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Telephone1<-merge(Telephone1,Telephone2,by =c("ProvinceCode2"),all.x=TRUE)
Telephone1[,Weight:=NULL]
write.xlsx(Telephone1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Telephone1",append = TRUE)

Telephone<-CBN_Rural[,Telephone:=weighted.mean(Telephone,Weight),by=Poor11][order(Poor11)]
Telephone<-Telephone[,.(Telephone,Poor11,Weight)]
Telephone3<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Telephone3[,Weight:=NULL]
write.xlsx(Telephone3, "D:/R/Poors_Rural_home.xlsx",sheetName = "Telephone2",append = TRUE)

##### Internet #####
Internet<-CBN_NonPoor_Rural[,Internet_Poors:=weighted.mean(Internet,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Internet<-Internet[,.(Internet_Poors,ProvinceCode2,Weight)]
Internet1<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Internet1[,Weight:=NULL]

Internet<-CBNPoor_Rural[,Internet:=weighted.mean(Internet,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Internet<-Internet[,.(Internet,ProvinceCode2,Weight)]
Internet2<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Internet1<-merge(Internet1,Internet2,by =c("ProvinceCode2"),all.x=TRUE)
Internet1[,Weight:=NULL]
write.xlsx(Internet1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Internet1",append = TRUE)

Internet<-CBN_Rural[,Internet:=weighted.mean(Internet,Weight),by=Poor11][order(Poor11)]
Internet<-Internet[,.(Internet,Poor11,Weight)]
Internet3<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Internet3[,Weight:=NULL]
write.xlsx(Internet3, "D:/R/Poors_Rural_home.xlsx",sheetName = "Internet2",append = TRUE)

##### Car #####
Car<-CBN_NonPoor_Rural[,Car_Poors:=weighted.mean(Car,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Car<-Car[,.(Car_Poors,ProvinceCode2,Weight)]
Car1<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Car1[,Weight:=NULL]

Car<-CBNPoor_Rural[,Car:=weighted.mean(Car,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Car<-Car[,.(Car,ProvinceCode2,Weight)]
Car2<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Car1<-merge(Car1,Car2,by =c("ProvinceCode2"),all.x=TRUE)
Car1[,Weight:=NULL]
write.xlsx(Car1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Car1",append = TRUE)

Car<-CBN_Rural[,Car:=weighted.mean(Car,Weight),by=Poor11][order(Poor11)]
Car<-Car[,.(Car,Poor11,Weight)]
Car3<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Car3[,Weight:=NULL]
write.xlsx(Car3, "D:/R/Poors_Rural_home.xlsx",sheetName = "Car2",append = TRUE)

##### PC #####
PC<-CBN_NonPoor_Rural[,PC_Poors:=weighted.mean(PC,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PC<-PC[,.(PC_Poors,ProvinceCode2,Weight)]
PC1<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
PC1[,Weight:=NULL]

PC<-CBNPoor_Rural[,PC:=weighted.mean(PC,Weight),by=ProvinceCode2][order(ProvinceCode2)]
PC<-PC[,.(PC,ProvinceCode2,Weight)]
PC2<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

PC1<-merge(PC1,PC2,by =c("ProvinceCode2"),all.x=TRUE)
PC1[,Weight:=NULL]
write.xlsx(PC1, "D:/R/Poors_Rural_home.xlsx",sheetName = "PC1",append = TRUE)

PC<-CBN_Rural[,PC:=weighted.mean(PC,Weight),by=Poor11][order(Poor11)]
PC<-PC[,.(PC,Poor11,Weight)]
PC3<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
PC3[,Weight:=NULL]
write.xlsx(PC3, "D:/R/Poors_Rural_home.xlsx",sheetName = "PC2",append = TRUE)

##### Cell #####
Cell<-CBN_NonPoor_Rural[,Cell_Poors:=weighted.mean(Cell,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Cell<-Cell[,.(Cell_Poors,ProvinceCode2,Weight)]
Cell1<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]
Cell1[,Weight:=NULL]

Cell<-CBNPoor_Rural[,Cell:=weighted.mean(Cell,Weight),by=ProvinceCode2][order(ProvinceCode2)]
Cell<-Cell[,.(Cell,ProvinceCode2,Weight)]
Cell2<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(ProvinceCode2)]

Cell1<-merge(Cell1,Cell2,by =c("ProvinceCode2"),all.x=TRUE)
Cell1[,Weight:=NULL]
write.xlsx(Cell1, "D:/R/Poors_Rural_home.xlsx",sheetName = "Cell1",append = TRUE)

Cell<-CBN_Rural[,Cell:=weighted.mean(Cell,Weight),by=Poor11][order(Poor11)]
Cell<-Cell[,.(Cell,Poor11,Weight)]
Cell3<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Poor11)]
Cell3[,Weight:=NULL]
write.xlsx(Cell3, "D:/R/Poors_Rural_home.xlsx",sheetName = "Cell2",append = TRUE)


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
