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

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  #Load Data
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"NewFinalPoor.rda"))
  UNewFinalPoor<-NewFinalPoor[Region=="Urban"]
  RNewFinalPoor<-NewFinalPoor[Region=="Rural"]
  
  #Add house properties
  T95P2 <- read_dta("D:/R/T95P2.dta")
  R95P2 <- read_dta("D:/R/R95P2.dta")
  U95P2 <- read_dta("D:/R/U95P2.dta")
  
  CBN95<-merge(NewFinalPoor,T95P2,by =c("HHID"),all.x=TRUE)
  CBN_Urban<-merge(UNewFinalPoor,U95P2,by =c("HHID"),all.x=TRUE)
  CBN_Rural<-merge(RNewFinalPoor,R95P2,by =c("HHID"),all.x=TRUE)
  
  CBN_Poor95<-CBN95[FinalPoor==1]
  CBN_Poor_Urban<-CBN_Urban[FinalPoor==1]
  CBN_Poor_Rural<-CBN_Rural[FinalPoor==1]
  
  
  CBN_NonPoor_Urban<-CBN_Urban[FinalPoor==0]
  CBN_NonPoor_Rural<-CBN_Rural[FinalPoor==0]
  CBN_NonPoor95<-rbind(CBN_NonPoor_Urban,CBN_NonPoor_Rural)


##### Hoese Conditions #####
Area_Per<-CBN_NonPoor_Urban[,Area_Per1:=weighted.mean(Area/Size,Weight),by=NewArea][order(NewArea)]
Area_Per<-Area_Per[,.(Area_Per1,NewArea,Weight)]
Area_Per1<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Area_Per1[,Weight:=NULL]

Area_Per<-CBN_Poor_Urban[,Area_Per2:=weighted.mean(Area/Size,Weight),by=NewArea][order(NewArea)]
Area_Per<-Area_Per[,.(Area_Per2,NewArea,Weight)]
Area_Per2<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Area_Per2[,Weight:=NULL]

Area_Per1<-merge(Area_Per1,Area_Per2,by =c("NewArea"),all.x=TRUE)
write.xlsx(Area_Per1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Area_Per1")

Area_Per<-CBN_Urban[,Area_Per:=weighted.mean(Area/Size,Weight),by=FinalPoor][order(FinalPoor)]
Area_Per<-Area_Per[,.(Area_Per,FinalPoor,Weight)]
Area_Per<-Area_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Area_Per[,Weight:=NULL]
write.xlsx(Area_Per, "D:/R/Poors_Urban_home.xlsx",sheetName = "Area_Per2",append = TRUE)

Rooms_Per<-CBN_NonPoor_Urban[,Rooms_Per1:=weighted.mean(Rooms/Size,Weight),by=NewArea][order(NewArea)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per1,NewArea,Weight)]
Rooms_Per1<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Rooms_Per1[,Weight:=NULL]

Rooms_Per<-CBN_Poor_Urban[,Rooms_Per2:=weighted.mean(Rooms/Size,Weight),by=NewArea][order(NewArea)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per2,NewArea,Weight)]
Rooms_Per2<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Rooms_Per2[,Weight:=NULL]

Rooms_Per1<-merge(Rooms_Per1,Rooms_Per2,by =c("NewArea"),all.x=TRUE)
write.xlsx(Rooms_Per1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Rooms_Per1",append = TRUE)

Rooms_Per<-CBN_Urban[,Rooms_Per:=weighted.mean(Rooms/Size,Weight),by=FinalPoor][order(FinalPoor)]
Rooms_Per<-Rooms_Per[,.(Rooms_Per,FinalPoor,Weight)]
Rooms_Per<-Rooms_Per[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Rooms_Per[,Weight:=NULL]
write.xlsx(Rooms_Per, "D:/R/Poors_Urban_home.xlsx",sheetName = "Rooms_Per2",append = TRUE)

MetrPrice<-CBN_NonPoor_Urban[,MetrPrice1:=weighted.mean(MetrPrice,Weight),by=NewArea][order(NewArea)]
MetrPrice<-MetrPrice[,.(MetrPrice1,NewArea,Weight)]
MetrPrice1<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
MetrPrice1[,Weight:=NULL]

MetrPrice<-CBN_Poor_Urban[,MetrPrice2:=weighted.mean(MetrPrice,Weight),by=NewArea][order(NewArea)]
MetrPrice<-MetrPrice[,.(MetrPrice2,NewArea,Weight)]
MetrPrice2<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
MetrPrice2[,Weight:=NULL]

MetrPrice1<-merge(MetrPrice1,MetrPrice2,by =c("NewArea"),all.x=TRUE)
write.xlsx(MetrPrice1, "D:/R/Poors_Urban_home.xlsx",sheetName = "MetrPrice1",append = TRUE)

MetrPrice<-CBN_Urban[,MetrPrice:=weighted.mean(MetrPrice,Weight),by=FinalPoor][order(FinalPoor)]
MetrPrice<-MetrPrice[,.(MetrPrice,FinalPoor,Weight)]
MetrPrice<-MetrPrice[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
MetrPrice[,Weight:=NULL]
write.xlsx(MetrPrice, "D:/R/Poors_Urban_home.xlsx",sheetName = "MetrPrice2",append = TRUE)

##### Electricity #####
Electricity<-CBN_NonPoor_Urban[,Electricity_Poors:=weighted.mean(Electricity,Weight),by=NewArea][order(NewArea)]
Electricity<-Electricity[,.(Electricity_Poors,NewArea,Weight)]
Electricity1<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Electricity1[,Weight:=NULL]

Electricity<-CBN_Poor_Urban[,Electricity:=weighted.mean(Electricity,Weight),by=NewArea][order(NewArea)]
Electricity<-Electricity[,.(Electricity,NewArea,Weight)]
Electricity2<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Electricity1<-merge(Electricity1,Electricity2,by =c("NewArea"),all.x=TRUE)
Electricity1[,Weight:=NULL]
write.xlsx(Electricity1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Electricity1",append = TRUE)

Electricity<-CBN_Urban[,Electricity:=weighted.mean(Electricity,Weight),by=FinalPoor][order(FinalPoor)]
Electricity<-Electricity[,.(Electricity,FinalPoor,Weight)]
Electricity3<-Electricity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Electricity3[,Weight:=NULL]
write.xlsx(Electricity3, "D:/R/Poors_Urban_home.xlsx",sheetName = "Electricity2",append = TRUE)

##### PipedWater #####
PipedWater<-CBN_NonPoor_Urban[,PipedWater_Poors:=weighted.mean(PipedWater,Weight),by=NewArea][order(NewArea)]
PipedWater<-PipedWater[,.(PipedWater_Poors,NewArea,Weight)]
PipedWater1<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
PipedWater1[,Weight:=NULL]

PipedWater<-CBN_Poor_Urban[,PipedWater:=weighted.mean(PipedWater,Weight),by=NewArea][order(NewArea)]
PipedWater<-PipedWater[,.(PipedWater,NewArea,Weight)]
PipedWater2<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

PipedWater1<-merge(PipedWater1,PipedWater2,by =c("NewArea"),all.x=TRUE)
PipedWater1[,Weight:=NULL]
write.xlsx(PipedWater1, "D:/R/Poors_Urban_home.xlsx",sheetName = "PipedWater1",append = TRUE)

PipedWater<-CBN_Urban[,PipedWater:=weighted.mean(PipedWater,Weight),by=FinalPoor][order(FinalPoor)]
PipedWater<-PipedWater[,.(PipedWater,FinalPoor,Weight)]
PipedWater3<-PipedWater[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
PipedWater3[,Weight:=NULL]
write.xlsx(PipedWater3, "D:/R/Poors_Urban_home.xlsx",sheetName = "PipedWater2",append = TRUE)


##### PipedGas #####
PipedGas<-CBN_NonPoor_Urban[,PipedGas_Poors:=weighted.mean(PipedGas,Weight),by=NewArea][order(NewArea)]
PipedGas<-PipedGas[,.(PipedGas_Poors,NewArea,Weight)]
PipedGas1<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
PipedGas1[,Weight:=NULL]

PipedGas<-CBN_Poor_Urban[,PipedGas:=weighted.mean(PipedGas,Weight),by=NewArea][order(NewArea)]
PipedGas<-PipedGas[,.(PipedGas,NewArea,Weight)]
PipedGas2<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

PipedGas1<-merge(PipedGas1,PipedGas2,by =c("NewArea"),all.x=TRUE)
PipedGas1[,Weight:=NULL]
write.xlsx(PipedGas1, "D:/R/Poors_Urban_home.xlsx",sheetName = "PipedGas1",append = TRUE)

PipedGas<-CBN_Urban[,PipedGas:=weighted.mean(PipedGas,Weight),by=FinalPoor][order(FinalPoor)]
PipedGas<-PipedGas[,.(PipedGas,FinalPoor,Weight)]
PipedGas3<-PipedGas[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
PipedGas3[,Weight:=NULL]
write.xlsx(PipedGas3, "D:/R/Poors_Urban_home.xlsx",sheetName = "PipedGas2",append = TRUE)

##### Telephone #####
Telephone<-CBN_NonPoor_Urban[,Telephone_Poors:=weighted.mean(Telephone,Weight),by=NewArea][order(NewArea)]
Telephone<-Telephone[,.(Telephone_Poors,NewArea,Weight)]
Telephone1<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Telephone1[,Weight:=NULL]

Telephone<-CBN_Poor_Urban[,Telephone:=weighted.mean(Telephone,Weight),by=NewArea][order(NewArea)]
Telephone<-Telephone[,.(Telephone,NewArea,Weight)]
Telephone2<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Telephone1<-merge(Telephone1,Telephone2,by =c("NewArea"),all.x=TRUE)
Telephone1[,Weight:=NULL]
write.xlsx(Telephone1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Telephone1",append = TRUE)

Telephone<-CBN_Urban[,Telephone:=weighted.mean(Telephone,Weight),by=FinalPoor][order(FinalPoor)]
Telephone<-Telephone[,.(Telephone,FinalPoor,Weight)]
Telephone3<-Telephone[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Telephone3[,Weight:=NULL]
write.xlsx(Telephone3, "D:/R/Poors_Urban_home.xlsx",sheetName = "Telephone2",append = TRUE)

##### Internet #####
Internet<-CBN_NonPoor_Urban[,Internet_Poors:=weighted.mean(Internet,Weight),by=NewArea][order(NewArea)]
Internet<-Internet[,.(Internet_Poors,NewArea,Weight)]
Internet1<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Internet1[,Weight:=NULL]

Internet<-CBN_Poor_Urban[,Internet:=weighted.mean(Internet,Weight),by=NewArea][order(NewArea)]
Internet<-Internet[,.(Internet,NewArea,Weight)]
Internet2<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Internet1<-merge(Internet1,Internet2,by =c("NewArea"),all.x=TRUE)
Internet1[,Weight:=NULL]
write.xlsx(Internet1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Internet1",append = TRUE)

Internet<-CBN_Urban[,Internet:=weighted.mean(Internet,Weight),by=FinalPoor][order(FinalPoor)]
Internet<-Internet[,.(Internet,FinalPoor,Weight)]
Internet3<-Internet[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Internet3[,Weight:=NULL]
write.xlsx(Internet3, "D:/R/Poors_Urban_home.xlsx",sheetName = "Internet2",append = TRUE)

##### Car #####
Car<-CBN_NonPoor_Urban[,Car_Poors:=weighted.mean(Car,Weight),by=NewArea][order(NewArea)]
Car<-Car[,.(Car_Poors,NewArea,Weight)]
Car1<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Car1[,Weight:=NULL]

Car<-CBN_Poor_Urban[,Car:=weighted.mean(Car,Weight),by=NewArea][order(NewArea)]
Car<-Car[,.(Car,NewArea,Weight)]
Car2<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Car1<-merge(Car1,Car2,by =c("NewArea"),all.x=TRUE)
Car1[,Weight:=NULL]
write.xlsx(Car1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Car1",append = TRUE)

Car<-CBN_Urban[,Car:=weighted.mean(Car,Weight),by=FinalPoor][order(FinalPoor)]
Car<-Car[,.(Car,FinalPoor,Weight)]
Car3<-Car[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Car3[,Weight:=NULL]
write.xlsx(Car3, "D:/R/Poors_Urban_home.xlsx",sheetName = "Car2",append = TRUE)

##### PC #####
PC<-CBN_NonPoor_Urban[,PC_Poors:=weighted.mean(PC,Weight),by=NewArea][order(NewArea)]
PC<-PC[,.(PC_Poors,NewArea,Weight)]
PC1<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
PC1[,Weight:=NULL]

PC<-CBN_Poor_Urban[,PC:=weighted.mean(PC,Weight),by=NewArea][order(NewArea)]
PC<-PC[,.(PC,NewArea,Weight)]
PC2<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

PC1<-merge(PC1,PC2,by =c("NewArea"),all.x=TRUE)
PC1[,Weight:=NULL]
write.xlsx(PC1, "D:/R/Poors_Urban_home.xlsx",sheetName = "PC1",append = TRUE)

PC<-CBN_Urban[,PC:=weighted.mean(PC,Weight),by=FinalPoor][order(FinalPoor)]
PC<-PC[,.(PC,FinalPoor,Weight)]
PC3<-PC[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
PC3[,Weight:=NULL]
write.xlsx(PC3, "D:/R/Poors_Urban_home.xlsx",sheetName = "PC2",append = TRUE)

##### Cell #####
Cell<-CBN_NonPoor_Urban[,Cell_Poors:=weighted.mean(Cell,Weight),by=NewArea][order(NewArea)]
Cell<-Cell[,.(Cell_Poors,NewArea,Weight)]
Cell1<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
Cell1[,Weight:=NULL]

Cell<-CBN_Poor_Urban[,Cell:=weighted.mean(Cell,Weight),by=NewArea][order(NewArea)]
Cell<-Cell[,.(Cell,NewArea,Weight)]
Cell2<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

Cell1<-merge(Cell1,Cell2,by =c("NewArea"),all.x=TRUE)
Cell1[,Weight:=NULL]
write.xlsx(Cell1, "D:/R/Poors_Urban_home.xlsx",sheetName = "Cell1",append = TRUE)

Cell<-CBN_Urban[,Cell:=weighted.mean(Cell,Weight),by=FinalPoor][order(FinalPoor)]
Cell<-Cell[,.(Cell,FinalPoor,Weight)]
Cell3<-Cell[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
Cell3[,Weight:=NULL]
write.xlsx(Cell3, "D:/R/Poors_Urban_home.xlsx",sheetName = "Cell2",append = TRUE)

}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
