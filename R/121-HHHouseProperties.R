# 121-HHHouseProperties.R
# Builds the House Properties data.table for households
#
# Copyright © 2019-2022: Arin Shahbazian & Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ HHHouseProperties ================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

source("000-FunctionDefs.R")


P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))
year<-100
for(year in (Settings$startyear:Settings$endyear)){    
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  P2 <- rbind(Tables[[paste0("R",year,"P2")]],Tables[[paste0("U",year,"P2")]])
  nP2 <- names(P2)
  if(length(which(sapply(P2, is.character)))>0){
    P2c <- P2[,lapply(.SD,iconv,"WINDOWS-1252","UTF-8"), 
              .SDcols=sapply(P2,is.character)] 
    P2nc <- P2[,!sapply(P2,is.character),with=FALSE]
    P2 <- cbind(P2c,P2nc)[,nP2,with=FALSE]
  }
  
  a <- unlist(P2Cols[P2Cols$Year==year,])
  ind <- which(!is.na(a))[-1]
  setnames(P2,a[ind],names(a[ind]))
  
  P2 <- P2[, lapply(.SD, trim_to_number)] 
  
  P2[is.na(P2)]<-0  
  P2[,tenure :=factor(tenure, levels=1:7, 
                      labels=c("OwnLandandBuilding","Apartment","Rented",
                               "Mortgage","AgainstService",
                               "Free","Other"))]
  
  P2[,skeleton :=factor(skeleton, levels=1:3, 
                        labels=c("metal","concrete","other"))]
  
  P2[,constmat :=factor(constmat, levels=1:8, 
                        labels=c("BrickSteel_StoneSteel","Brickwood_Stonewood",
                                 "CementBlocks","AllBrick_Stone","Allwood",
                                 "SundriedBrickwood","SundriedBrickmud",
                                 "other"))]
  save(P2, file=paste0(Settings$HEISProcessedPath,"Y",year,
                                      "HHHouseProperties.rda"))
  P2[,cookfuel :=factor(cookfuel, levels=1:10, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  
  P2[,heatfuel :=factor(heatfuel, levels=11:20, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  if("hotwater" %in% names(P2)){
    P2[,hotwater :=factor(hotwater, levels=21:30, 
                          labels=c("karosine","gasoline",
                                   "gas","pipedgas",
                                   "electricity","woodandcharcoal",
                                   "Animalfuel","charcoal","otherFuel","None"))]
  }
  
  if (year==90){
    P2<-P2[,Phone:=ifelse(phone==4,TRUE,FALSE)]
    P2<-P2[,Bike:=ifelse(bike==1,TRUE,FALSE)]
    P2<-P2[,Radio:=ifelse(radio==1,TRUE,FALSE)]
    P2<-P2[,Cassette:=ifelse(cassette==1,TRUE,FALSE)]
    P2<-P2[,Tvbw:=ifelse(tvbw==1,TRUE,FALSE)]
    P2<-P2[,Tvcr:=ifelse(tvcr==1,TRUE,FALSE)]
    P2<-P2[,Vcr:=ifelse(vcr==1,TRUE,FALSE)]
    P2<-P2[,CellPhone:=ifelse(cellphone==1,TRUE,FALSE)]
    P2<-P2[,Freezer:=ifelse(freezer==1,TRUE,FALSE)]
    P2<-P2[,Refrigerator:=ifelse(refrigerator==1,TRUE,FALSE)]
    P2<-P2[,Frez_Refrig:=ifelse(frez_refrig==1,TRUE,FALSE)]
    P2<-P2[,Oven:=ifelse(oven==1,TRUE,FALSE)]
    P2<-P2[,Vacuum:=ifelse(vacuum==1,TRUE,FALSE)]
    P2<-P2[,Washer:=ifelse(washer==1,TRUE,FALSE)]
    P2<-P2[,Sewing:=ifelse(sewing==1,TRUE,FALSE)]
    P2<-P2[,Fan:=ifelse(fan==1,TRUE,FALSE)]
    P2<-P2[,Cooler_Water_Movable:=ifelse(cooler_water_movable==1,TRUE,FALSE)]
    P2<-P2[,Cooler_Gas_Movable:=ifelse(cooler_gas_movable==1,TRUE,FALSE)]
    P2<-P2[,Dishwasher:=ifelse(dishwasher==1,TRUE,FALSE)]
    P2<-P2[,Internet:=ifelse(internet==5,TRUE,FALSE)]
    P2<-P2[,Bathroom:=ifelse(bathroom==6,TRUE,FALSE)]
    P2<-P2[,None:=ifelse(none==1,TRUE,FALSE)]
    P2<-P2[,Pipewater:=ifelse(pipewater==1,TRUE,FALSE)]
    P2<-P2[,Electricity:=ifelse(electricity==2,TRUE,FALSE)]
    P2<-P2[,Pipegas:=ifelse(pipegas==3,TRUE,FALSE)]
    P2<-P2[,Kitchen:=ifelse(kitchen==7,TRUE,FALSE)]
    P2<-P2[,Cooler:=ifelse(cooler==8,TRUE,FALSE)]
    P2<-P2[,CentralCooler:=ifelse(centralcooler==9,TRUE,FALSE)]
    P2<-P2[,CentralHeat:=ifelse(centralheat==10,TRUE,FALSE)]
    P2<-P2[,Pakage:=ifelse(pakage==11,TRUE,FALSE)]
    P2<-P2[,Cooler_Gas:=ifelse(cooler_gas==12,TRUE,FALSE)]
    P2<-P2[,SewageNetwork:=ifelse(seweragenetwork==13,TRUE,FALSE)]
    P2<-P2[,Car:=ifelse(car==1,TRUE,FALSE)]
    P2<-P2[,Motorcycle:=ifelse(motorcycle==1,TRUE,FALSE)]
    
    P2<-P2[,phone:= Phone]
    P2<-P2[,bike:= Bike]
    P2<-P2[,motorcycle:=Motorcycle]
    P2<-P2[,radio:= Radio ]
    P2<-P2[,cassette:= Cassette ]
    P2<-P2[,tvbw:= Tvbw ]
    P2<-P2[,tvcr:= Tvcr ]
    P2<-P2[,vcr:= Vcr ]
    P2<-P2[,cellphone:= CellPhone ]
    P2<-P2[,freezer:= Freezer ]
    P2<-P2[,refrigerator:= Refrigerator ]
    P2<-P2[,frez_refrig:= Frez_Refrig ]
    P2<-P2[,oven:= Oven ]
    P2<-P2[,vacuum:= Vacuum ]
    P2<-P2[,washer:= Washer ]
    P2<-P2[,sewing:= Sewing ]
    P2<-P2[,fan:= Fan ]
    P2<-P2[,cooler_water_movable:= Cooler_Water_Movable ]
    P2<-P2[,cooler_gas_movable:= Cooler_Gas_Movable ]
    P2<-P2[,dishwasher:= Dishwasher ]
    P2<-P2[,internet:= Internet]
    P2<-P2[,bathroom:= Bathroom]
    P2<-P2[,none:= None ]
    P2<-P2[,pipewater:= Pipewater ]
    P2<-P2[,electricity:= Electricity]
    P2<-P2[,pipegas:= Pipegas]
    P2<-P2[,kitchen:= Kitchen]
    P2<-P2[,cooler:= Cooler]
    P2<-P2[,centralcooler:= CentralCooler]
    P2<-P2[,centralheat:= CentralHeat]
    P2<-P2[,pakage:= Pakage]
    P2<-P2[,cooler_gas:= Cooler_Gas]
    P2<-P2[,seweragenetwork:= SewageNetwork]
    }else{
  
  booleanvars <- c("car", "motorcycle", "bike", "radio", "cassette", "tvbw",
                   "tvcr", "vcr", "computer", "cellphone", "freezer",
                   "refrigerator", "frez_refrig", "oven", "vacuum", "washer", 
                   "sewing", "fan", "cooler_water_movable", 
                   "cooler_gas_movable", "dishwasher", "Microwave", "none",
                   "pipewater", "electricity", "pipegas", "phone", "internet",
                   "bathroom", "kitchen", "cooler", "centralcooler", 
                   "centralheat", "pakage", "cooler_gas", "seweragenetwork",
                   "party_month", "party_year", "ceremony_month",
                   "ceremony_year", "homerepaire_month", "homerepaire_year", 
                   "prtrip_month", "prtrip_year", "frtrip_month", "frtrip_year",
                   "bastari_month", "bastari_year", "operation_month",
                   "operation_year", "other_month", "other_year", "other_name", 
                   "noceremony", "noceremony_year")
  booleanvars <- intersect(booleanvars,names(P2))
  for (var in booleanvars) P2[, (var):= get(var)==1]
  }
  HHHouseProperties<-P2
  save(HHHouseProperties, file=paste0(Settings$HEISProcessedPath,"Y",year,
                                      "HHHouseProperties.rda"))
}

endTime <- proc.time()
cat("\n\n============================\nIt took",
    (endTime-startTime)[3], "seconds.")