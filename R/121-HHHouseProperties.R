# 121-HHHouseProperties.R
# Builds the House Properties data.table for households
#
# Copyright © 2019-2020: Arin Shahbazian, Majid Einian
# Licence: GPL-3

rm(list=ls())

startTime <- proc.time()
cat("\n\n================ HHHouseProperties ================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)

P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))

for(year in Settings$startyear:Settings$endyear){    
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
  
  f <- function(x){as.numeric(str_trim(x))}
  P2 <- P2[, lapply(.SD, f)] 
  
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
  
  HHHouseProperties<-P2
  save(HHHouseProperties, file=paste0(Settings$HEISProcessedPath,"Y",year,
                                      "HHHouseProperties.rda"))
}

endTime <- proc.time()
cat("\n\n============================\nIt took",
    (endTime-startTime)[3], "seconds.")