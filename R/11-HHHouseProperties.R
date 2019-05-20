# 11-HHHouseProperties.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHouseProperties =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)


P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))


years <- Settings$startyear:Settings$endyear

for(year in setdiff(years,63:88)){    # TODO: Add the metadata for 63 to 88 in P2Cols
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  

  P2 <- rbind(Tables[[paste0("R",year,"P2")]],Tables[[paste0("U",year,"P2")]])
  nP2 <- names(P2)
  if(length(which(sapply(P2, is.character)))>0){
    P2c <- P2[,lapply(.SD,iconv,"WINDOWS-1252","UTF-8"), .SDcols=sapply(P2,is.character)] 
    P2nc <- P2[,!sapply(P2,is.character),with=FALSE]
    P2 <- cbind(P2c,P2nc)[,nP2,with=FALSE]
  }
  
  if(year==96){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[2:46]
    setnames(P2,names(a[ind]))
    
  }else if(year %in% 89:95){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[-1]
    setnames(P2,names(a[ind]))
  }
  

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
                      labels=c("BrickSteel_StoneSteel","Brickwood_Stonewood","CementBlocks",
                               "AllBrick_Stone","Allwood",
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
  
  P2[,hotwater :=factor(hotwater, levels=21:30, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  
  P2[,car := factor(car, levels = 0:1,
                    labels=c("False","True"))]
  
  P2[,motorcycle := factor(motorcycle, levels=0:1,
                    labels=c("False","True"))]
  
  P2[,bike := factor(bike, levels=0:1,
                           labels=c("False","True"))]

  P2[,radio := factor(radio, levels=0:1,
                           labels=c("False","True"))]
  
  P2[,cassette := factor(cassette, levels=0:1,
                           labels=c("False","True"))]
  
  P2[,tvbw := factor(tvbw, levels=0:1,
                         labels=c("False","True"))]
  
  P2[,tvcr := factor(tvcr, levels=0:1,
                         labels=c("False","True"))]
  
  P2[,vcr := factor(vcr, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,computer := factor(computer, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,cellphone := factor(cellphone, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,freezer := factor(freezer, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,refrigerator := factor(refrigerator, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,frez_refrig := factor(frez_refrig, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,oven := factor(oven, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,vacuum := factor(vacuum, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,washer := factor(washer, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,sewing := factor(sewing, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,fan := factor(fan, levels=0:1,
                       labels=c("False","True"))]
  
  P2[,cooler_water_movable := factor(cooler_water_movable,
                                     levels=0:1,
                       labels=c("False","True"))]
  
  P2[,cooler_gas_movable := factor(cooler_gas_movable, 
                                   levels=0:1,
                       labels=c("False","True"))]
  
  P2[,dishwasher := factor(dishwasher, levels=0:1,
                       labels=c("False","True"))]
  
  if(year %in% 91:96){
  P2[,Microwave := factor(Microwave, levels=0:1,
                           labels=c("False","True"))]
  }
  
  P2[,none := factor(none, levels=0:1,
                           labels=c("False","True"))]

  P2[,pipewater := factor(pipewater, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,electricity := factor(electricity, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,pipegas := factor(pipegas, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,phone := factor(phone, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,internet := factor(internet, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,bathroom := factor(bathroom, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,kitchen := factor(kitchen, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,cooler := factor(cooler, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,centralcooler := factor(centralcooler, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,centralheat := factor(centralheat, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,pakage := factor(pakage, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,cooler_gas := factor(cooler_gas, levels=0:1,
                       labels=c("False","True"))]
  
  P2[,ego := factor(ego, levels=0:1,
                       labels=c("False","True"))]
  
  if(year %in% 89:95){
    P2[,party_month := factor(party_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,party_year := factor(party_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,ceremony_month := factor(ceremony_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,ceremony_year := factor(ceremony_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,homerepaire_month := factor(homerepaire_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,homerepaire_year := factor(homerepaire_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,prtrip_month := factor(prtrip_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,prtrip_year := factor(prtrip_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,frtrip_month := factor(frtrip_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,frtrip_year := factor(frtrip_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,bastari_month := factor(bastari_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,bastari_year := factor(bastari_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,operation_month := factor(operation_month, levels=0:1,
                            labels=c("False","True"))]
    
    if(year %in% 91:95){
    P2[,operation_year := factor(operation_year, levels=0:2,
                            labels=c("False","none","True"))]
    }
    
    P2[,other_year := factor(other_year, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,noceremony := factor(noceremony, levels=0:1,
                            labels=c("False","True"))]
  }
  
  HHHouseProperties<-P2
  save(HHHouseProperties, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
