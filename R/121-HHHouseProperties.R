# 121-HHHouseProperties.R
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
library(ggplot2)
library(spatstat)
library(scales)

P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))


Table<-data.table(Year=NA_integer_,Auto=NA_real_,Mobile=NA_real_,
                  Refrigerator=NA_real_,TV=NA_real_)[0]

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
  
  if(year>=96){
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
  
  if(year %in% 91:97){
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
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHHouseProperties<-merge(HHHouseProperties,HHWeights)
  HHHouseProperties<-merge(HHHouseProperties,HHBase)
  HHHouseProperties<-merge(HHHouseProperties,MD[,.(HHID,Decile,FinalPoor)])
  HHHouseProperties<-merge(HHHouseProperties,TotalDurable)
  
  X <- HHHouseProperties[,.(Auto=weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
                                  Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight),
                          Mobile=weighted.mean(Mobile>0,Weight),
             Refrigerator=weighted.mean(Yakhchal>0 | freezer2>0,Weight),
             TV=weighted.mean(TV_Rangi_Irani>0 |
                                TV_Rangi_Khareji>0,Weight))]
  X[,Year:=year]
  Table <- rbind(Table,X)
  
  HHHouseProperties[,weighted.mean(car=="True",Weight)]
  HHHouseProperties[,weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
                                     Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(motorcycle=="True",Weight)]
  HHHouseProperties[,weighted.mean(Motor>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(bike=="True",Weight)]
  HHHouseProperties[,weighted.mean(Bycycle>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(radio=="True",Weight)]
  HHHouseProperties[,weighted.mean(cassette=="True",Weight)]
  HHHouseProperties[,weighted.mean(Zabtesot>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(tvbw=="True",Weight)]
  HHHouseProperties[,weighted.mean(TV_SS>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(tvcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(TV_Rangi_Irani>0 |
                                     TV_Rangi_Khareji>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(vcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(Video_Player>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(computer=="True",Weight)]
  HHHouseProperties[,weighted.mean(PC>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(cellphone=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mobile>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(freezer=="True",Weight)]
  HHHouseProperties[,weighted.mean(frez_refrig=="True",Weight)]
  HHHouseProperties[,weighted.mean(freezer2>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(refrigerator=="True",Weight)]
  HHHouseProperties[,weighted.mean(Yakhchal>0 ,Weight)]
  

  HHHouseProperties[,weighted.mean(oven=="True",Weight)]
 # HHHouseProperties[,weighted.mean(Microwave=="True",Weight)]
  HHHouseProperties[,weighted.mean(OjaghGaz>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(vacuum=="True",Weight)]
  HHHouseProperties[,weighted.mean(Jaroobarghi>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(washer=="True",Weight)]
  HHHouseProperties[,weighted.mean(dishwasher=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mashin_Lebasshooyi>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(sewing=="True",Weight)]
  HHHouseProperties[,weighted.mean(Charkh_Khayati>0,Weight)]
  
  HHHouseProperties[,weighted.mean(fan=="True",Weight)]
  HHHouseProperties[,weighted.mean(cooler_water_movable=="True",Weight)]
  HHHouseProperties[,weighted.mean(Panke>0 ,Weight)]
  
   HHHouseProperties[,weighted.mean(cooler_gas_movable=="True",Weight)]
  HHHouseProperties[,weighted.mean(Cooler_Gaz>0,Weight)]
  
  load(file = "durable.rda")
  ggplot(durable)+
    geom_line(mapping = aes(x=Year,y=Ratio,col=factor(Type))) + ylim(0,0.13)
  
  HHHouseProperties[Auto1_Khareji>0,weighted.mean(Auto1_Khareji,Weight),by=.(Region,ProvinceCode)]
  HHHouseProperties[Auto1_Khareji>0,weighted.median(Auto1_Khareji,Weight),by=.(Region,ProvinceCode)]
  
  #Auto
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                      Auto1_Khareji>0 | Auto2_Khareji>0,
                      .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Auto[,Ratio:=N/Number]

  
  ggplot(Auto, aes(fill=factor(Region), y=Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                              Auto1_Khareji>0 | Auto2_Khareji>0,
                            .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Auto[,Auto_Ratio:=N/Number]
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  

  #Mobile
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Mobile<-HHHouseProperties[Mobile>0,
                          .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Mobile[,Mobile_Ratio:=N/Number]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Mobile<-HHHouseProperties[Mobile>0,
                          .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Mobile[,Mobile_Ratio:=N/Number]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Yakhchal
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0,
                            .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Yakhchal[,Yakhchal_Ratio:=N/Number]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0,
                            .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Yakhchal[,Yakhchal_Ratio:=N/Number]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #TV
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                                TV_Rangi_Khareji>0,
                              .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  TV[,TV_Ratio:=N/Number]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |TV_Rangi_Khareji>0,
  .(.N,Number=mean(Number)),by=.(Region,Decile)]
  TV[,TV_Ratio:=N/Number]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)

  
  #########################Exp#############################
  #Auto
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                            Auto1_Khareji>0 | Auto2_Khareji>0,
                          .(.N,Auto_Exp=weighted.mean(Auto1_Irani+Auto2_rani+
                            Auto1_Khareji+Auto2_Khareji,Weight),
                            Auto_Exp2=weighted.median(Auto1_Irani+Auto2_rani+
                             Auto1_Khareji+Auto2_Khareji,Weight)),
                          by=.(Region,ProvinceName)]
  
  Auto<-Auto[,Mean_Median:=Auto_Exp-Auto_Exp2]

  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                            Auto1_Khareji>0 | Auto2_Khareji>0,
                          .(.N,Auto_Exp=weighted.mean(Auto1_Irani+Auto2_rani+
                           Auto1_Khareji+Auto2_Khareji,Weight),
                            Auto_Exp2=weighted.median(Auto1_Irani+Auto2_rani+
                           Auto1_Khareji+Auto2_Khareji,Weight)),
                          by=.(Region,Decile)]
  Auto<-Auto[,Mean_Median:=Auto_Exp-Auto_Exp2]
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Mobile
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Mobile<-HHHouseProperties[Mobile>0 ,
                            .(.N,Mobile_Exp=weighted.mean(Mobile,Weight),
                              Mobile_Exp2=weighted.median(Mobile,Weight)),
                          by=.(Region,ProvinceName)]
  
  Mobile<-Mobile[,Mean_Median:=Mobile_Exp-Mobile_Exp2]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Mobile<-HHHouseProperties[Mobile>0 ,
                          .(.N,Mobile_Exp=weighted.mean(Mobile,Weight),
                            Mobile_Exp2=weighted.median(Mobile,Weight)),
                          by=.(Region,Decile)]
  Mobile<-Mobile[,Mean_Median:=Mobile_Exp-Mobile_Exp2]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Yakhchal
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0 ,
                            .(.N,Yakhchal_Exp=weighted.mean(Yakhchal+freezer2,Weight),
                              Yakhchal_Exp2=weighted.median(Yakhchal+freezer2,Weight)),
                            by=.(Region,ProvinceName)]
  
  Yakhchal<-Yakhchal[,Mean_Median:=Yakhchal_Exp-Yakhchal_Exp2]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0 ,
                            .(.N,Yakhchal_Exp=weighted.mean(Yakhchal+freezer2,Weight),
                              Yakhchal_Exp2=weighted.median(Yakhchal+freezer2,Weight)),
                            by=.(Region,Decile)]
  Yakhchal<-Yakhchal[,Mean_Median:=Yakhchal_Exp-Yakhchal_Exp2]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)  
  
  #TV
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                          TV_Rangi_Khareji>0 ,
                              .(.N,TV_Exp=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight),
                                TV_Exp2=weighted.median(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),
                              by=.(Region,ProvinceName)]
  
  TV<-TV[,Mean_Median:=TV_Exp-TV_Exp2]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                          TV_Rangi_Khareji>0 ,
                              .(.N,TV_Exp=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight),
                                TV_Exp2=weighted.median(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),
                              by=.(Region,Decile)]
  TV<-TV[,Mean_Median:=TV_Exp-TV_Exp2]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)  
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
