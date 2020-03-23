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

M_Buyers <- data.table(Year=NA_integer_,cluster3=NA_real_,Mobile_Buyers_Exp=NA_real_)[0]

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
  HHHouseProperties<-merge(HHHouseProperties,MD[,.(HHID,Decile,FinalPoor,Total_Exp_Month,cluster3)])
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
  

  
  MM_Buyers<-HHHouseProperties[Mobile>0,.(Mobile_Buyers=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  M_Holders<-HHHouseProperties[,.(Mobile_Holders=weighted.mean(cellphone=="True",Weight)),by=.(cluster3)]
  M_All<-HHHouseProperties[,.(All=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  
  Y <- HHHouseProperties[Mobile>0,.(Mobile_Buyers_Exp=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  Y[,Year:=year]
  M_Buyers <- rbind(M_Buyers,Y)
  
  M<-merge(MM_Buyers,M_Holders)
  M<-merge(M,M_All)
  
  HHHouseProperties[cluster3==1,weighted.mean(Mobile/Total_Exp_Month,Weight)]
}

ggplot(M_Buyers)+
  geom_line(mapping = aes(x=Year,y=Mobile_Buyers_Exp,col=factor(cluster3)))


HHHouseProperties[,weighted.mean((Auto1_Irani+Auto2_rani+ 
                                   Auto1_Khareji+Auto2_Khareji+
                                   TV_Rangi_Irani+TV_Rangi_Khareji+
                                   Mobile+Yakhchal)/G13,Weight,na.rm = TRUE)]

HHHouseProperties[,weighted.mean(EducationTExp/G13,na.rm = TRUE)]



for(G in c("ClothDurableExpenditure",
           "ClothDurable_WomanExp",
           "ClothDurable_WomanExp2",
           "Paltopoost",
           "Lebas_Aroos",
           "ClothDurable_OtherExp",
           "ClothDurable_OtherExp2",
           "Kolah_Imeni",
           "ClothDurable_RepairExp",
           "ClothDurable_RepairExp2",
           "Rent_Lebas",
           "ProtectHouseDurableExpenditure",
           "HouseRepairMaterialsExp",
           "HouseRepairMaterialsExp2",
           "Ajor",
           "Siman",
           "Mase",
           "Mozaik",
           "Seramik",
           "Dar_Panjere",
           "Shishe",
           "Rang",
           "Tiner",
           "Ghalam_Moo",
           "Kaghaz_Divari",
           "Lole_Ab",
           "Lavazem_Behdashti",
           "Toor",
           "Iranit",
           "Kah_Gel",
           "Other_Masaleh",
           "HouseRepairServicesExp",
           "HouseRepairServicesExp2",
           "Cooler_Repair",
           "Naghashi_Masaleh",
           "Naghashi_NoMasaleh",
           "KaghazDivari",
           "Nasb_Shishe",
           "Asfalt_Bam",
           "Tamirat_Shirvani",
           "Tamir_Narde",
           "Tamir_Chah",
           "Ojrat_Bana",
           "Ojrat_Kargar",
           "Tamirat_Detail",
           "Other_Negahdari",
           "FurnitureCarpetDurableExp",
           "FurnitureDurableExpenditure",
           "OtherFurnitureExp",
           "Cheragh_Electricy",
           "Parde",
           "Ayne",
           "Asar_Honari",
           "Sanaye_Dasti",
           "Gahvare",
           "Lavazem_Safar",
           "MavadTamir1",
           "HazineNasb",
           "FurnituregoodsExp",
           "Takhtekhab",
           "Komod",
           "Anvae_Miz",
           "Mobl",
           "MavadTamir2",
           "CarpetsExp",
           "CarpetsExp2",
           "Ghali_Dastbaf",
           "Ghali_Mashini",
           "Mooket",
           "Ziloo",
           "Kafpoosh_Nasb",
           "Repair_FurnitureExp",
           "Repair_FurnitureExp2",
           "Lavazem_TamirMobleman",
           "Hazine_TamirMobleman",
           "HouseDurableInstrumentExpenditure",
           "MainEquipmentsExp",
           "MainEquipmentsExp2",
           "Yakhchal",
           "freezer2",
           "Mashin_Lebasshooyi",
           "Otoo",
           "OjaghGaz",
           "KhorakPaz",
           "CilandreGaz",
           "Tahvie",
           "Bokhari",
           "Cooler_Gaz",
           "Jaroobarghi",
           "Charkh_Khayati",
           "Machine_bafandei",
           "Sandoogh",
           "Generator",
           "LavazemRepair3",
           "HazineNasb3",
           "MinorEquipmentsExp",
           "MinorEquipmentsExp2",
           "Polopaz",
           "ghahvejoosh",
           "Samavar",
           "Abmivergiri",
           "Panke",
           "Other_Barghi",
           "RepairRentEquipmentsExp",
           "RepairRentEquipmentsExp2",
           "LavazemRepair4",
           "hazine_Ejare",
           "UtensilDurableExpenditure",
           "FoodOtherUtensilExp",
           "Zoodpaz",
           "Samavar_Nafti",
           "Samavar_Gaz",
           "Keraye_Zoroof",
           "Other_Khanegi",
           "GardenInstruExpenditure",
           "DurableEquipmentsExp",
           "DurableEquipmentsExp2",
           "Abzar_Barghi",
           "Chamanzani",
           "TamiratAbzar",
           "HouseCommodityServDurableExpenditure",
           "HouseServicesRentExp",
           "HouseServicesRentExp2",
           "Keraye_Mobleman",
           "MedicalDurableExp",
           "MedicalInstruDurableExpenditure",
           "MedicalDurableEquipmentsExp",
           "MedicalDurableEquipments2",
           "Patoo_Barghi",
           "Capsule_O2",
           "MedicalStandingDurableExpenditure",
           "MedicalDurableServicesExp",
           "MedicalDurableServices2",
           "Visit_Jarahi_G",
           "Visit_Jarahi_NG",
           "Zibai_Jarahi_G",
           "Zibai_Jarahi_NG",
           "HospitalDurableExpenditure",
           "HospitalServicesExp",
           "HospitalServices2",
           "ShimiDarmani_G",
           "ShimiDarmani_NG",
           "Bastari_G",
           "Bastari_NG",
           "Azmayeshgah_G",
           "Azmayeshgah_NG",
           "Jarahi_G",
           "Jarahi_NG",
           "Visit_G",
           "Visit_NG",
           "Private_Nurse2",
           "TransportationDurableExp",
           "BuyingAutoDurableExpenditure",
           "BuyingAutomobileExp",
           "BuyingAutomobileExp2",
           "Auto1_Khareji",
           "Auto2_Khareji",
           "Auto2_rani",
           "Auto1_Irani",
           "BuyingMotorExp",
           "BuyingMotorExp2",
           "Motor",
           "BuyingBicycleExp",
           "BuyingBicycleExp2",
           "Bycycle",
           "Secharkhe",
           "Bycycle_Repair",
           "TransportInstruDurableExpenditure",
           "AccessoryExp",
           "AccessoryExp2",
           "Lastik_Mashin",
           "lastik_Motor",
           "Battery_Machin",
           "Motor_Machin",
           "Filter_Roghan",
           "KomakFanar",
           "Tolombe",
           "Chador_Mashin",
           "Zinatalat_Mashin",
           "Barband",
           "Dozdgir",
           "Separ",
           "Cheragh_Mashin",
           "Radiator_Mashin",
           "Lavazem_Yadak",
           "Doganesooz",
           "CarRepairServiceExp",
           "CarRepairServiceExp2",
           "Safkari",
           "Tamirat_Asasi",
           "Simkeshi_Mashin",
           "OtherTamir_Mashin",
           "OtherCar_DurableServicesExp",
           "OtherCar_DurableServicesExp2",
           "Akhz_Govahiname",
           "Talim_Ranandegi",
           "Moayene_Fani",
           "Keraye_Mashin",
           "Jarime_Mashin",
           "Taviz_Pelak",
           "Tarh_Traffic",
           "TelDurableEquipExp",
           "TelDurableEquipExp2",
           "TelDurableEquipExp3",
           "TelDurableEquipExp4",
           "Telephone_Sabet",
           "Fax",
           "Mobile",
           "Software",
           "VisualEquipDurableExpenditure",
           "AcousticProjective_EquipExp",
           "AcousticProjective_EquipExp2",
           "TV_SS",
           "Video_Player",
           "Anten",
           "Mahvare",
           "Zabtesot",
           "Hedphone",
           "Ghatayat_TV",
           "Tjhizat_Soti",
           "TV_Rangi_Irani",
           "TV_Rangi_Khareji",
           "CameraDurableEquipExp",
           "CameraDurableEquipExp2",
           "Doorbin_Digital",
           "Repair_Doorbin",
           "Instrument_Doorbin",
           "PCetcExp",
           "PCetcExp2",
           "PC",
           "Ertegha_System",
           "Printer",
           "Modem",
           "Software_PC",
           "Calculator",
           "Ghatayat_PC",
           "Other_Tajhozat_PC",
           "RepairAcousticProjectiveExp",
           "RepairAcousticProjectiveExp2",
           "Ojrat_Tamir_TV",
           "Ojrat_Tamir_PC",
           "Tamir_Other_Soti",
           "OtherinstruDurableExpenditure",
           "OtherAmusingDurableExp",
           "OtherAmusingDurableExp2",
           "Durable_for_Amusement",
           "MusicEquipDurableExp",
           "MusicEquipDurableExp2",
           "Musc_Instrument",
           "Biliard",
           "Repair_Durable_Amusement",
           "RepairAmusingDurableExp",
           "RepairAmusingDurableExp2",
           "Dampezeshk_Visit",
           "OtherAmusingDurableExpenditure",
           "GamesToysExp",
           "GamesToysExp2",
           "GamesToysExp3",
           "Game_Equip",
           "Repair_Game_Equip",
           "Mask",
           "Atishbazi",
           "Koleksion",
           "Software_Game",
           "Other_Amusement",
           "Sofre_Aghd",
           "Lavazem_Sofre_Aghd",
           "SportsEquipExp",
           "SportsEquipExp2",
           "BallSport",
           "Eski",
           "Kolah_Sport",
           "Dastkesh_Sport",
           "KiseKhab",
           "Tofang",
           "Ojagh_Pikniki",
           "Kafsh_Varzeshi",
           "Shamshirbazi",
           "Mahigiri",
           "Tolombe_Bad",
           "Varzeshi_Other",
           "Repair_Varzeshi",
           "PlantsGardenExp",
           "PlantsGardenExp2",
           "Artificial_Flower",
           "Artificial_Tree",
           "PetsExp",
           "PetsExp2",
           "Parande_Mahi",
           "Ghafas",
           "Aquarium",
           "Heyvan_Food",
           "Daroo_Heyvanat",
           "BookNewspaperDurableExpenditure",
           "BookNewspaperDurableData2",
           "Books_curriculumExp",
           "Books_curriculumExp2",
           "Book_Dabestan",
           "Book_Rahnamayi",
           "Book_Dabirestn",
           "Book_Uni",
           "Book_Komakdarsi",
           "Books_NoncurriculumExp",
           "Books_NoncurriculumExp2",
           "Book_Reference",
           "Book_Mazhabi",
           "TravelDurableExpenditure",
           "TravelsExp",
           "TravelsExp2",
           "TravelsExp3",
           "Travel_Mazhabi_Iran",
           "Travel_Tafrihi_Iran",
           "Travel_Haj",
           "Travel_Khareji",
           "EducationTExp",
           "PrimaryschoolRegExp",
           "PrimaryschoolRegExp2",
           "Enrollment_Dabestan_G",
           "Enrollment_Dabestan_NG",
           "Enrollment_SavadAmoozi",
           "Taghviati_Dabestan",
           "HelptoSchool",
           "HighschoolDurableExpenditure",
           "HighschoolRegExp",
           "HighschoolRegExp2",
           "Enrollment_Rahnamayi_G",
           "Enrollment_Rahnamayi_NG",
           "Enrollment_Rahnamayi_Shabane",
           "Taghviati_Rahnamayi",
           "Enrollment_Dabirestan_G",
           "Enrollment_Dabirestan_NG",
           "Enrollment_Dabirestan_Shabane",
           "Taghviati_Dabirestan",
           "HelptoDabirestan",
           "PreDurableExpenditure",
           "PreUniversityRegExp",
           "PreUniversityRegExp2",
           "Enrollment_pish_G",
           "Enrollment_pish_NG",
           "Enrollment_pish_Shabane",
           "Taghviati_pish",
           "KelasKonkoor",
           "Helptipish",
           "UniDurableExpenditure",
           "UniversityRegExp",
           "UniversityRegExp2",
           "Enrollment_Uni_G",
           "Enrollment_Elmikarbordi",
           "Enrollment_Azad_Uni",
           "Taghviati_Uni",
           "Jabejai_Daneshjoo",
           "OtherEduDurableExpenditure",
           "OtherClassRegExp",
           "OtherClassRegExp2",
           "EnrollmentZaban",
           "Education_Fanni",
           "Education_Ashbazi",
           "Education_Quran",
           "Education_Other",
           "OtherDurableExp",
           "PersonalOtherDurableExpenditure",
           "JewelryandWatchExp",
           "JewelryandWatchExp2",
           "Gardanband_Gold",
           "Gardanband_Silver",
           "Gardanband_Badal",
           "Saat_Divari",
           "Saat_Mochi",
           "Gire_Kraat",
           "Saat_Repait",
           "Sekke_Prize",
           "BurialExp",
           "BurialExp2",
           "Tadfin",
           "Aramgah",
           "SocialProtDurableExpenditure",
           "Social_ProtectsExp",
           "Social_ProtectsExp2",
           "Shirkhargah",
           "InsuranceDurableExpenditure",
           "Premium_NonMedicalExp",
           "Premium_NonMedicalExp2",
           "Premium_Omr",
           "Premium_Havades",
           "Premium_gheyredarmani_mostakhdem",
           "Premium_gheyredarmani_karfarma",
           "Premium_retirement_mostakhdem",
           "Premium_retirement_karfarma",
           "Premium_retirement_general",
           "Premium_retirement_Rural_Household",
           "Premium_retirement_Rural_Govern",
           "Premium_retirement_bank",
           "Premium_HouseExp",
           "Premium_HouseExp2",
           "Premium_manzel",
           "Premium_asas_manzel",
           "Premium_MedicalExp",
           "Premium_MedicalExp2",
           "Premium_Medical_Household",
           "Premium_Medical_Government",
           "Premium_Social_Mostakhdem",
           "Premium_Social_Karfarma",
           "Premium_Other",
           "Premium_Medical_General",
           "Premium_Medical_Rural",
           "Premium_TransportationExp",
           "Premium_TransportationExp2",
           "Premium_Naghlie",
           "Premium_Bar",
           "Premium_Shakhsesales",
           "Premium_OtherExp",
           "Premium_OtherExp2",
           "Premium_Other2",
           "OtherfinancialDurableExpenditure",
           "Financial_MediatorExp",
           "Financial_MediatorExp2",
           "Fee_Loan_Maskan",
           "Fee_Loan_NoMaskan",
           "Financial_OtherExp",
           "Financial_OtherExp2",
           "BankServices",
           "MoshavereServices",
           "OtherserviceDurableExpenditure",
           "Other_Nonsorted_ServicesExp",
           "Other_Nonsorted_ServicesExp2",
           "Hagholvekalat",
           "Other_Hoghooghi",
           "Karyabi",
           "Kafnodafn",
           "Dalali_Maskan",
           "Dalali_Mashin",
           "Avarez_Shahrdari",
           "Shenasname_Kartmelli",
           "Gozarname",
           "Daftarche_Bime",
           "Avareze_Khorooj",
           "Photocopy",
           "Sanad_Ezdevaj",
           "Parvane_Shekar",
           "Tablighat_GheyreShoghli",
           "Azmoon_Estekhdami",
           "Talebini",
           "Daroltarjome",
           "Khadamat_Motefarehje",
           "Dalali_Simcard",
           "TransitionsDurableExpenditure",
           "ReligiousExp",
           "ReligiousExp2",
           "Khoms",
           "Zokat",
           "Fetrie",
           "HelptoMasjed",
           "Madahi",
           "Sandoogh_Sadaghat",
           "Damage_AtonementExp",
           "Damage_AtonementExp2",
           "Die",
           "KhesaratbeAfrad",
           "Maliat_Maskan",
           "Pardakht_Enteghali",
           "Cash_Prize_G",
           "Cash_Prize_NG",
           "Zendan")){
  G<-as.name(G)
  cat(HHHouseProperties[,weighted.mean(G/G13,na.rm = TRUE)],"\n")
}

a<-lapply(HHHouseProperties, mean, na.rm = T)

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
