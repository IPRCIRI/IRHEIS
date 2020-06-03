# 142-Durables Depreciation.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Durables Depreciation =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(spatstat)
library(scales)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
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
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(car=="True",Weight),
  #                            Buy=weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
  #                           Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
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
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(tvcr=="True",Weight),
  #                            Buy=weighted.mean(TV_Rangi_Irani>0 |
  #                                               TV_Rangi_Khareji>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(vcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(Video_Player>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(computer=="True",Weight)]
  HHHouseProperties[,weighted.mean(PC>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(cellphone=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mobile>0 ,Weight)]
  # X1 <- HHHouseProperties[,.(Use=weighted.mean(cellphone=="True",Weight),
  #                            Buy=weighted.mean(Mobile>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(cooler_gas=="True",Weight),
  #                             Buy=weighted.mean(Cooler_Gaz>0  ,Weight))]
  #  X1[,share:=Use/Buy]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(computer=="True",Weight),
  #                             Buy=weighted.mean(PC>0  ,Weight))]
  #  X1[,share:=Use/Buy]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  X1 <- HHHouseProperties[,.(Use=weighted.mean(car=="True",Weight),
                             Buy=weighted.mean(Tamirat_Asasi>0  ,Weight))]
  X1[,share:=Use/Buy]
  X1[,Year:=year]
  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(freezer=="True",Weight)]
  HHHouseProperties[,weighted.mean(frez_refrig=="True",Weight)]
  HHHouseProperties[,weighted.mean(freezer2>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(refrigerator=="True",Weight)]
  HHHouseProperties[,weighted.mean(Yakhchal>0 ,Weight)]
  
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(freezer=="True" | 
  #                                                refrigerator=="True" |
  #                                               frez_refrig=="True",Weight),
  #                          Buy=weighted.mean(freezer2>0 |
  #                                             Yakhchal>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(oven=="True",Weight)]
  # HHHouseProperties[,weighted.mean(Microwave=="True",Weight)]
  HHHouseProperties[,weighted.mean(OjaghGaz>0 ,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(oven=="True" ,Weight),
  #                            Buy=weighted.mean(OjaghGaz>0  ,Weight))]
  #  X1[,Year:=year]
  #  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(vacuum=="True",Weight)]
  HHHouseProperties[,weighted.mean(Jaroobarghi>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(washer=="True",Weight)]
  HHHouseProperties[,weighted.mean(dishwasher=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mashin_Lebasshooyi>0,Weight)]
  #  X1 <- HHHouseProperties[,.(Use=weighted.mean(washer=="True"  |
  #                                                dishwasher=="True",Weight),
  #                           Buy=weighted.mean(Mashin_Lebasshooyi>0  ,Weight))]
  #  X1[,Year:=year]
  # Dep <- rbind(Dep,X1)
  
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
  
  HHHouseProperties[cluster3==1,weighted.mean(Mobile/Total_Exp_Month,Weight,TotalDurable)]
  
  if (year<97){
    TotalDurable[,G13:=Durable_Exp]
    TotalDurable[,Durable_Exp:=NULL]
  }
  
  
  #  TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Decile,Weight)],by="HHID")
  #  s2 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
  # s2[,Year:=year]
  #Name <- rbind(Name,s2)
  
  TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Weight,Decile)],by="HHID")
  X1<-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                   .(HHID,Weight,G13,Auto2_rani,Auto1_Irani,
                     Gardanband_Gold,
                     Ghali_Mashini,freezer2,Lastik_Mashin,
                     Mobile,Tamirat_Asasi,Travel_Haj,Mobl,
                     Mashin_Lebasshooyi,TV_Rangi_Khareji)]
  X1<-X1[, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
  
  X1[,Year:=year]
  HighShare <- rbind(HighShare,X1)
  
  
  A1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight)]
  A2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Auto2_rani>0 | Auto1_Khareji>0 | 
                               Auto2_Khareji>0 | Auto1_Irani>0),weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani,Weight)]
  
  B1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]
  B2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (TV_Rangi_Irani>0 | TV_Rangi_Khareji>0),weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)]
  
  C1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(freezer2,Weight)]
  C2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (freezer2>0),weighted.mean(freezer2,Weight)]
  
  D1<-   HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                           weighted.mean(OjaghGaz,Weight)]
  D2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (OjaghGaz>0),weighted.mean(OjaghGaz,Weight)]
  
  E1<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                            weighted.mean(Mashin_Lebasshooyi,Weight)]
  E2<-    HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                              (Mashin_Lebasshooyi>0),weighted.mean(Mashin_Lebasshooyi,Weight)]
  
  F1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Mobile,Weight)]
  F2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Mobile>0),weighted.mean(Mobile,Weight)]
  
  G1<- HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                         weighted.mean(Cooler_Gaz,Weight)]
  G2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Cooler_Gaz>0),weighted.mean(Cooler_Gaz,Weight)]
  
  H1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(PC,Weight)]
  H2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (PC>0),weighted.mean(PC,Weight)]
  
  I1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Lastik_Mashin,Weight)]
  I2<- HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                           (Lastik_Mashin>0),weighted.mean(Lastik_Mashin,Weight)]
  
  J1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Motor_Machin,Weight)]
  J2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Motor_Machin>0),weighted.mean(Motor_Machin,Weight)]
  
  K1<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                          weighted.mean(Tamirat_Asasi,Weight)]
  K2<-  HHHouseProperties[as.numeric(Decile)>1 & as.numeric(Decile)<4 &
                            (Tamirat_Asasi>0),weighted.mean(Tamirat_Asasi,Weight)]
  
  Value<-HHHouseProperties[,.(HHID,Auto2_rani,Auto1_Khareji,Auto2_Khareji,Auto1_Irani,
                              TV_Rangi_Irani,TV_Rangi_Khareji,freezer2,OjaghGaz,
                              Mashin_Lebasshooyi,Mobile,Cooler_Gaz,PC,
                              Lastik_Mashin,Motor_Machin,Tamirat_Asasi)]
  
  save(Value, file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))
  
}

ggplot(M_Buyers)+
  geom_line(mapping = aes(x=Year,y=Mobile_Buyers_Exp,col=factor(cluster3)))


HHHouseProperties[,weighted.mean((Auto1_Irani+Auto2_rani+ 
                                    Auto1_Khareji+Auto2_Khareji+
                                    TV_Rangi_Irani+TV_Rangi_Khareji+
                                    Mobile+Yakhchal)/G13,Weight,na.rm = TRUE)]

#HHHouseProperties[,weighted.mean(EducationTExp/G13,na.rm = TRUE)]
#######################################################
m<-HHHouseProperties[,.(.N,Share=weighted.mean(G13/Total_Exp_Month)),by=.(FinalPoor,Region)]
m<-m[,FinalPoor:=ifelse(FinalPoor==1,"Poors","NonPoors")]

ggplot(m, aes(fill=factor(Region), y=Share, x=factor(FinalPoor))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
########################################################

TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Weight,Decile)],by="HHID")
#z1 <-TotalDurable[, {lapply(.SD, function(x) sum(x*Weight)/sum(Weight))}][]
#z2 <-TotalDurable[, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]

#write.csv(z1,"z1.csv")
#write.csv(z2,"z2.csv")

#s1 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(Weight))}][]
#s2 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
#s2[,Year:=year]
#Name <- rbind(Name,s2)

#write.csv(s1,"s1.csv")
#write.csv(s2,"s2.csv")


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)