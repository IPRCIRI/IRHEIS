# 168- Step 8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Exp =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

Exp <- data.table(#Region=NA_character_,
                  Year=NA_integer_,
                  Total_Exp_Month_Per=NA_real_,
                  G01=NA_real_,
                  G03=NA_real_,G041=NA_real_,G044=NA_real_,
                  G05=NA_real_,G06=NA_real_,
                  G07=NA_real_,G08=NA_real_,
                  G09=NA_real_,G10=NA_real_,
                  G11=NA_real_,G12=NA_real_,
                  G13=NA_real_,
                  Total_Exp_Month=NA_real_)[0]

share <- data.table(#Region=NA_character_,
                    Year=NA_integer_,
                  G01=NA_real_,
                  G03=NA_real_,G041=NA_real_,G044=NA_real_,
                  G05=NA_real_,G06=NA_real_,
                  G07=NA_real_,G08=NA_real_,
                  G09=NA_real_,G10=NA_real_,
                  G11=NA_real_,G12=NA_real_,
                  G13=NA_real_)[0]


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total10.rda"))
  Total[,Malek:=ifelse(tenure=="OwnLandandBuilding" | tenure=="Apartment" ,1,0)]
  Total[,G041:=ifelse(Malek==1 ,0,G041)]
  Total[,G042:=ifelse(Malek==1,0,G042)]
  
  X<-Total[,.(Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              G01=weighted.mean(G01+G02,Weight),
              G03=weighted.mean(G03,Weight),
              G041=weighted.mean(G041+G042,Weight),
              G044=weighted.mean(G044+G045,Weight),
              G05=weighted.mean(G05,Weight),
              G06=weighted.mean(G06,Weight),
              G07=weighted.mean(G07,Weight),
              G08=weighted.mean(G08,Weight),
              G09=weighted.mean(G09,Weight),
              G10=weighted.mean(G101+G102+G103+G104+G105,Weight),
              G11=weighted.mean(G11,Weight),
              G12=weighted.mean(G12,Weight),
              G13=weighted.mean(G13,Weight),
              Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight))
           #,by="Region"
           ]
  
  Total[,All:=ifelse(Malek==1,G01+G02+G03+G044+G045+
                       G05+G06+G07+G08+G09+
          G101+G102+G103+G104+G105+G11+G12+G13
          ,G01+G02+G03+G04+G05+G06+G07+G08+G09+
          G101+G102+G103+G104+G105+G11+G12+G13)]
  
  Y<-Total[,.(G01=weighted.mean((G01+G02)/All,Weight),
              G03=weighted.mean(G03/All,Weight),
              G041=weighted.mean((G041+G042)/All,Weight),
              G044=weighted.mean((G044+G045)/All,Weight),
              G05=weighted.mean(G05/All,Weight),
              G06=weighted.mean(G06/All,Weight),
              G07=weighted.mean(G07/All,Weight),
              G08=weighted.mean(G08/All,Weight),
              G09=weighted.mean(G09/All,Weight),
              G10=weighted.mean((G101+G102+G103+G104+G105)/All,Weight),
              G11=weighted.mean(G11/All,Weight),
              G12=weighted.mean(G12/All,Weight),
              G13=weighted.mean(G13/All,Weight))
           #,by="Region"
           ]
  
  X[,Year:=year]
  Exp <- rbind(Exp,X)
  
  Y[,Year:=year]
  share <- rbind(share,Y)
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")