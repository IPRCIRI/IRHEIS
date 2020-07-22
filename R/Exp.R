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

Exp <- data.table(Region=NA_character_,
                  Total_Exp_Month_Per=NA_real_,
                  G01=NA_real_,G011=NA_real_,
                  G0111=NA_real_,G01123=NA_real_,
                  G0112=NA_real_,G01131=NA_real_,
                  G0114=NA_real_,G0115=NA_real_,
                  G0116=NA_real_,G0117=NA_real_,
                  G0118=NA_real_,
                  G0119=NA_real_,G012=NA_real_,
                  G02=NA_real_,G00=NA_real_,
                  G03=NA_real_,G04=NA_real_,
                  G041=NA_real_,G042=NA_real_,
                  G044=NA_real_,G045=NA_real_,
                  G05=NA_real_,G06=NA_real_,
                  G07=NA_real_,G08=NA_real_,
                  G09=NA_real_,G100=NA_real_,
                  G11=NA_real_,G12=NA_real_,
                  GT=NA_real_,Year=NA_integer_)[0]


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total10.rda"))

  X<-Total[,.(Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
                G01=weighted.mean(G01,Weight),
                G011=weighted.mean(G011,Weight),
                G0111=weighted.mean(G0111,Weight),
                G01123=weighted.mean(G0112+G01131,Weight),
                G0112=weighted.mean(G0112,Weight),
                G01131=weighted.mean(G01131,Weight),
                G0114=weighted.mean(G0114,Weight),
                G0115=weighted.mean(G0115,Weight),
                G0116=weighted.mean(G0116,Weight),
                G0117=weighted.mean(G0117,Weight),
                G0118=weighted.mean(G0118,Weight),
                G0119=weighted.mean(G0119,Weight),
                G012=weighted.mean(G012,Weight),
                G02=weighted.mean(G02,Weight),
                G00=weighted.mean(Total_Exp_Month_Per-G01,Weight),
                G03=weighted.mean(G03,Weight),
                G04=weighted.mean(G04,Weight),
                G041=weighted.mean(G041,Weight),
                G042=weighted.mean(G042,Weight),
                G044=weighted.mean(G044,Weight),
                G045=weighted.mean(G045,Weight),
                G05=weighted.mean(G05,Weight),
                G06=weighted.mean(G06,Weight),
                G07=weighted.mean(G07,Weight),
                G08=weighted.mean(G08,Weight),
                G09=weighted.mean(G09,Weight),
                G100=weighted.mean(G101+G102+G103+G104+G105,Weight),
                G11=weighted.mean(G11,Weight),
                G12=weighted.mean(G12,Weight),
                GT=weighted.mean(Total_Exp_Month_Per-G041,Weight))
             ,by="Region"]
  
  X[,Year:=year]
  Exp <- rbind(Exp,X)
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")