rm(list=ls())
starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
year<-95
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
TD<-MD
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
TD<-merge(TD,MD[,.(HHID,FinalPoor)])


TD[FinalPoor==1,weighted.mean(FoodExpenditure/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Cigar_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Cloth_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Energy_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(ServiceExp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Behdasht_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Transportation_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Furniture_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Communication_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Amusement_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(EducExpenditure/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Hotel_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Other_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Durable_Exp/Total_Exp_Month,Weight)]
TD[FinalPoor==1,weighted.mean(Medical_Exp/Total_Exp_Month,Weight)]




