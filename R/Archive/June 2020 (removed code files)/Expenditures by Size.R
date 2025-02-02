#Expenditures by Size.R
# 
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

ExpSize <- data.table(Year=NA_integer_,Size=NA_real_,Total_Exp_Month_nondurable=NA_real_,
                      OriginalFoodExpenditure=NA_real_,HouseandEnergy_Exp=NA_real_)[0]


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  MD<-MD[Size<6]
  MD<-MD[Region=="Urban"]
  MD<-MD[as.numeric(Decile)<3 & as.numeric(Decile)>1]
  
  X1 <- MD[,.(Total_Exp_Month_nondurable=weighted.mean(Total_Exp_Month_nondurable,Weight*Size),
              OriginalFoodExpenditure=weighted.mean(OriginalFoodExpenditure,Weight*Size),
              HouseandEnergy_Exp=weighted.mean(HouseandEnergy_Exp,Weight*Size)),by=Size]

   X1[,Year:=year]

  ExpSize <- rbind(ExpSize,X1)
  
}

ggplot(MD)+
  geom_line(mapping = aes(x=Year,y=OriginalFoodExpenditure/Total_Exp_Month,col=factor(Size)))

ggplot(MD, aes(fill=factor(Size), y=OriginalFoodExpenditure/Total_Exp_Month, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(ExpSize, aes(fill=factor(Size), y=OriginalFoodExpenditure, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(ExpSize, aes(fill=factor(Size), y=HouseandEnergy_Exp, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)