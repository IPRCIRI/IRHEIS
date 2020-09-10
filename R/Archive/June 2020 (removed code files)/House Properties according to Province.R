# House Properties according to Province.R
# Builds the House Properties according to Province
#
# Copyright © 2020:Arin Shahbazian
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

tenure <- data.table(Year=NA_integer_,Region=NA_integer_,ProvinceCode=NA_real_,
                           OwnLandandBuilding=NA_real_,
                     Apartment=NA_real_,
                     Rented=NA_real_,
                     Mortgage=NA_real_,
                     AgainstService=NA_real_,
                     Other=NA_real_,
                     Free=NA_real_)[0]

tenure2 <- data.table(Year=NA_integer_,Region=NA_integer_,ProvinceCode=NA_real_,
                     tenure=NA_character_,
                     Share=NA_real_)[0]


for (year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:", year, "\n"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  MD<-MD[Region=="Urban" ]
  MD<-merge(MD,HHHouseProperties[,.(HHID,tenure,room,area)],by="HHID")
  MD2<-MD[,.(.N,Pop=sum(HIndivNo*Weight*Size)),by=.(Region,ProvinceCode,tenure)]
  MD3<-MD[,.(PopT=sum(HIndivNo*Weight*Size)),by=.(Region,ProvinceCode)]
  MD2<-merge(MD2,MD3)
  MD2<-MD2[,Share:=Pop/PopT]
  MD2[,sum(Share),by=.(Region,ProvinceCode)]
  
  X2 <- MD2[,.(Region,ProvinceCode,tenure,Share)]  
  X2[,Year:=year]
  tenure2 <- rbind(tenure2,X2)
  
  
  X1 <- MD[,.(OwnLandandBuilding=weighted.mean(tenure=="OwnLandandBuilding",Weight),
              Apartment=weighted.mean(tenure=="Apartment",Weight),
              Rented=weighted.mean(tenure=="Rented",Weight),
              Mortgage=weighted.mean(tenure=="Mortgage",Weight),
              AgainstService=weighted.mean(tenure=="AgainstService",Weight),
              Free=weighted.mean(tenure=="Free",Weight),
              Other=weighted.mean(tenure=="Other",Weight)),by=.(Region,ProvinceCode)]
  X1[,Year:=year]
  
  tenure <- rbind(tenure,X1)
  
}

ggplot(tenure2, aes(fill=tenure, y=Share, x=Year)) + 
  geom_bar(position="dodge", stat="identity") + xlim(89, 98) +
  ggtitle("Types of House")

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
