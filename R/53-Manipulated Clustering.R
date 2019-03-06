#53-Manipulated Clustering.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

cat(paste0("\n------------------------------\nYear:",Settings$baseyear,"\n"))
load(file=paste0(Settings$HEISProcessedPath,"Y",Settings$baseyear,"InitialPoor.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",Settings$baseyear,"BigFoodPrice.rda"))
MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all=TRUE)

#####Clustering#####
dt2Urban<-MD[Region=="Urban",.(NewArea,NewArea2,Region,HHID)]
dt2Rural<-MD[Region=="Rural",.(NewArea,NewArea2,Region,HHID)]

#####Urban#####
dt2Urban<-dt2Urban[NewArea2=="Sh_Tehran",cluster2:=1]
dt2Urban<-dt2Urban[NewArea2=="Sh_Shiraz" | NewArea2=="Sh_Esfahan" |
              NewArea2=="Sh_Bandarabas" | NewArea2=="Gilan" |
              NewArea2=="Sh_Karaj" | NewArea2=="Sh_Arak" |
              NewArea2=="Mazandaran"  ,
              cluster2:=2]
dt2Urban<-dt2Urban[NewArea2=="Ghom" | NewArea2=="Alborz" |
              NewArea2=="Sh_Kermanshah" | NewArea2=="Hamedan" |
              NewArea2=="Az_Sharghi" | NewArea2=="Sh_Tabriz" |
              NewArea2=="Tehran" | NewArea2=="Semnan" |
              NewArea2=="Sh_Urmia" | NewArea2=="Sh_Mashhad" |
              NewArea2=="Esfahan" | NewArea2=="Fars" |
              NewArea2=="Sh_Kerman" | NewArea2=="Ghazvin" ,
              cluster2:=3]
dt2Urban<-dt2Urban[NewArea2=="Zanjan" | NewArea2=="Golestan" |
               NewArea2=="Kermanshah" | NewArea2=="Markazi" |
               NewArea2=="Yazd" | NewArea2=="Kordestan" |
               NewArea2=="Chaharmahal" | 
               NewArea2=="Ardebil" | NewArea2=="Booshehr" |
               NewArea2=="Sh_Ahvaz" | NewArea2=="Kerman" |
               NewArea2=="Lorestan" | NewArea2=="Az_Gharbi" |
                 NewArea2=="Khorasan_Shomali",
               cluster2:=4]
dt2Urban<-dt2Urban[NewArea2=="Khoozestan" |NewArea2=="Ilam" |
                     NewArea2=="Khorasan_Jonoobi" | 
                     NewArea2=="Hormozgan" | NewArea2=="Kohkilooye" |
                     NewArea2=="Khorasan_Razavi",
                   cluster2:=5]
dt2Urban<-dt2Urban[NewArea2=="Sistan",cluster2:=6]

#####Rural#####
dt2Rural<-dt2Rural[NewArea2=="Tehran" | NewArea2=="Alborz",
                   cluster2:=1]
dt2Rural<-dt2Rural[NewArea2=="Esfahan" | NewArea2=="Yazd" |
                     NewArea2=="Mazandaran" | NewArea2=="Gilan" ,
                   cluster2:=2]
dt2Rural<-dt2Rural[NewArea2=="Az_Sharghi" | NewArea2=="Ghom" |
                     NewArea2=="Golestan" | NewArea2=="Semnan"|
                     NewArea2=="Kermanshah" | NewArea2=="Ghazvin",
                   cluster2:=3]
dt2Rural<-dt2Rural[NewArea2=="Chaharmahal" | NewArea2=="Fars" |
                     NewArea2=="Zanjan" | NewArea2=="Markazi" |
                     NewArea2=="Booshehr" | NewArea2=="Hamedan" |
                     NewArea2=="Kordestan" ,
                   cluster2:=4]
dt2Rural<-dt2Rural[NewArea2=="Khorasan_Jonoobi" | NewArea2=="Lorestan" |
                     NewArea2=="Khorasan_Shomali" | NewArea2=="Az_Gharbi" |
                     NewArea2=="Hormozgan" | NewArea2=="Khoozestan" |
                     NewArea2=="Khorasan_Razavi" | NewArea2=="Ardebil" |
                     NewArea2=="Kohkilooye" | NewArea2=="Ilam" |
                     NewArea2=="Kerman" ,
                   cluster2:=5]
dt2Rural<-dt2Rural[NewArea2=="Sistan",cluster2:=6]
                   
#####Merge#####
dt2total<-rbind(dt2Urban,dt2Rural)
dt2total[,Region:=NULL]
dt2total[,NewArea:=NULL]
dt2total[,NewArea2:=NULL]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-merge(MD,dt2total,by=c("HHID"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")