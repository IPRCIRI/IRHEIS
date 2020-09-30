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
dt2Urban<-MD[Region=="Urban",.(NewArea,NewArea_Name,Region,HHID)]
dt2Rural<-MD[Region=="Rural",.(NewArea,NewArea_Name,Region,HHID)]

#####Urban#####
dt2Urban<-dt2Urban[NewArea_Name=="Sh_Tehran",cluster2:=1]
dt2Urban<-dt2Urban[NewArea_Name=="Sh_Shiraz" | NewArea_Name=="Sh_Esfahan" |
              NewArea_Name=="Sh_Bandarabas" | NewArea_Name=="Gilan" |
              NewArea_Name=="Sh_Karaj" | NewArea_Name=="Sh_Arak" |
              NewArea_Name=="Mazandaran"  ,
              cluster2:=2]
dt2Urban<-dt2Urban[NewArea_Name=="Ghom" | NewArea_Name=="Alborz" |
              NewArea_Name=="Sh_Kermanshah" | NewArea_Name=="Hamedan" |
              NewArea_Name=="Az_Sharghi" | NewArea_Name=="Sh_Tabriz" |
              NewArea_Name=="Tehran" | NewArea_Name=="Semnan" |
              NewArea_Name=="Sh_Urmia" | NewArea_Name=="Sh_Mashhad" |
              NewArea_Name=="Esfahan" | NewArea_Name=="Fars" |
              NewArea_Name=="Sh_Kerman" | NewArea_Name=="Ghazvin" ,
              cluster2:=3]
dt2Urban<-dt2Urban[NewArea_Name=="Zanjan" | NewArea_Name=="Golestan" |
               NewArea_Name=="Kermanshah" | NewArea_Name=="Markazi" |
               NewArea_Name=="Yazd" | NewArea_Name=="Kordestan" |
               NewArea_Name=="Chaharmahal" | 
               NewArea_Name=="Ardebil" | NewArea_Name=="Booshehr" |
               NewArea_Name=="Sh_Ahvaz" | NewArea_Name=="Kerman" |
               NewArea_Name=="Lorestan" | NewArea_Name=="Az_Gharbi" |
                 NewArea_Name=="Khorasan_Shomali",
               cluster2:=4]
dt2Urban<-dt2Urban[NewArea_Name=="Khoozestan" |NewArea_Name=="Ilam" |
                     NewArea_Name=="Khorasan_Jonoobi" | 
                     NewArea_Name=="Hormozgan" | NewArea_Name=="Kohkilooye" |
                     NewArea_Name=="Khorasan_Razavi" | NewArea_Name=="Sh_Zahedan",
                   cluster2:=5]
dt2Urban<-dt2Urban[NewArea_Name=="Sistan",cluster2:=6]

#####Rural#####
dt2Rural<-dt2Rural[NewArea_Name=="Tehran" | NewArea_Name=="Alborz",
                   cluster2:=1]
dt2Rural<-dt2Rural[NewArea_Name=="Esfahan" | NewArea_Name=="Yazd" |
                     NewArea_Name=="Mazandaran" | NewArea_Name=="Gilan" ,
                   cluster2:=2]
dt2Rural<-dt2Rural[NewArea_Name=="Az_Sharghi" | NewArea_Name=="Ghom" |
                     NewArea_Name=="Golestan" | NewArea_Name=="Semnan"|
                     NewArea_Name=="Kermanshah" | NewArea_Name=="Ghazvin",
                   cluster2:=3]
dt2Rural<-dt2Rural[NewArea_Name=="Chaharmahal" | NewArea_Name=="Fars" |
                     NewArea_Name=="Zanjan" | NewArea_Name=="Markazi" |
                     NewArea_Name=="Booshehr" | NewArea_Name=="Hamedan" |
                     NewArea_Name=="Kordestan" ,
                   cluster2:=4]
dt2Rural<-dt2Rural[NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Lorestan" |
                     NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Az_Gharbi" |
                     NewArea_Name=="Hormozgan" | NewArea_Name=="Khoozestan" |
                     NewArea_Name=="Khorasan_Razavi" | NewArea_Name=="Ardebil" |
                     NewArea_Name=="Kohkilooye" | NewArea_Name=="Ilam" |
                     NewArea_Name=="Kerman" ,
                   cluster2:=5]
dt2Rural<-dt2Rural[NewArea_Name=="Sistan",cluster2:=6]
                   
#####Merge#####
dt2total<-rbind(dt2Urban,dt2Rural)
dt2total[,Region:=NULL]
dt2total[,NewArea:=NULL]
dt2total[,NewArea_Name:=NULL]

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