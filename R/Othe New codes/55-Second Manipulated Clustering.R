#53-Second Manipulated Clustering.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
library(dplyr)
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
dt2Urban<-dt2Urban[NewArea_Name=="Sh_Tehran",cluster3:=1]
dt2Urban<-dt2Urban[NewArea_Name=="Sh_Shiraz" | NewArea_Name=="Sh_Esfahan" |
                     NewArea_Name=="Sh_Bandarabas" |
                     NewArea_Name=="Sh_Karaj" ,
                   cluster3:=2]
dt2Urban<-dt2Urban[NewArea_Name=="Gilan" | NewArea_Name=="Alborz",
                   cluster3:=3]
dt2Urban<-dt2Urban[NewArea_Name=="Mazandaran"  | 
                      NewArea_Name=="Sh_Ahvaz" | NewArea_Name=="Tehran"|
                     NewArea_Name=="Kohkilooye"  |
                     NewArea_Name=="Yazd" | NewArea_Name=="Sh_Tabriz" |
                     NewArea_Name=="Sh_Mashhad" | NewArea_Name=="Ghom" |
                     NewArea_Name=="Sh_Kerman" | NewArea_Name=="Sh_Arak" |
                     NewArea_Name=="Ghazvin" | NewArea_Name=="Fars" |
                     NewArea_Name=="Hormozgan" | NewArea_Name=="Markazi" |
                     NewArea_Name=="Semnan" | NewArea_Name=="Hamedan"  |
                     NewArea_Name=="Esfahan" ,
                   cluster3:=4]
dt2Urban<-dt2Urban[NewArea_Name=="Az_Sharghi" |  
                     NewArea_Name=="Kerman" | NewArea_Name=="Zanjan" |
                     NewArea_Name=="Booshehr" | NewArea_Name=="Kordestan" |
                     NewArea_Name=="Ardebil" | NewArea_Name=="Chaharmahal" |
                     NewArea_Name=="Khorasan_Razavi",
                   cluster3:=5]
dt2Urban<-dt2Urban[NewArea_Name=="Khoozestan" |NewArea_Name=="Ilam" |
                     NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Az_Gharbi" |
                     NewArea_Name=="Kermanshah" | NewArea_Name=="Lorestan" |
                     NewArea_Name=="Sh_Kermanshah" | NewArea_Name=="Sh_Urmia" ,
                   cluster3:=6]
dt2Urban<-dt2Urban[NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Golestan",
                  cluster3:=7]
dt2Urban<-dt2Urban[NewArea_Name=="Sistan" | NewArea_Name=="Sh_Zahedan",
                   cluster3:=8]
save(dt2Urban,file ="dt2Urban.rda")
#####Rural#####
dt2Rural<-dt2Rural[ NewArea_Name=="Tehran" | NewArea_Name=="Alborz",
                   cluster3:=1]
dt2Rural<-dt2Rural[NewArea_Name=="Esfahan" | NewArea_Name=="Mazandaran" ,
                   cluster3:=2]
dt2Rural<-dt2Rural[ NewArea_Name=="Yazd" |
                     NewArea_Name=="Kohkilooye" | NewArea_Name=="Booshehr" |
                       NewArea_Name=="Gilan" ,
                   cluster3:=3]
dt2Rural<-dt2Rural[ NewArea_Name=="Hormozgan" | NewArea_Name=="Az_Sharghi" | 
                      NewArea_Name=="Ghom" | NewArea_Name=="Semnan"|
                     NewArea_Name=="Fars" | NewArea_Name=="Ghazvin",
                   cluster3:=4]
dt2Rural<-dt2Rural[  NewArea_Name=="Zanjan" | NewArea_Name=="Markazi" |
                       NewArea_Name=="Hamedan",
                    cluster3:=5]
dt2Rural<-dt2Rural[NewArea_Name=="Khorasan_Razavi" | NewArea_Name=="Kermanshah" |
                     NewArea_Name=="Kerman" | NewArea_Name=="Golestan" |
                     NewArea_Name=="Chaharmahal"|
                     NewArea_Name=="Ardebil" | 
                      NewArea_Name=="Kordestan"|
                     NewArea_Name=="Khoozestan",
                   cluster3:=6]
dt2Rural<-dt2Rural[NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Lorestan" |
                     NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Az_Gharbi" |
                     NewArea_Name=="Ilam" ,
                   cluster3:=7]
dt2Rural<-dt2Rural[NewArea_Name=="Sistan",cluster3:=8]
save(dt2Rural,file ="dt2Rural.rda")
#####Merge#####
dt2total<-rbind(dt2Urban,dt2Rural)


dt2total[,HHID:=NULL]
dt2total<-distinct(dt2total)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-merge(MD,dt2total,by=c("NewArea","NewArea_Name","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")