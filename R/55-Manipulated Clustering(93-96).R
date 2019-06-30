#55-Manipulated Clustering(93-96).R
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
dt2Urban<-MD[Region=="Urban",.(NewArea,NewArea2,Region,HHID)]
dt2Rural<-MD[Region=="Rural",.(NewArea,NewArea2,Region,HHID)]

#####Urban#####
dt2Urban<-dt2Urban[NewArea2=="Sh_Tehran",cluster3:=1]
dt2Urban<-dt2Urban[NewArea2=="Sh_Shiraz" | NewArea2=="Sh_Esfahan" |
                     NewArea2=="Sh_Bandarabas" |
                     NewArea2=="Sh_Karaj" ,
                   cluster3:=2]
dt2Urban<-dt2Urban[NewArea2=="Gilan" | NewArea2=="Alborz" |
                     NewArea2=="Kohkilooye" ,
                   cluster3:=3]
dt2Urban<-dt2Urban[NewArea2=="Mazandaran"  | 
                     NewArea2=="Sh_Ahvaz" | NewArea2=="Tehran" |
                     NewArea2=="Yazd" | NewArea2=="Sh_Tabriz" |
                     NewArea2=="Sh_Mashhad" | NewArea2=="Ghom" |
                     NewArea2=="Sh_Kerman" | NewArea2=="Sh_Arak" |
                     NewArea2=="Ghazvin" | NewArea2=="Fars"  | NewArea2=="Markazi"   |
                     NewArea2=="Esfahan" ,
                   cluster3:=4]
dt2Urban<-dt2Urban[NewArea2=="Az_Sharghi" |
                     NewArea2=="Semnan" | NewArea2=="Hamedan" |  
                     NewArea2=="Kerman" | NewArea2=="Zanjan" |
                     NewArea2=="Hormozgan"|
                     NewArea2=="Booshehr" | NewArea2=="Kordestan" |
                     NewArea2=="Ardebil" | NewArea2=="Chaharmahal" |
                     NewArea2=="Sh_Urmia",
                   cluster3:=5]
dt2Urban<-dt2Urban[NewArea2=="Khoozestan" |NewArea2=="Ilam" |
                     NewArea2=="Khorasan_Razavi" | 
                     NewArea2=="Khorasan_Jonoobi" | NewArea2=="Az_Gharbi" |
                     NewArea2=="Kermanshah" | NewArea2=="Lorestan" |
                     NewArea2=="Sh_Kermanshah" | 
                     NewArea2=="Khorasan_Shomali" | NewArea2=="Golestan",
                   cluster3:=6]
dt2Urban<-dt2Urban[NewArea2=="Sistan" | NewArea2=="Sh_Zahedan",
                   cluster3:=7]
save(dt2Urban,file ="dt2Urban.rda")
#####Rural#####
dt2Rural<-dt2Rural[ NewArea2=="Tehran" | NewArea2=="Alborz",
                    cluster3:=1]
dt2Rural<-dt2Rural[NewArea2=="Esfahan" | NewArea2=="Mazandaran" |
                     NewArea2=="Kohkilooye" | NewArea2=="Booshehr",
                   cluster3:=2]
dt2Rural<-dt2Rural[ NewArea2=="Yazd"  |
                      NewArea2=="Gilan" | NewArea2=="Zanjan" |
                      NewArea2=="Chaharmahal"| NewArea2=="Hormozgan" | NewArea2=="Az_Sharghi" | 
                      NewArea2=="Ghom" | 
                      NewArea2=="Fars" | NewArea2=="Ghazvin" | 
                      NewArea2=="Markazi",
                    cluster3:=3]
dt2Rural<-dt2Rural[  NewArea2=="Semnan"| NewArea2=="Hamedan" |
                       NewArea2=="Ardebil" | 
                       NewArea2=="Kordestan"|
                       NewArea2=="Khoozestan",
                    cluster3:=4]
dt2Rural<-dt2Rural[NewArea2=="Khorasan_Razavi" | NewArea2=="Kermanshah" |
                     NewArea2=="Kerman" | NewArea2=="Golestan" |
                     NewArea2=="Khorasan_Jonoobi" | NewArea2=="Lorestan" |
                     NewArea2=="Khorasan_Shomali" | NewArea2=="Az_Gharbi" |
                     NewArea2=="Ilam" ,
                   cluster3:=5]
dt2Rural<-dt2Rural[NewArea2=="Sistan",cluster3:=6]
save(dt2Rural,file ="dt2Rural.rda")
#####Merge#####
dt2total<-rbind(dt2Urban,dt2Rural)


dt2total[,HHID:=NULL]
dt2total<-distinct(dt2total)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-merge(MD,dt2total,by=c("NewArea","NewArea2","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")