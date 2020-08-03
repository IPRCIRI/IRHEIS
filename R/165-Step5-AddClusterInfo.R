#165-Step 5- Clustering.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Clustering =====================================\n")
library(yaml)
library(dplyr)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  #####Clustering#####
  dt2Urban<-MD[Region=="Urban",.(NewArea,NewArea_Name,Region,HHID)]
  dt2Rural<-MD[Region=="Rural",.(NewArea,NewArea_Name,Region,HHID)]
  
  #####Urban#####
  dt2Urban<-dt2Urban[NewArea_Name=="Sh_Tehran",cluster3:=1]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Sh_Shiraz" | NewArea_Name=="Sh_Esfahan" |
                       NewArea_Name=="Sh_Bandarabas" |
                       NewArea_Name=="Sh_Karaj" |
                       NewArea_Name=="Sh_Rasht",
                     cluster3:=2]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Gilan" | NewArea_Name=="Alborz" |
                       NewArea_Name=="Kohkilooye" | NewArea_Name=="Ghazvin" | 
                       NewArea_Name=="Markazi"   |  NewArea_Name=="Esfahan" |
                       NewArea_Name=="Ghom" |  NewArea_Name=="Sh_Arak" |
                       NewArea_Name=="Tehran" |  NewArea_Name=="Sh_Tabriz" |
                       NewArea_Name=="Mazandaran"  | NewArea_Name=="Sh_Yazd" ,
                     cluster3:=3]
  
  dt2Urban<-dt2Urban[  NewArea_Name=="Yazd" |
                         NewArea_Name=="Sh_Kerman" |
                         NewArea_Name=="Sh_Hamedan" |
                         NewArea_Name=="Chaharmahal" |
                         NewArea_Name=="Zanjan" | NewArea_Name=="Hamedan" |
                         NewArea_Name=="Sh_Mashhad" |  NewArea_Name=="Sh_Urmia" |
                         NewArea_Name=="Sh_Ahvaz" |   NewArea_Name=="Hormozgan"|
                         NewArea_Name=="Booshehr" |
                         NewArea_Name=="Semnan" | NewArea_Name=="Fars",
                       cluster3:=4]
  
  dt2Urban<-dt2Urban[ NewArea_Name=="Sh_Kermanshah" | NewArea_Name== "Az_Sharghi" |
                        NewArea_Name=="Kerman" | 
                        NewArea_Name=="Ardebil" |
                        NewArea_Name=="Ilam" ,
                      cluster3:=5]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Khoozestan" | NewArea_Name=="Kordestan" |
                       NewArea_Name=="Khorasan_Razavi" | 
                       NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Az_Gharbi" |
                       NewArea_Name=="Kermanshah" | NewArea_Name=="Lorestan" |
                       NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Golestan",
                     cluster3:=6]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Sistan" | NewArea_Name=="Sh_Zahedan",
                     cluster3:=7]
  
  save(dt2Urban,file ="dt2Urban.rda")
  #####Rural#####
  dt2Rural<-dt2Rural[ NewArea_Name=="Tehran" | NewArea_Name=="Alborz",
                      cluster3:=8]
  
  dt2Rural<-dt2Rural[NewArea_Name=="Esfahan" | NewArea_Name=="Mazandaran" ,
                     cluster3:=9]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Ghazvin" | NewArea_Name=="Yazd"  |
                        NewArea_Name=="Gilan" |
                        NewArea_Name=="Kohkilooye" | NewArea_Name=="Booshehr",
                      cluster3:=10]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Zanjan" |
                        NewArea_Name=="Chaharmahal"| NewArea_Name=="Hormozgan" | NewArea_Name=="Az_Sharghi" | 
                        NewArea_Name=="Ghom" | 
                        NewArea_Name=="Fars" |  
                        NewArea_Name=="Markazi" |  NewArea_Name=="Semnan",
                      cluster3:=11]
  
  dt2Rural<-dt2Rural[  NewArea_Name=="Hamedan" |
                         NewArea_Name=="Az_Gharbi" |  NewArea_Name=="Kermanshah" |
                         NewArea_Name=="Kerman" | NewArea_Name=="Golestan" |
                         NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Lorestan" |
                         NewArea_Name=="Kordestan"|
                         NewArea_Name=="Khoozestan" |
                         NewArea_Name=="Ardebil" | NewArea_Name=="Ilam",
                       cluster3:=12]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Khorasan_Razavi" |
                        NewArea_Name=="Sistan" ,
                      cluster3:=13]
  
  save(dt2Rural,file ="dt2Rural.rda")
  #####Merge#####
  dt2total<-rbind(dt2Urban,dt2Rural)
  
  
  dt2total[,HHID:=NULL]
  dt2total<-distinct(dt2total)
  
  MD<-merge(MD,dt2total,by=c("NewArea","NewArea_Name","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  cluster<-MD[,.(HHID,cluster3)]
  save(cluster,file=paste0(Settings$HEISProcessedPath,"Y",year,"cluster.rda"))
  
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")