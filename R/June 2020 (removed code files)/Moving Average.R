# Moving Average.R
# 
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Moving Average =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(pracma)
library(tidyverse)     
library(lubridate)     
library(fpp2)          
library(zoo)
library(smooth)
library(plotrix)
library(Hmisc)
library(gridExtra)

load(file = "Needed_Calorie.rda")

Needed_Calorie_M_W<-Needed_Calorie[Sex_W=="M",.(Age,Needed_Calorie_W,Sex_W)]
Needed_Calorie_W_W<-Needed_Calorie[Sex_W=="W",.(Age,Needed_Calorie_W,Sex_W)]

Boy<-as.data.table(ma(Needed_Calorie_M_W$Needed_Calorie_W, order=5, centre = TRUE))
Boy<-cbind(Needed_Calorie_M_W[,.(Age,Needed_Calorie_W)],Boy)
Boy[is.na(Boy)] <- 0
Boy<-Boy[,Smoothed_Needed_Calorie:=ifelse(x>0,x,Needed_Calorie_W)]
Boy<-Boy[,.(Age,Needed_Calorie_W,Smoothed_Needed_Calorie)]

p1<-ggplot(Boy,aes(Age,Needed_Calorie_W,alpha=0.5))+geom_density(stat="identity")
p2<-ggplot(Boy,aes(Age,Smoothed_Needed_Calorie,alpha=0.5))+geom_density(stat="identity")
grid.arrange(p1,p2,nrow=2) 


Girl<-as.data.table(ma(Needed_Calorie_W_W$Needed_Calorie_W, order=5, centre = TRUE))
Girl<-cbind(Needed_Calorie_W_W[,.(Age,Needed_Calorie_W)],Girl)
Girl[is.na(Girl)] <- 0
Girl<-Girl[,Smoothed_Needed_Calorie:=ifelse(x>0,x,Needed_Calorie_W)]
Girl<-Girl[,.(Age,Needed_Calorie_W,Smoothed_Needed_Calorie)]

p1<-ggplot(Girl,aes(Age,Needed_Calorie_W,alpha=0.5))+geom_density(stat="identity")
p2<-ggplot(Girl,aes(Age,Smoothed_Needed_Calorie,alpha=0.5))+geom_density(stat="identity")
grid.arrange(p1,p2,nrow=2) 

y.movavg1 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "s")) 
y.movavg2 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "t")) 
y.movavg3 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "w")) 
y.movavg4 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "m")) 
y.movavg5 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "e")) 
y.movavg6 <-as.data.table(movavg(x = Needed_Calorie_M_W$Needed_Calorie_W, n = 2, type = "r")) 


Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg1)
Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg2)
Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg3)
Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg4)
Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg5)
Needed_Calorie_M_W<-cbind(Needed_Calorie_M_W,y.movavg6)

A<-as.data.table(rollmean(Needed_Calorie_M_W$Needed_Calorie_W, 3,
                         na.pad = FALSE,align = c("center")))

cma(Needed_Calorie_M_W$Needed_Calorie_W, order = NULL,silent = FALSE)



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")