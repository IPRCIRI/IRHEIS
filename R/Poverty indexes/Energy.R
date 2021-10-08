#Maskan

#Zahra Shahidi
#2021


cat("\n\n================ Energy Index =====================================\n")

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(writexl)


Geoo <- as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx",
                                 sheet = "Sheet1"))
Geo_O<-as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx", 
                                sheet = "Sheet2"))
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  MD<-merge(MD,HHHouseProperties,by=c("HHID"),all.x = T)
  
a<-MD[,sum(Weight)]
Cook<-MD[,sum(Weight)/a,by=c("cookfuel")] 
Heat<-MD[,sum(Weight)/a,by=c("heatfuel")] 
Hot<-MD[,sum(Weight)/a,by=c("hotwater")] 


if (year==Settings$startyear){

  CookFuel<-Cook
  HeatFuel<-Heat
  HotWater<-Hot
}else{

  CookFuel<-rbind(CookFuel,Cook)
  HeatFuel<-rbind(HeatFuel,Heat)
  HotWater<-rbind(HotWater,Hot)
}

a_Region<-MD[,sum(Weight),by=c("Region")]
Cook_Region<-MD[,sum(Weight)/a,by=c("cookfuel","Region")] 
Heat_Region<-MD[,sum(Weight)/a,by=c("cookfuel","Region")] 
Hot_Region<-MD[,sum(Weight)/a,by=c("cookfuel","Region")] 


if (year==Settings$startyear){
  
  CookFuel_Region<-Cook_Region
  HeatFuel_Region<-Heat_Region
  HotWater_Region<-Hot_Region
}else{
  
  CookFuel_Region<-rbind(CookFuel_Region,Cook_Region)
  HeatFuel_Region<-rbind(HeatFuel_Region,Heat_Region)
  HotWater_Region<-rbind(HotWater_Region,Hot_Region)
}

a_Proveince<-MD[,sum(Weight),by=c("Province")]
Cook_Proveince<-MD[,sum(Weight)/a,by=c("cookfuel","Province")] 
Heat_Proveince<-MD[,sum(Weight)/a,by=c("cookfuel","Province")] 
Hot_Proveince<-MD[,sum(Weight)/a,by=c("cookfuel","Province")] 


if (year==Settings$startyear){
  
  CookFuel_Proveince<-Cook_Proveince
  HeatFuel_Proveince<-Heat_Proveince
  HotWater_Proveince<-Hot_Proveince
}else{
  
  CookFuel_Proveince<-rbind(CookFuel_Proveince,Cook_Proveince)
  HeatFuel_Proveince<-rbind(HeatFuel_Proveince,Heat_Proveince)
  HotWater_Proveince<-rbind(HotWater_Proveince,Hot_Proveince)
}


}