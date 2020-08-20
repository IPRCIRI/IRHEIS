
rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(spatstat)
library(writexl)
library(tidyr)
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPrices.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))

goosht<-BigFData[FoodType=="Goosht"]
goosht<-goosht[,Goosht_Grams:=FGrams  ]
morgh<-BigFData[FoodType=="Morgh"]
morgh<-morgh[,Morgh_Grams:=FGrams  ]
mive<-BigFData[FoodType=="Mive"]
mive<-mive[,Mive_Grams:=FGrams  ]
nan<-BigFData[FoodType=="Nan"]
nan<-nan[,Nan_Grams:=FGrams  ]
sibzamini<-BigFData[FoodType=="Sibzamini"]
sibzamini<-sibzamini[,Sibzamini_Grams:=FGrams  ]
makarooni<-BigFData[FoodType=="Makarooni"]
makarooni<-makarooni[,Makarooni_Grams:=FGrams  ]
berenj<-BigFData[FoodType=="Berenj"]
berenj<-berenj[,Berenj_Grams:=FGrams  ]
hoboobat<-BigFData[FoodType=="Hoboobat"]
hoboobat<-hoboobat[,Hoboobat_Grams:=FGrams  ]
sabzi<-BigFData[FoodType=="Sabzi"]
sabzi<-sabzi[,Sabzi_Grams:=FGrams  ]
roghan<-BigFData[FoodType=="Roghan"]
roghan<-roghan[,Roghan_Grams:=FGrams  ]
ghand<-BigFData[FoodType=="Ghand"]
ghand<-ghand[,Ghand_Grams:=FGrams  ]
shir<-BigFData[FoodType=="Shir"]
shir<-shir[,Shir_Grams:=FGrams ]
panir<-BigFData[FoodType=="Panir"]
panir<-panir[,Panir_Grams:=FGrams ]
mast<-BigFData[FoodType=="Mast"]
mast<-mast[,Mast_Grams:=FGrams ]
tokhmemorgh<-BigFData[FoodType=="Tokhmemorgh"]
tokhmemorgh<-tokhmemorgh[,Tokhmemorgh_Grams:=FGrams  ]
mahi<-BigFData[FoodType=="Mahi"]
mahi<-mahi[,Mahi_Grams:=FGrams  ]

MD<-merge(MD,goosht[,.(HHID,Goosht_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,morgh[,.(HHID,Morgh_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mive[,.(HHID,Mive_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,nan[,.(HHID,Nan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sibzamini[,.(HHID,Sibzamini_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,makarooni[,.(HHID,Makarooni_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,berenj[,.(HHID,Berenj_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,hoboobat[,.(HHID,Hoboobat_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sabzi[,.(HHID,Sabzi_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,roghan[,.(HHID,Roghan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,ghand[,.(HHID,Ghand_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,shir[,.(HHID,Shir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,panir[,.(HHID,Panir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mast[,.(HHID,Mast_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,tokhmemorgh[,.(HHID,Tokhmemorgh_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mahi[,.(HHID,Mahi_Grams)],by=c("HHID"),all.x = T)

MD<-MD[,Labaniat_Grams:=Shir_Grams+Mast_Grams]

   
gram <-MD [FinalPoor==1,
           .(Nan_Grams= weighted.mean(Nan_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Berenj_Grams= weighted.mean(Berenj_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Makarooni_Grams=weighted.mean(Makarooni_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Hoboobat_Grams=weighted.mean(Hoboobat_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Sibzamini_Grams=weighted.mean(Sibzamini_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Sabzi_Grams=weighted.mean(Sabzi_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Mive_Grams=weighted.mean(Mive_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Goosht_Grams=weighted.mean(Goosht_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Morgh_Grams=weighted.mean(Morgh_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Tokhmemorgh_Grams=weighted.mean(Tokhmemorgh_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Labaniat_Grams=weighted.mean(Labaniat_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Roghan_Grams=weighted.mean(Roghan_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Ghand_Grams=weighted.mean(Ghand_Grams/EqSizeCalory,Weight*Size,na.rm=TRUE),
             Mahi_Grams=weighted.mean(Mahi_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE),
             Panir_Grams=weighted.mean(Panir_Grams/EqSizeCalory,Weight*Size,na.rm = TRUE),
             Calory=weighted.mean(FoodKCaloriesHH_Per,Weight*Size,na.rm = TRUE),
             Hygiene=weighted.mean(Hygiene_Exp/EqSizeOECD,Weight*Size),
             Medical=weighted.mean(Medical_Exp/EqSizeOECD,Weight*Size),
             Cloth=weighted.mean(Cloth_Exp/EqSizeOECD,Weight*Size),
             Education=weighted.mean(Education_Exp/EqSizeOECD,Weight*Size)),by=c("cluster3")]

Gram<-gram[,Year:=year]
if (year==90){
  PoorFeature<-Gram
}else{
PoorFeature<-rbind(Gram,PoorFeature)
}
}
 PoorFeature<- PoorFeature[,Year:=as.factor(Year)]
 PoorFeature<- PoorFeature[,cluster3:=as.factor(cluster3)]


ggplot(data=PoorFeature,aes(x=Year, y=Goosht_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Berenj_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Makarooni_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Sibzamini_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Nan_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Morgh_Grams, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Calory, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Hygiene, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Cloth, group=cluster3, colour=cluster3))+geom_line()
ggplot(data=PoorFeature,aes(x=Year, y=Education, group=cluster3, colour=cluster3))+geom_line()
