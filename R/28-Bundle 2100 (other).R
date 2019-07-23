# Bundle 2100
# Builds the Food Groups data.table for households
#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ FoodGroups =====================================\n")
year<-96
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
 
 Base2<-merge(BigFData,FData,by="HHID")
 
 Base2<-merge(Base2,MD[,.(HHID,NewArea,NewArea2,ProvinceCode,cluster3,Decile,
                            FinalFoodPoor,FinalPoor,Weight,EqSizeRevOECD,EqSizeCalory,PEngel)],by="HHID")

 Base<-Base2[PEngel>0.5]
 Base<-Base[,FGrams_Per:=FGrams/EqSizeCalory]
 Base<-Base[,FoodKCalories_Per:=FoodKCaloriesHH/EqSizeCalory]
 Base<-Base[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
 Base<-Base[,Coef:=FoodKCalories_Per/2100]
 
 Base<-Base[,FoodKCalories_PerNew:=FoodKCalories_Per/Coef]
 Base<-Base[,FGrams_PerNew:=FGrams_Per/Coef]
 BaseX <- Base[,.(FoodKCalories2=mean(FoodKCalories_PerNew),
                  FoodKCalories3=mean(FoodKCalories_Per),
                  FoodProtein2=mean(FoodProtein_Per),
                  Weight=mean(Weight),
                  ProvinceCode=mean(ProvinceCode)),by=HHID]
 
 BaseX1<-Base[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight),
                            cluster3=mean(cluster3)),
             by=.(ProvinceCode,FoodType)]

 BaseX1<-BaseX1[,N2:=max(N),by=.(ProvinceCode)]
 BaseX1<-BaseX1[,Average_New:=N*Average_Consumption/N2]
 BaseM<-BaseX1[,.(ProvinceCode,N,N2,FoodType,Average_New)]
 BaseM2<-BaseM[FoodType=="Berenj"]
 BaseM<-BaseM[,.(ProvinceCode,FoodType,N,Average_New)]
 
 BaseX<-BaseX[,Region:=as.integer(str_sub(HHID,1,1))]
 BaseX[,weighted.mean(FoodProtein2,Weight)]
 BaseX[,weighted.mean(FoodProtein2,Weight),by=.(Region,ProvinceCode)]
 
 
 BaseXUrban<-BaseX[Region==1]
 BaseXRural<-BaseX[Region==2]
 plot(FoodKCalories3~FoodProtein2,data=BaseXUrban)
 BaseXRural<-BaseXRural[FoodProtein2<700]
 plot(FoodKCalories3~FoodProtein2,data=BaseXRural)
 
#Base2<-Base[FinalPoor==0,.(.N,Average_Consumption=weighted.mean(FGrams_Per,Weight),
# cluster3=mean(cluster3)),by=.(ProvinceCode,FoodType)]
 
# Base3<-MD[PEngel>0.6 & FinalPoor==1]

# Base4<-MD[FinalPoor==0,.(.N,cluster3=mean(cluster3)),by=.(ProvinceCode)]

# Base[FoodType=="Goosht" & FinalPoor==1 ,weighted.mean(FGrams_Per,Weight,na.rm = TRUE),by=.(ProvinceCode)]


cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")