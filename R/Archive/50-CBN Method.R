#50-CBN Method.R
# 
# Copyright © 2018:Majid Einian- Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
 cat(paste0("\n------------------------------\nYear:",year,"\n"))
 
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"HHWeights",year,".rda"))
  HHWeights[,Year:=NULL]
  
  #load Expenditures
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Energy", "Furnitures","Hotels",
             "House", "Medicals","Behdashts","Transportations","Others",
             "Resturants")){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  }
  
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  FData[,Region:=NULL]
  #for (col in c("FoodKCalories")) FData[is.na(get(col)), (col) := 0]
  FData <- FData[FoodKCalories>0]
  
  #merge groups
  CBN<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FData ,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,HHWeights ,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FoodData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,CigarData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,ClothData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,AmusementData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,CommunicationData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,EducData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,EnergyData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,FurnitureData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,HotelData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,BehdashtData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,TransportationData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,OtherData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,HouseData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,MedicalData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,DurableData,by =c("HHID"),all=TRUE)
  CBN<-merge(CBN,ResturantData,by =c("HHID"),all=TRUE)
 for (col in c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
               "Communication_Exp", "EducExpenditure", "Energy_Exp", 
               "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", "Transportation_Exp",
               "Other_Exp", "ServiceExp", "Medical_Exp", "Durable_Exp", 
               "Resturant_Exp")) 
   CBN[is.na(get(col)), (col) := 0]
  
  
  #Calculate Monthly Total Expenditures 
  nw <- c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp",
                                "Amusement_Exp", "Communication_Exp", "EducExpenditure",
                                "Energy_Exp", "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", 
                                "Transportation_Exp", "Other_Exp", "ServiceExp")
  w <- c(nw, "Medical_Exp", "Durable_Exp")
  
  CBN[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  CBN[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]

  CBN[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeRevOECD]
  CBN[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeRevOECD]

  CBN<-merge(CBN,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  CBN<-CBN[Size!=0 & FoodExpenditure!=0 & !is.na(FoodKCalories)]
  #CBN[,Home_Per_Metr:=MetrPrice/EqSizeRevOECD]
  
  #Calculate Per Values
  CBN[,EqSizeCalory :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  CBN[,FoodExpenditure_Per :=FoodExpenditure/EqSizeCalory]
  CBN[,FoodKCalories_Per:=FoodKCalories/EqSizeCalory]
  
  #Calculate per_Calory from resturants
  CBN[,Calory_Price:=(FoodExpenditure_Per/FoodKCalories_Per)]
#  CBN[,Calory_Price_Area:=weighted.mean(Calory_Price,Weight,na.rm = TRUE),by=NewArea]
  CBN[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=NewArea]
  CBN[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
  for (col in c("ResturantKCalories")) CBN[is.na(get(col)), (col) := 0]
  CBN[,TFoodKCalories:=FoodKCalories+ResturantKCalories]
  CBN[,TFoodExpenditure:=FoodExpenditure+(Settings$OutFoodKCXShare*Resturant_Exp)]
  # CBN[,weighted.mean(TFoodKCalories,Weight,na.rm = TRUE),by=NewArea]
  # CBN[,weighted.mean(FoodKCalories,Weight,na.rm = TRUE),by=NewArea]
  
  CBN[,TFoodExpenditure_Per :=TFoodExpenditure/EqSizeCalory]
  CBN[,TFoodKCalories_Per:=TFoodKCalories/EqSizeCalory]
  
  
  #Sort Expenditure data
  CBN<- CBN[order(Total_Exp_Month_Per_nondurable)]
  
  #Calculate cumulative weights
  CBN<-CBN[Region=="Urban"]
  sum(CBN$Weight)
  CBN$cumweight <- cumsum(CBN$Weight)
  tx <- max(CBN$cumweight)
  
  #Calculate deciles by weights
  CBN[,Decile:=cut(cumweight,breaks = seq(0,tx,tx/10),labels = 1:10)]
  CBN[,Percentile:=cut(cumweight,breaks=seq(0,tx,tx/100),labels=1:100)]
  
  #Assume that deciles 1 and 2 are poor
  CBN[,Poor:=ifelse(Decile %in% 1:2,1,0)]
  CBNPoor<-CBN[Poor==1]
  
   
  #Food expenditures (equal 2100 KCAL)
  CBNPoor[,Bundle_Value:=TFoodExpenditure_Per*Settings$KCaloryNeed_Adult/TFoodKCalories_Per]
  CBNPoor[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE),by=NewArea]
 
  #Real Prices
  T_Bundle_Value<-subset(CBNPoor, NewArea==2301, select=c(Bundle_Value,Home_Per_Metr,Weight))
  Tehran_Bundle_Value1<-weighted.mean(T_Bundle_Value$Bundle_Value,T_Bundle_Value$Weight,na.rm = TRUE)
  Tehran_Bundle_Value2<-weighted.mean(T_Bundle_Value$Home_Per_Metr,T_Bundle_Value$Weight,na.rm = TRUE)
  
  CBNPoor[,RealPriceIndex1:=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/Tehran_Bundle_Value1,by=NewArea]
  CBNPoor[,RealPriceIndex2:=weighted.mean(Home_Per_Metr,Weight,na.rm = TRUE)/Tehran_Bundle_Value2,by=NewArea]
  CBNPoor[,weighted.mean(RealPriceIndex1,Weight),by=NewArea]
  CBNPoor[,weighted.mean(RealPriceIndex2,Weight),by=NewArea]
  
  Index<-CBNPoor[,.(RealPriceIndex1,RealPriceIndex2,NewArea,Weight)]
  Index<-Index[,RealPriceIndex:=(RealPriceIndex1+RealPriceIndex2)/2]
  Index<-Index[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
  Index<-Index[,.(NewArea,RealPriceIndex1,RealPriceIndex2,RealPriceIndex)]
  Index<-Index[,.(NewArea,RealPriceIndex)]
  CBN<-merge(CBN,Index,by=c("NewArea"),all.x = TRUE)
  CBN<-CBN[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable*RealPriceIndex]
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)