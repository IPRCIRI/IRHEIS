#50-Merge Data for CBN Method.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
  HHBase<-merge(HHBase,lactating,by="HHID")
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights>-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  
  
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
            "Durables", "Education", "Energy", "Furnitures","Hotels",
            "House", "Medicals","Behdashts","Transportations","Others",
           "Resturants","Barghs"
           #,"Benzins","Gazs","NaftSefids"
           )){
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
   }
  
 # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Added_Food.rda")) 
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  FData[,Region:=NULL]
  #for (col in c("FoodKCaloriesHH")) FData[is.na(get(col)), (col) := 0]
  FData <- FData[FoodKCaloriesHH>0]
  
  #merge groups
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  FData[,Size:=NULL]
  MD<-merge(MD,FData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HHWeights ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodData,by =c("HHID"),all=TRUE)
 # MD<-merge(MD,Added_Food,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,BehdashtData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BenzinData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,GazData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,BarghData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,NaftSefidData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,DurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "EducExpenditure", "Energy_Exp", 
                "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", "Transportation_Exp",
                "Other_Exp", "ServiceExp", "Medical_Exp", "Durable_Exp", 
                "Resturant_Exp","Bargh_Exp"
                #,"Benzin_Exp","Gaz_Exp","NaftSefid_Exp"
                )) 
    MD[is.na(get(col)), (col) := 0]
#  MD<-MD[,Yaraneh:=416000*Size]
  
  #Calculate Monthly Total Expenditures 
  nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp",
          "Amusement_Exp", "Communication_Exp", 
          "Energy_Exp", "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", 
          "Transportation_Exp", "Other_Exp", "ServiceExp")
  w <- c(nw, "Medical_Exp", "Durable_Exp")
 # pw <- c(nw, "Added_Food_Exp_Month")
  #Lw <- c(pw,  "Medical_Exp", "Durable_Exp")
  
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
 # MD[, Total_Exp_Month := Total_Exp_Month + Yaraneh]
 # MD[, Total_Exp_Month_nondurable := Total_Exp_Month_nondurable + Yaraneh]
  
 # MD[, Total_Exp_Month := Total_Exp_Month*1.2]
 # MD[, Total_Exp_Month_nondurable := Total_Exp_Month_nondurable*1.2]
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeRevOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeRevOECD]

 
  #MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]
  #MD[,Home_Per_Metr:=MetrPrice/EqSizeRevOECD]
  
  #Calculate Per Values
  MD[,EqSizeCalory3 :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  MD[,EqSizeCalory2 :=
       NAge1B*(Settings$KCaloryNeed_B1/Settings$KCaloryNeed_Adult) +
       NAge2B*(Settings$KCaloryNeed_B2/Settings$KCaloryNeed_Adult) +
       NAge3B*(Settings$KCaloryNeed_B3/Settings$KCaloryNeed_Adult) +
       NAge4B*(Settings$KCaloryNeed_B4/Settings$KCaloryNeed_Adult) +
       NAge5B*(Settings$KCaloryNeed_B5/Settings$KCaloryNeed_Adult) +
       NAge6B*(Settings$KCaloryNeed_B6/Settings$KCaloryNeed_Adult) +
       NAge7B*(Settings$KCaloryNeed_B7/Settings$KCaloryNeed_Adult) +
       NAge8B*(Settings$KCaloryNeed_B8/Settings$KCaloryNeed_Adult) +
       NAge9B*(Settings$KCaloryNeed_B9/Settings$KCaloryNeed_Adult) +
       NAge10B*(Settings$KCaloryNeed_B10/Settings$KCaloryNeed_Adult)+
       NAge1G*(Settings$KCaloryNeed_G1/Settings$KCaloryNeed_Adult) +
       NAge2G*(Settings$KCaloryNeed_G2/Settings$KCaloryNeed_Adult) +
       NAge3G*(Settings$KCaloryNeed_G3/Settings$KCaloryNeed_Adult) +
       NAge4G*(Settings$KCaloryNeed_G4/Settings$KCaloryNeed_Adult) +
       NAge5G*(Settings$KCaloryNeed_G5/Settings$KCaloryNeed_Adult) +
       NAge6G*(Settings$KCaloryNeed_G6/Settings$KCaloryNeed_Adult) +
       NAge7G*(Settings$KCaloryNeed_G7/Settings$KCaloryNeed_Adult) +
       NAge8G*(Settings$KCaloryNeed_G8/Settings$KCaloryNeed_Adult) +
       NAge9G*(Settings$KCaloryNeed_G9/Settings$KCaloryNeed_Adult) +
       NAge10G*(Settings$KCaloryNeed_G10/Settings$KCaloryNeed_Adult)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult)]
  
  MD[,EqSizeCalory :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Settings$KCaloryNeed_Adult) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Settings$KCaloryNeed_Adult) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Settings$KCaloryNeed_Adult) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Settings$KCaloryNeed_Adult) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Settings$KCaloryNeed_Adult) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Settings$KCaloryNeed_Adult) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Settings$KCaloryNeed_Adult) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Settings$KCaloryNeed_Adult) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Settings$KCaloryNeed_Adult) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Settings$KCaloryNeed_Adult) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Settings$KCaloryNeed_Adult) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Settings$KCaloryNeed_Adult) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Settings$KCaloryNeed_Adult) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Settings$KCaloryNeed_Adult) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Settings$KCaloryNeed_Adult) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Settings$KCaloryNeed_Adult) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Settings$KCaloryNeed_Adult) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Settings$KCaloryNeed_Adult)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult)]
  

  MD[,Relative_Calorie1 :=FoodKCaloriesHH/Calorie_Need1]
  MD[,Relative_Calorie2 :=FoodKCaloriesHH/Calorie_Need2]
  MD[,OriginalFoodExpenditure_Per :=OriginalFoodExpenditure/EqSizeCalory]
  MD[,FoodKCaloriesHH_Per:=FoodKCaloriesHH/EqSizeCalory]
  MD[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
  
  #Calculate per_Calory from resturants
  MD[,Calory_Price:=(OriginalFoodExpenditure_Per/FoodKCaloriesHH_Per)]
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)][order(Calory_Price)]
  MD[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
  for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
  MD[,TFoodKCaloriesHH:=FoodKCaloriesHH+ResturantKCalories]
  MD[,TOriginalFoodExpenditure:=OriginalFoodExpenditure+(Settings$OutFoodKCXShare*Resturant_Exp)]
  
  MD[,TOriginalFoodExpenditure_Per :=TOriginalFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCaloriesHH_Per:=TFoodKCaloriesHH/EqSizeCalory]
  
  ##############################################################
  #####for CV EV report  
 # MD[, Total_Exp_Month2 := Reduce(`+`, .SD), .SDcols=Lw]
 # MD[, Total_Exp_Month_nondurable2 := Reduce(`+`, .SD), .SDcols=pw]
  
 # MD[,Total_Exp_Month_Per2:=Total_Exp_Month2/EqSizeRevOECD]
 # MD[,Total_Exp_Month_Per_nondurable2:=Total_Exp_Month_nondurable2/EqSizeRevOECD]
 
#  MD[,FoodExpenditure_Per2 :=(FoodExpenditure+Added_Food_Exp_Month)/EqSizeCalory]
#  MD[,TFoodExpenditure2:=FoodExpenditure+Added_Food_Exp_Month+(Settings$OutFoodKCXShare*Resturant_Exp)]
 # MD[,TFoodExpenditure_Per2 :=TFoodExpenditure2/EqSizeCalory]
   ##############################################################
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  
  cat(MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight*Size)])
  
  MD[,weighted.mean(Bargh_Exp,Weight),by=.(ProvinceCode)]
  MD[,weighted.median(Bargh_Exp,Weight),by=.(ProvinceCode)]
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)