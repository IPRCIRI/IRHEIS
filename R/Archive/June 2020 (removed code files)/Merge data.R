
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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  
  
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Furnitures","HotelRestaurants",
             "HouseandEnergys","House", "Medicals","Hygienes","Transportations","Others",
             "Resturants"
  )){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
    load(file = paste0(Settings$HEISProcessedPath,"Y",year,"NonFreeDurableData.rda"))
    
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
  MD<-merge(MD,HouseandEnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelRestaurantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HygieneData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BenzinData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,GazData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BarghData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,NaftSefidData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,NonFreeDurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "Education_Exp", "HouseandEnergy_Exp", 
                "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", "Transportation_Exp",
                "Other_Exp", "Medical_Exp", "NonFreeDurable_Exp",
                "Resturant_Exp","ServiceExp"
  )) 
    MD[is.na(get(col)), (col) := 0]
  #  MD<-MD[,Yaraneh:=416000*Size]
  
  MD<-merge(MD,Calorie_Need)
  
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durablele_Detail.rda"))
  #MD<-merge(MD,Durablele_Detail)
  

   
   cpi97 <- read_excel("C:/Users/pc1/Desktop/cpi97.xlsx")
   cpi97<-as.data.table(cpi97)
   cpi97<-cpi97[Month>0]
   cpi97<-cpi97[,Year:=NULL]
   cpi97<-cpi97[,PeriodYear:=NULL]
   cpi97<-cpi97[,.(Month,FoodDrink_Index,Cigar_Index,Cloth_Index,HouseEnergy_Index,
                   Furniture_Index,Hygiene_Index,Transportation_Index,
                   Communication_Index,Amusement_Index,Hotel_Index,
                   Other_Index,Durable_Index,Total_Index)]
   
   MD<-merge(MD,cpi97,by=c("Month"),all.x=TRUE)
   
   FoodDrink_Index98<-219.7
   Cigar_Index98<-281.1
   Cloth_Index98<-197.8
   HouseEnergy_Index98<-155.1
   Furniture_Index98<-225.1
   Hygiene_Index98<-158.1
   Transportation_Index98<-195.9
   Communication_Index98<-146.7
   Amusement_Index98<-224.5
   Hotel_Index98<-182.0
   Other_Index98<-199.5
   Durable_Index98<-259.2
   Total_Index98<-185.1
   
   #Calculate Monthly Total Expenditures 
   nw <- c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp",
           "Amusement_Exp", "Communication_Exp", 
           "HouseandEnergy_Exp", "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", 
           "Transportation_Exp", "Other_Exp" )
   #w <- c(nw, "Medical_Exp", "Remain_Durable")
   w <- c(nw, "Medical_Exp", "NonFreeDurable_Exp")
   #w <- c(nw, "Medical_Exp", "Durable_Pure_Exp")
   # pw <- c(nw, "Added_Food_Exp_Month")
   #Lw <- c(pw,  "Medical_Exp", "Durable_Exp")
   
   MD<-MD[,FoodExpenditure:=FoodExpenditure*FoodDrink_Index98/FoodDrink_Index]
   MD<-MD[,OriginalFoodExpenditure:=OriginalFoodExpenditure*FoodDrink_Index98/FoodDrink_Index]
  MD<-MD[,Cigar_Exp:=Cigar_Exp*Cigar_Index98/Cigar_Index]
   MD<-MD[,Cloth_Exp:=Cloth_Exp*Cloth_Index98/Cloth_Index]
   MD<-MD[,HouseandEnergy_Exp:=HouseandEnergy_Exp*HouseEnergy_Index98/HouseEnergy_Index]
   MD<-MD[,Furniture_Exp:=Furniture_Exp*Furniture_Index98/Furniture_Index]
   MD<-MD[,Hygiene_Exp:=Hygiene_Exp*Hygiene_Index98/Hygiene_Index]
   MD<-MD[,Medical_Exp:=Medical_Exp*Hygiene_Index98/Hygiene_Index]
   MD<-MD[,Transportation_Exp:=Transportation_Exp*Transportation_Index98/Transportation_Index]
   MD<-MD[,Communication_Exp:=Communication_Exp*Communication_Index98/Communication_Index]
   MD<-MD[,Amusement_Exp:=Amusement_Exp*Amusement_Index98/Amusement_Index]
   MD<-MD[,HotelRestaurant_Exp:=HotelRestaurant_Exp*Hotel_Index98/Hotel_Index]
   MD<-MD[,Other_Exp:=Other_Exp*Other_Index98/Other_Index]
   MD<-MD[,NonFreeDurable_Exp:=NonFreeDurable_Exp*Durable_Index98/Durable_Index]

   MD[is.na(MD)] <- 0
   
  MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  MD[,weighted.mean(Total_Exp_Month,Weight)]
  MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]

  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN198.rda"))
  cat(MD[,weighted.mean(Total_Exp_Month,Weight)])
  
  x<-MD[,.(FoodExpenditure,OriginalFoodExpenditure,FoodExpenditure-OriginalFoodExpenditure)]
}


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  cat("\n")
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN198.rda"))
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  
  #MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]
  #MD[,Home_Per_Metr:=MetrPrice/EqSizeOECD]
  
  #Calculate Per Values
  MD[,EqSizeCalory3 :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  
  MD[,EqSizeCalory :=
       NAge1B	*(Settings$	KCaloryNeed_B1	/Calorie_Need_WorldBank)+
       NAge2B	*(Settings$	KCaloryNeed_B2	/Calorie_Need_WorldBank)+
       NAge3B	*(Settings$	KCaloryNeed_B3	/Calorie_Need_WorldBank)+
       NAge4B	*(Settings$	KCaloryNeed_B4	/Calorie_Need_WorldBank)+
       NAge5B	*(Settings$	KCaloryNeed_B5	/Calorie_Need_WorldBank)+
       NAge6B	*(Settings$	KCaloryNeed_B6	/Calorie_Need_WorldBank)+
       NAge7B	*(Settings$	KCaloryNeed_B7	/Calorie_Need_WorldBank)+
       NAge8B	*(Settings$	KCaloryNeed_B8	/Calorie_Need_WorldBank)+
       NAge9B	*(Settings$	KCaloryNeed_B9	/Calorie_Need_WorldBank)+
       NAge10B	*(Settings$	KCaloryNeed_B10	/Calorie_Need_WorldBank)+
       NAge1G	*(Settings$	KCaloryNeed_G1	/Calorie_Need_WorldBank)+
       NAge2G	*(Settings$	KCaloryNeed_G2	/Calorie_Need_WorldBank)+
       NAge3G	*(Settings$	KCaloryNeed_G3	/Calorie_Need_WorldBank)+
       NAge4G	*(Settings$	KCaloryNeed_G4	/Calorie_Need_WorldBank)+
       NAge5G	*(Settings$	KCaloryNeed_G5	/Calorie_Need_WorldBank)+
       NAge6G	*(Settings$	KCaloryNeed_G6	/Calorie_Need_WorldBank)+
       NAge7G	*(Settings$	KCaloryNeed_G7	/Calorie_Need_WorldBank)+
       NAge8G	*(Settings$	KCaloryNeed_G8	/Calorie_Need_WorldBank)+
       NAge9G	*(Settings$	KCaloryNeed_G9	/Calorie_Need_WorldBank)+
       NAge10G	*(Settings$	KCaloryNeed_G10	/Calorie_Need_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_WorldBank)]
  
  MD[,EqSizeCalory2 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Calorie_Need_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Calorie_Need_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Calorie_Need_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Calorie_Need_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Calorie_Need_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Calorie_Need_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Calorie_Need_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Calorie_Need_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Calorie_Need_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Calorie_Need_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Calorie_Need_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Calorie_Need_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Calorie_Need_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Calorie_Need_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Calorie_Need_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Calorie_Need_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Calorie_Need_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Calorie_Need_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_Anstitoo)]
  
  MD[,EqSizeCalory4 :=
       NAge1B*(Settings$KCaloryNeed_B1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2B*(Settings$KCaloryNeed_B2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3B*(Settings$KCaloryNeed_B3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4B*(Settings$KCaloryNeed_B4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5B*(Settings$KCaloryNeed_B5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6B*(Settings$KCaloryNeed_B6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7B*(Settings$KCaloryNeed_B7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8B*(Settings$KCaloryNeed_B8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9B*(Settings$KCaloryNeed_B9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10B*(Settings$KCaloryNeed_B10/Settings$KCaloryNeed_Adult_WorldBank)+
       NAge1G*(Settings$KCaloryNeed_G1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2G*(Settings$KCaloryNeed_G2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3G*(Settings$KCaloryNeed_G3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4G*(Settings$KCaloryNeed_G4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5G*(Settings$KCaloryNeed_G5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6G*(Settings$KCaloryNeed_G6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7G*(Settings$KCaloryNeed_G7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8G*(Settings$KCaloryNeed_G8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9G*(Settings$KCaloryNeed_G9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10G*(Settings$KCaloryNeed_G10/Settings$KCaloryNeed_Adult_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_WorldBank)]
  
  MD[,EqSizeCalory5 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Settings$KCaloryNeed_Adult_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_Anstitoo)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN298.rda"))
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN298.rda"))
  
  
  #MD[,Relative_Calorie1 :=FoodKCaloriesHH/Calorie_Need1]
  #MD[,Relative_Calorie2 :=FoodKCaloriesHH/Calorie_Need2]
  MD[,OriginalFoodExpenditure_Per :=OriginalFoodExpenditure/EqSizeCalory]
  MD[,FoodKCaloriesHH_Per:=FoodKCaloriesHH/EqSizeCalory]
  MD[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
  
  #Calculate per_Calory from resturants
  MD[,Calory_Price:=(OriginalFoodExpenditure_Per/FoodKCaloriesHH_Per)]
  MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)]
  MD[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
  for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
  MD[,TFoodKCaloriesHH:=FoodKCaloriesHH+ResturantKCalories]
  MD[,TOriginalFoodExpenditure:=OriginalFoodExpenditure+Resturant_Exp]
  
  MD[,TOriginalFoodExpenditure_Per :=TOriginalFoodExpenditure/EqSizeCalory]
  MD[,TFoodKCaloriesHH_Per:=TFoodKCaloriesHH/EqSizeCalory]
  
  ##############################################################
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN398.rda"))
  
  
}




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)