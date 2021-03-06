#56- Poverty line Predictions (Fall 1397)
# 
# Copyright � 2019: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Merge Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)
library(spatstat)

year<-96
cat(paste0("\n------------------------------\nYear:",year,"\n"))

#load Demos+FoodPrices+Weights
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))   
HHWeights<- as.data.table(HHWeights)
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
MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
FData[,Size:=NULL]
MD<-merge(MD,FData ,by =c("HHID"),all=TRUE)
MD<-merge(MD,HHWeights ,by =c("HHID"),all=TRUE)
MD<-merge(MD,FoodData,by =c("HHID"),all=TRUE)
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
MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
MD<-merge(MD,DurableData,by =c("HHID"),all=TRUE)
MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
#MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
for (col in c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
              "Communication_Exp", "EducExpenditure", "Energy_Exp", 
              "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", "Transportation_Exp",
              "Other_Exp", "ServiceExp", "Medical_Exp", "Durable_Exp", 
              "Resturant_Exp")) 
  MD[is.na(get(col)), (col) := 0]

#Realize Expenditures
load(file="PriceIndex9697.rda")
MD<-merge(MD,PriceIndex9697,by =c("ProvinceCode"),all.x=TRUE)

MD$FoodExpenditure<-MD$FoodExpenditure*MD$fall971food_index/MD$total961food_index
MD$Cigar_Exp<-MD$Cigar_Exp*MD$fall971cigar_index/MD$total961cigar_index
MD$Cloth_Exp<-MD$Cloth_Exp*MD$fall971cloth_index/MD$total961cloth_index
MD$Amusement_Exp<-MD$Amusement_Exp*MD$fall971amusement_index/MD$total961amusement_index
MD$Communication_Exp<-MD$Communication_Exp*MD$fall971communication_index/MD$total961communication_index
#MD$EducExpenditure<-MD$EducExpenditure*MD$fall971education_index/MD$total961education_index
MD$Energy_Exp<-MD$Energy_Exp*MD$fall971house_energy_index/MD$total961house_energy_index
MD$Furniture_Exp<-MD$Furniture_Exp*MD$fall971furniture_index/MD$total961furniture_index
MD$Hotel_Exp<-MD$Hotel_Exp*MD$fall971hotel_index/MD$total961hotel_index
MD$Behdasht_Exp<-MD$Behdasht_Exp*MD$fall971behdasht_index/MD$total961behdasht_index
MD$Transportation_Exp<-MD$Transportation_Exp*MD$fall971transportation_index/MD$total961transportation_index
MD$Other_Exp<-MD$Other_Exp*MD$fall971other_index/MD$total961other_index
MD$ServiceExp<-MD$ServiceExp*MD$fall971house_energy_index/MD$total961house_energy_index
MD$Medical_Exp<-MD$Medical_Exp*MD$fall971behdasht_index/MD$total961behdasht_index
MD$Durable_Exp<-MD$Durable_Exp*MD$azar97/MD$total96
MD$Resturant_Exp<-MD$Resturant_Exp*MD$fall971hotel_index/MD$total961hotel_index

#Calculate Monthly Total Expenditures 
nw <- c("FoodExpenditure", "Cigar_Exp", "Cloth_Exp",
        "Amusement_Exp", "Communication_Exp",
        "Energy_Exp", "Furniture_Exp", "Hotel_Exp", "Behdasht_Exp", 
        "Transportation_Exp", "Other_Exp", "ServiceExp")
w <- c(nw, "Medical_Exp", "Durable_Exp")

MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]

MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
MD<-MD[Size!=0 & FoodExpenditure!=0 & !is.na(FoodKCalories)]

#Calculate Per Values
MD[,EqSizeCalory :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
MD[,FoodExpenditure_Per :=FoodExpenditure/EqSizeCalory]
MD[,FoodKCalories_Per:=FoodKCalories/EqSizeCalory]

#Calculate per_Calory from resturants
MD[,Calory_Price:=(FoodExpenditure_Per/FoodKCalories_Per)]
MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)][order(Calory_Price)]
MD[,ResturantKCalories:=(Settings$OutFoodKCXShare*Resturant_Exp)/Calory_Price_Area]
for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
MD[,TFoodKCalories:=FoodKCalories+ResturantKCalories]
MD[,TFoodExpenditure:=FoodExpenditure+(Settings$OutFoodKCXShare*Resturant_Exp)]

MD[,TFoodExpenditure_Per :=TFoodExpenditure/EqSizeCalory]
MD[,TFoodKCalories_Per:=TFoodKCalories/EqSizeCalory]

SMD <- MD[,.(HHID,Region,NewArea,Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,TFoodKCalories_Per,
             Weight,MetrPrice,Size,EqSizeOECD)]

SMD[,Bundle_Value:=TFoodExpenditure_Per*Settings$KCaloryNeed_Adult/TFoodKCalories_Per]

SMD <- SMD[Bundle_Value<=5000000 | TFoodKCalories_Per>=300]

#Real Prices
T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]

SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                  +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
    ,by=.(Region,NewArea)]

SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 

SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
SMD[,crw:=cumsum(Weight)/sum(Weight),by=Region]  # Cumulative Relative Weight

#Calculate deciles by weights
SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]

FirstSMD<-SMD[,.(HHID,Region,NewArea,Percentile,Decile)]
FirstSMD<-FirstSMD[,Realfirstpoor:=ifelse(Decile %in% 1:2,1,0)]

SMD[,NewPoor:=1]
SMD[,ThisIterationPoor:=0]
i <- 0
while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=30 & i <=50){
  i <- i+1
  SMD[,pold:=Percentile]
  SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
  SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
  
  T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  Index <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
                                              weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2)/2)
                            ,by=.(Region,NewArea)]
  
  SMD[,PriceIndex:=NULL]
  SMD<- SMD[order(Region,NewArea)]  
  SMD <- merge(SMD,Index,by=c("Region","NewArea"))
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
  SMD[,crw:=cumsum(Weight)/sum(Weight),by=Region]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]

  
  cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
}
MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile)],by="HHID")
setnames(MD,"NewPoor","InitialPoor")

MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea,Region)]
save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

load(file="dt2Urban.rda")
load(file="dt2Rural.rda")
dt2total<-rbind(dt2Urban,dt2Rural)

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
dt2total <- dt2total[,lapply(.SD,sum),by=.(Region, NewArea,NewArea_Name,cluster3)]
dt2total[,HHID:=NULL]
MD<-merge(MD,dt2total,by=c("NewArea","Region","NewArea_Name"),all.x=TRUE)

#Determine Food (Equal 2100 KCal) Bundle
MD[,NewPoor:=InitialPoor]
MD[,OldPoor:=1]

i <- 0
while(MD[(NewPoor-OldPoor)!=0,.N]>5  & i <=15){
  i <- i + 1
  MD[,ThisIterationPoor:=NewPoor]
  MD[,FPLine:=NULL]    
  MDP <- MD[ThisIterationPoor==1,
            .(FPLine=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)),
            by=.(cluster3,Region)]
  MD <- merge(MD,MDP,by=c("Region","cluster3"))
  #    print(MDP)
  x<-MD[,.(cluster3,Region,FPLine,InitialPoor)]
  MD[,NewPoor:=ifelse(TFoodExpenditure_Per < FPLine,1,0)]
  print(table(MD[,.(ThisIterationPoor,NewPoor)]))
  MD[,OldPoor:=ThisIterationPoor]
}

MD[,FinalFoodPoor:=OldPoor]
MD <- MD[,.(HHID,Region,NewArea,cluster3,ProvinceCode,Size,HAge,HSex,
            HLiterate,HEduLevel0,HActivityState,Area,Rooms,MetrPrice,
            Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,
            FoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor,
            TFoodExpenditure,Total_Exp_Month_nondurable,Total_Exp_Month,
            Total_Exp_Month_Per)]

EngleD <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine,
             .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
               FPLine=mean(FPLine)),by=.(Region,cluster3)]
EngleD[,PovertyLine:=FPLine/Engel]
MD <- merge(MD,EngleD[,.(cluster3,Region,PovertyLine,Engel)],by=c("Region","cluster3"))
MD<-MD[Region=="Rural" ]
MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
    MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
    MD[,weighted.mean(Engel,Weight*Size)],"\t",
    MD[,weighted.mean(FPLine,Weight*Size)])
#MD[,weighted.mean(FinalPoor,Weight*Size),by=.(Region,NewArea)][order(V1)]
MD[,weighted.mean(FinalPoor,Weight),by=c("Region","cluster3")]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)