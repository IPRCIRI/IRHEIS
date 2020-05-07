#174-FindInitialPoor.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,ProvinceCode,
               ServiceExp,FoodExpenditure,Total_Exp_Month,
               NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               Durable_Exp,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
  #Real Prices.
  
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea_Name)]
  
  X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
             weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]

  SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2*V2)/V
      ,by=.(Region,NewArea_Name)]
  
  Compare1<-SMD[,.(Old=mean(PriceIndex2),
                  New=mean(PriceIndex)),by=.(Region,NewArea_Name)]

  SMD[,V1:=NULL]
  SMD[,V2:=NULL]
  SMD[,V:=NULL]
  
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  ###Real- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  ###Nominal- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable)]  #Deciling in Country(Nominal)
  SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    SMDIterationPoor[,sum(ThisIterationPoor),by=.(Region,NewArea_Name)][order(Region,NewArea_Name)]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
   
    X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
    X[,V:=V1+V2]
    
    SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
    SMDIterationPoor[,PriceIndex:=NULL]  
    
    Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                      weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2*V2)/V)
        ,by=.(Region,NewArea_Name)]
    Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea_Name)]
    
    
    SMD[,PriceIndex:=NULL]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea_Name"))
    
    SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    
    ###Real- Country
    SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
    SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
    SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
    SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
    
    SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
    
    #cat("\n",sum(SMD[ProvinceCode==2,.N]))
    cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
    SMD[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]
    SMD[,sum(Size*Weight),by=.(Region,Decile)][order(Region,Decile)]
    
    SMD[,weighted.mean(Size,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]

  }

    MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile,Decile_Nominal,Percentile_Nominal)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  
  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea_Name,Region)]
  MD[,sum(Weight*Size), by=.(Decile,Region)][order(Region,Decile)]
  MD[,sum(Weight*Size), by=.(Decile_Nominal,Region)][order(Region,Decile_Nominal)]
  

#############################################
A1<-MD[(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani>0),
      .(A1=weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+
                           Auto1_Irani,Weight)),by=Decile]
  A2<-MD[(TV_Rangi_Irani+TV_Rangi_Khareji>0),
   .(A2=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),by=Decile]
  A3<-MD[freezer2>0 ,.(A3=weighted.mean(freezer2,Weight)),by=Decile]
  A4<-MD[OjaghGaz>0 , .(A4=weighted.mean(OjaghGaz,Weight)),by=Decile]
  A5<-MD[Mashin_Lebasshooyi>0 ,.(A5=weighted.mean(Mashin_Lebasshooyi,Weight)),by=Decile]
  A6<-MD[Mobile>0, .(A6=weighted.mean(Mobile,Weight)),by=Decile]
  A7<-MD[Cooler_Gaz>0,.(A7=weighted.mean(Cooler_Gaz,Weight)),by=Decile]
  A8<-MD[PC>0 ,.(A8= weighted.mean(PC,Weight)),by=Decile]
  A9<-MD[Lastik_Mashin>0,.(A9=weighted.mean(Lastik_Mashin,Weight)),by=Decile]
  A10<-MD[Motor_Machin>0 ,.(A10=weighted.mean(Motor_Machin,Weight)),by=Decile]
  A11<-MD[Tamirat_Asasi>0 ,.(A11=weighted.mean(Tamirat_Asasi,Weight)),by=Decile]
  
  MD<-merge(MD,A1,by="Decile")
  MD<-merge(MD,A2,by="Decile")
  MD<-merge(MD,A3,by="Decile")
  MD<-merge(MD,A4,by="Decile")
  MD<-merge(MD,A5,by="Decile")
  MD<-merge(MD,A6,by="Decile")
  MD<-merge(MD,A7,by="Decile")
  MD<-merge(MD,A8,by="Decile")
  MD<-merge(MD,A9,by="Decile")
  MD<-merge(MD,A10,by="Decile")
  MD<-merge(MD,A11,by="Decile")

MD[car=="True",Added1:=A1]
MD[tvcr=="True",Added2:=A2]
MD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
      Added3:=A3]
MD[oven=="True",Added4:=A4]
MD[washer=="True",Added5:=A5]
MD[cellphone=="True",Added6:=A6]
MD[cooler_gas=="True",Added7:=A7]
MD[computer=="True",Added8:=A8]
MD[car=="True",Added9:=A9]
MD[car=="True",Added10:=A10]
MD[car=="True",Added11:=A11]

x<-MD[,.(HHID,Decile,car,Added1)]

dep <- c( "Auto2_rani", "Auto1_Khareji","Auto2_Khareji", "Auto1_Irani",
          "TV_Rangi_Irani", "TV_Rangi_Khareji","freezer2", "OjaghGaz",
          "Mashin_Lebasshooyi", "Mobile","Cooler_Gaz", "PC",
          "Lastik_Mashin", "Motor_Machin","Tamirat_Asasi")

MD[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
MD[is.na(MD)] <- 0

MD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
        Added7+Added8+Added9+Added10+Added11]

#Calculate Monthly Total Expenditures 
nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp",
        "Amusement_Exp", "Communication_Exp", 
        "HouseandEnergy_Exp", "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", 
        "Transportation_Exp", "Other_Exp"
        ,"Add_to_NonDurable"
        ,"Added"
        #, "Total_Depreciated_Durable"
)
w <- c(nw, "Medical_Exp",
       "Durable_NoDep","Durable_Emergency")


MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]

MD[,weighted.mean(Total_Exp_Month,Weight)]
MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]

MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]

###################################################################

SMD <- MD[,.(HHID,Region,ProvinceCode,
             ServiceExp,FoodExpenditure,Total_Exp_Month,
             NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
             # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
             Durable_Exp,
             TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
             Weight,MetrPrice,Size,EqSizeOECD)]

#Choose one of these
SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]


SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years

#Real Prices.

T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]

SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                   +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
    ,by=.(Region,NewArea_Name)]

X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
            weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
X[,V:=V1+V2]

SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))

SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                  +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2*V2)/V
    ,by=.(Region,NewArea_Name)]

Compare1<-SMD[,.(Old=mean(PriceIndex2),
                 New=mean(PriceIndex)),by=.(Region,NewArea_Name)]

SMD[,V1:=NULL]
SMD[,V2:=NULL]
SMD[,V:=NULL]


SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 

###Real- Country
SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]

###Nominal- Country
SMD<- SMD[order(Total_Exp_Month_Per_nondurable)]  #Deciling in Country(Nominal)
SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]

SMD[,NewPoor:=1]
SMD[,ThisIterationPoor:=0]
i <- 0
while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
  i <- i+1
  SMD[,pold:=Percentile]
  SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
  SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
  SMDIterationPoor[,sum(ThisIterationPoor),by=.(Region,NewArea_Name)][order(Region,NewArea_Name)]
  
  T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  
  X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                           weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]
  
  SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
  SMDIterationPoor[,PriceIndex:=NULL]  
  
  Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                                             weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2*V2)/V)
                            ,by=.(Region,NewArea_Name)]
  Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea_Name)]
  
  
  SMD[,PriceIndex:=NULL]  
  SMD <- merge(SMD,Index,by=c("Region","NewArea_Name"))
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  ###Real- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
  
  #cat("\n",sum(SMD[ProvinceCode==2,.N]))
  cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
  SMD[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]
  SMD[,sum(Size*Weight),by=.(Region,Decile)][order(Region,Decile)]
  
  SMD[,weighted.mean(Size,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
  
}

MD[,Bundle_Value:=NULL]
MD[,NewPoor:=NULL]
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
MD[,Decile_Nominal:=NULL]
MD[,Percentile_Nominal:=NULL]
MD[,A1:=NULL]
MD[,A2:=NULL]
MD[,A3:=NULL]
MD[,A4:=NULL]
MD[,A5:=NULL]
MD[,A6:=NULL]
MD[,A7:=NULL]
MD[,A8:=NULL]
MD[,A9:=NULL]
MD[,A10:=NULL]
MD[,A11:=NULL]
MD[,InitialPoor:=NULL]


MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile,Decile_Nominal,Percentile_Nominal)],by="HHID")
setnames(MD,"NewPoor","InitialPoor")

######################################################################
A1<-MD[(Auto2_rani+Auto1_Khareji+Auto2_Khareji+Auto1_Irani>0),
       .(A1=weighted.mean(Auto2_rani+Auto1_Khareji+Auto2_Khareji+
                            Auto1_Irani,Weight)),by=Decile]
A2<-MD[(TV_Rangi_Irani+TV_Rangi_Khareji>0),
       .(A2=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),by=Decile]
A3<-MD[freezer2>0 ,.(A3=weighted.mean(freezer2,Weight)),by=Decile]
A4<-MD[OjaghGaz>0 , .(A4=weighted.mean(OjaghGaz,Weight)),by=Decile]
A5<-MD[Mashin_Lebasshooyi>0 ,.(A5=weighted.mean(Mashin_Lebasshooyi,Weight)),by=Decile]
A6<-MD[Mobile>0, .(A6=weighted.mean(Mobile,Weight)),by=Decile]
A7<-MD[Cooler_Gaz>0,.(A7=weighted.mean(Cooler_Gaz,Weight)),by=Decile]
A8<-MD[PC>0 ,.(A8= weighted.mean(PC,Weight)),by=Decile]
A9<-MD[Lastik_Mashin>0,.(A9=weighted.mean(Lastik_Mashin,Weight)),by=Decile]
A10<-MD[Motor_Machin>0 ,.(A10=weighted.mean(Motor_Machin,Weight)),by=Decile]
A11<-MD[Tamirat_Asasi>0 ,.(A11=weighted.mean(Tamirat_Asasi,Weight)),by=Decile]

MD<-merge(MD,A1,by="Decile")
MD<-merge(MD,A2,by="Decile")
MD<-merge(MD,A3,by="Decile")
MD<-merge(MD,A4,by="Decile")
MD<-merge(MD,A5,by="Decile")
MD<-merge(MD,A6,by="Decile")
MD<-merge(MD,A7,by="Decile")
MD<-merge(MD,A8,by="Decile")
MD<-merge(MD,A9,by="Decile")
MD<-merge(MD,A10,by="Decile")
MD<-merge(MD,A11,by="Decile")

MD[car=="True",Added1:=A1]
MD[tvcr=="True",Added2:=A2]
MD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
   Added3:=A3]
MD[oven=="True",Added4:=A4]
MD[washer=="True",Added5:=A5]
MD[cellphone=="True",Added6:=A6]
MD[cooler_gas=="True",Added7:=A7]
MD[computer=="True",Added8:=A8]
MD[car=="True",Added9:=A9]
MD[car=="True",Added10:=A10]
MD[car=="True",Added11:=A11]

x<-MD[,.(HHID,Decile,car,Added1)]

dep <- c( "Auto2_rani", "Auto1_Khareji","Auto2_Khareji", "Auto1_Irani",
          "TV_Rangi_Irani", "TV_Rangi_Khareji","freezer2", "OjaghGaz",
          "Mashin_Lebasshooyi", "Mobile","Cooler_Gaz", "PC",
          "Lastik_Mashin", "Motor_Machin","Tamirat_Asasi")

MD[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
MD[is.na(MD)] <- 0

MD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
     Added7+Added8+Added9+Added10+Added11]

#Calculate Monthly Total Expenditures 
nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp",
        "Amusement_Exp", "Communication_Exp", 
        "HouseandEnergy_Exp", "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", 
        "Transportation_Exp", "Other_Exp"
        ,"Add_to_NonDurable"
        ,"Added"
        #, "Total_Depreciated_Durable"
)
w <- c(nw, "Medical_Exp",
       "Durable_NoDep","Durable_Emergency")


MD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
MD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]

MD[,weighted.mean(Total_Exp_Month,Weight)]
MD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]

MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]

###################################################################

SMD <- MD[,.(HHID,Region,ProvinceCode,
             ServiceExp,FoodExpenditure,Total_Exp_Month,
             NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
             # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
             Durable_Exp,
             TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
             Weight,MetrPrice,Size,EqSizeOECD)]

#Choose one of these
SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]


SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years

#Real Prices.

T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]

SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                   +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
    ,by=.(Region,NewArea_Name)]

X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
            weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
X[,V:=V1+V2]

SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))

SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                  +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2*V2)/V
    ,by=.(Region,NewArea_Name)]

Compare1<-SMD[,.(Old=mean(PriceIndex2),
                 New=mean(PriceIndex)),by=.(Region,NewArea_Name)]

SMD[,V1:=NULL]
SMD[,V2:=NULL]
SMD[,V:=NULL]


SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 

###Real- Country
SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]

###Nominal- Country
SMD<- SMD[order(Total_Exp_Month_Per_nondurable)]  #Deciling in Country(Nominal)
SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]

SMD[,NewPoor:=1]
SMD[,ThisIterationPoor:=0]
i <- 0
while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
  i <- i+1
  SMD[,pold:=Percentile]
  SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
  SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
  SMDIterationPoor[,sum(ThisIterationPoor),by=.(Region,NewArea_Name)][order(Region,NewArea_Name)]
  
  T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  
  X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                           weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]
  
  SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
  SMDIterationPoor[,PriceIndex:=NULL]  
  
  Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                                             weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2*V2)/V)
                            ,by=.(Region,NewArea_Name)]
  Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea_Name)]
  
  
  SMD[,PriceIndex:=NULL]  
  SMD <- merge(SMD,Index,by=c("Region","NewArea_Name"))
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  ###Real- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
  
  #cat("\n",sum(SMD[ProvinceCode==2,.N]))
  cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
  SMD[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]
  SMD[,sum(Size*Weight),by=.(Region,Decile)][order(Region,Decile)]
  
  SMD[,weighted.mean(Size,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
  
}

MD[,Bundle_Value:=NULL]
MD[,NewPoor:=NULL]
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
MD[,Decile_Nominal:=NULL]
MD[,Percentile_Nominal:=NULL]
MD[,InitialPoor:=NULL]


MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile,Decile_Nominal,Percentile_Nominal)],by="HHID")
setnames(MD,"NewPoor","InitialPoor")

######################################################################
save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
