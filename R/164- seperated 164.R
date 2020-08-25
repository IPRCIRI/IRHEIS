#164-Step 4-FindInitialPoor.R
# 
# Copyright © 2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)
library(compare)

# Function Defs ---------------------------------------------------------------------------------
CalcTornqvistIndex <- function(DataTable){
  X <- DataTable[,.(N=.N,wi1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
                    wi2=weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE),
                    pi1=weighted.mean(Bundle_Value,Weight,na.rm = TRUE),
                    pi2=weighted.mean(MetrPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
  
  X[,wi:=wi1+wi2]
  X[,wi1:=wi1/wi]
  X[,wi2:=wi2/wi]
  XTeh<-X[NewArea_Name=="Sh_Tehran"]
  wk1<-XTeh$wi1   # k == Sh_Tehran
  wk2<-XTeh$wi2
  pk1<-XTeh$pi1
  pk2<-XTeh$pi2
  
  X[,SimpleIndex:= .5 * pi1/pk1 + .5 * pi2/pk2]
  X[,AnotherIndex:= wi1 * pi1/pk1 + wi2 * pi2/pk2]
  
  X[,TornqvistIndex:= exp(   (wk1+wi1)/2 * log(pi1/pk1) + (wk2+wi2)/2 * log(pi2/pk2)  )      ]
  
  
  # print(X[Region=="Rural" & NewArea_Name=="Semnan",])
  # if("Percentile" %in% names(DataTable))
  #   print(DataTable[Region=="Rural" & NewArea_Name=="Semnan",.(min=min(as.integer(Percentile)),max=max(as.integer(Percentile))),by=.(Region,NewArea_Name)])
  
  return(X[,.(Region,NewArea_Name,PriceIndex=TornqvistIndex)])
}


DoDeciling_SetInitialPoor <- function(DataTable,PriceIndexDT){
  
  if("PriceIndex" %in% names(DataTable)){
    DataTable <- DataTable[,PriceIndex:=NULL]
  }
  DataTable <- merge(DataTable,PriceIndexDT,by=c("Region","NewArea_Name"))
  
  
  DataTable <- DataTable[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]
  DataTable <- DataTable[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
  DataTable <- DataTable[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
  
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  DataTable <- DataTable[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  DataTable <- DataTable[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  DataTable <- DataTable[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  #############################################
  A1<-DataTable[(`71111`+`71112`+`71116`+`71117`>0),
                .(A1=weighted.mean(`71111`+`71112`+`71116`+
                                     `71117`,Weight)),by=Decile]
  A2<-DataTable[(`91128`+`91129`>0),
                .(A2=weighted.mean(`91128`+`91129`,Weight)),by=Decile]
  A3<-DataTable[`53112`>0 ,.(A3=weighted.mean(`53112`,Weight)),by=Decile]
  A4<-DataTable[`53116`>0 , .(A4=weighted.mean(`53116`,Weight)),by=Decile]
  A5<-DataTable[`53113`>0 ,.(A5=weighted.mean(`53113`,Weight)),by=Decile]
  A6<-DataTable[`82113`>0, .(A6=weighted.mean(`82113`,Weight)),by=Decile]
  A7<-DataTable[`53125`>0,.(A7=weighted.mean(`53125`,Weight)),by=Decile]
  A8<-DataTable[`91311`>0 ,.(A8= weighted.mean(`91311`,Weight)),by=Decile]
  A9<-DataTable[`72111`>0,.(A9=weighted.mean(`72111`,Weight)),by=Decile]
  if (year!=90 & year!=92 & year!=93 & year!=95){
    A10<-DataTable[`72118`>0 ,.(A10=weighted.mean(`72118`,Weight)),by=Decile]
  }
  A11<-DataTable[`72319`>0 ,.(A11=weighted.mean(`72319`,Weight)),by=Decile]
  
  DataTable[,A1:=NULL]
  DataTable[,A2:=NULL]
  DataTable[,A3:=NULL]
  DataTable[,A4:=NULL]
  DataTable[,A5:=NULL]
  DataTable[,A6:=NULL]
  DataTable[,A7:=NULL]
  DataTable[,A8:=NULL]
  DataTable[,A9:=NULL]
  DataTable[,A10:=NULL]
  DataTable[,A11:=NULL]
  
  DataTable<-merge(DataTable,A1,by="Decile")
  DataTable<-merge(DataTable,A2,by="Decile")
  DataTable<-merge(DataTable,A3,by="Decile")
  DataTable<-merge(DataTable,A4,by="Decile")
  DataTable<-merge(DataTable,A5,by="Decile")
  DataTable<-merge(DataTable,A6,by="Decile")
  DataTable<-merge(DataTable,A7,by="Decile")
  DataTable<-merge(DataTable,A8,by="Decile")
  DataTable<-merge(DataTable,A9,by="Decile")
  if (year!=90 & year!=92 & year!=93 & year!=95){
    DataTable<-merge(DataTable,A10,by="Decile")
  }
  DataTable<-merge(DataTable,A11,by="Decile")
  
  DataTable[car=="True",Added1:=A1-0.05*Auto_Sale] ### We use 0.05 instead of 0.1
  DataTable[tvcr=="True",Added2:=A2-0.033*TV_Sale]
  DataTable[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
            Added3:=A3-0.033*yakhchal_Sale]
  DataTable[oven=="True",Added4:=A4-0.033*ojaghgaz_Sale]
  DataTable[washer=="True",Added5:=A5-0.033*lebasshooyi_Sale]
  DataTable[cellphone=="True",Added6:=A6-0.11*Mobile_Sale]
  DataTable[cooler_gas=="True",Added7:=A7-0.05*Coolergazi_Sale]
  DataTable[computer=="True",Added8:=A8-0.06*PC_Sale]
  DataTable[car=="True",Added9:=A9-0.5*lastik_Sale]
  if (year!=90 & year!=92 & year!=93 & year!=95){
    DataTable[car=="True",Added10:=A10]
  }
  DataTable[car=="True",Added11:=A11]
  
  if (year!=90 & year!=92 & year!=93 & year!=95){ 
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111", "72118","72319")
  }
  
  if (year==90 | year==92 | year==93 | year==95){
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111","72319")
  }
  
  DataTable[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  DataTable[is.na(DataTable)] <- 0
  
  if (year!=90 & year!=92 & year!=93 & year!=95){
    DataTable[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
                Added7+Added8+Added9+Added10+Added11]
  }
  if (year==90 | year==92 | year==93 | year==95){
    DataTable[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
                Added7+Added8+Added9+Added11]
  }
  
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
  
  
  DataTable[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  DataTable[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  DataTable[,weighted.mean(Total_Exp_Month,Weight)]
  DataTable[,weighted.mean(Total_Exp_Month_nondurable,Weight)]
  
  DataTable[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  DataTable[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  ###################################################################
  
  DataTable <- DataTable[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]
  DataTable <- DataTable[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
  DataTable <- DataTable[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
  
  DataTable <- DataTable[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  DataTable <- DataTable[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  DataTable <- DataTable[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  DataTable <- DataTable[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  
  DataTable <- DataTable[,InitialPoorBasedOnPercentile:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  
  return(DataTable)
}


#year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  if (year!=90 & year!=92 & year!=93 & year!=95){ 
    SMD <- MD[,.(HHID,Region,
                 ServiceExp,FoodExpenditure,Total_Exp_Month,
                 NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
                 # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
                 TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
                 Weight,MetrPrice,Size,EqSizeOECD
                 ,`71111`,`71117`,`71112`,`71116`,
                 `91128`,`91129`,`53112`,`53116`,
                 `53113`,`82113`,`53125`,`91311`,
                 `72111`,`72118`,`72319`
                 ,Auto_Sale,TV_Sale,Mobile_Sale,PC_Sale,
                 yakhchal_Sale,ojaghgaz_Sale,lebasshooyi_Sale,
                 Coolergazi_Sale,lastik_Sale
                 ,car,tvcr,freezer,frez_refrig,refrigerator,oven,
                 washer,cellphone,cooler_gas,computer
                 ,OriginalFoodExpenditure,FoodOtherExpenditure, Cigar_Exp, Cloth_Exp,
                 Amusement_Exp, Communication_Exp, 
                 HouseandEnergy_Exp, Furniture_Exp, HotelRestaurant_Exp, Hygiene_Exp, 
                 Transportation_Exp, Other_Exp
                 ,Add_to_NonDurable,Medical_Exp,
                 Durable_NoDep,Durable_Emergency)]
  }
  if (year==90 | year==92 | year==93 | year==95){
    SMD <- MD[,.(HHID,Region,
                 ServiceExp,FoodExpenditure,Total_Exp_Month,
                 NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
                 # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
                 TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
                 Weight,MetrPrice,Size,EqSizeOECD
                 ,`71111`,`71117`,`71112`,`71116`,
                 `91128`,`91129`,`53112`,`53116`,
                 `53113`,`82113`,`53125`,`91311`,
                 `72111`,`72319`
                 ,Auto_Sale,TV_Sale,Mobile_Sale,PC_Sale,
                 yakhchal_Sale,ojaghgaz_Sale,lebasshooyi_Sale,
                 Coolergazi_Sale,lastik_Sale
                 ,car,tvcr,freezer,frez_refrig,refrigerator,oven,
                 washer,cellphone,cooler_gas,computer
                 ,OriginalFoodExpenditure,FoodOtherExpenditure, Cigar_Exp, Cloth_Exp,
                 Amusement_Exp, Communication_Exp, 
                 HouseandEnergy_Exp, Furniture_Exp, HotelRestaurant_Exp, Hygiene_Exp, 
                 Transportation_Exp, Other_Exp
                 ,Add_to_NonDurable,Medical_Exp,
                 Durable_NoDep,Durable_Emergency)]
  }
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
 # S1<-SMD[,.(HHID,Region,NewArea_Name,TFoodKCaloriesHH_Per,Bundle_Value)]
  
  
  #PriceDT <- CalcTornqvistIndex(SMD)
  X <- SMD[,.(N=.N,wi1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
                    wi2=weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE),
                    pi1=weighted.mean(Bundle_Value,Weight,na.rm = TRUE),
                    pi2=weighted.mean(MetrPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
  
  X[,wi:=wi1+wi2]
  X[,wi1:=wi1/wi]
  X[,wi2:=wi2/wi]
  XTeh<-X[NewArea_Name=="Sh_Tehran"]
  wk1<-XTeh$wi1   # k == Sh_Tehran
  wk2<-XTeh$wi2
  pk1<-XTeh$pi1
  pk2<-XTeh$pi2
  
  X[,SimpleIndex:= .5 * pi1/pk1 + .5 * pi2/pk2]
  X[,AnotherIndex:= wi1 * pi1/pk1 + wi2 * pi2/pk2]
  
  X[,TornqvistIndex:= exp(   (wk1+wi1)/2 * log(pi1/pk1) + (wk2+wi2)/2 * log(pi2/pk2)  )      ]
  
  PriceDT <-X[,.(Region,NewArea_Name,PriceIndex=TornqvistIndex)]
  
  

  if("PriceIndex" %in% names(SMD)){
    SMD <- SMD[,PriceIndex:=NULL]
  }
  SMD <- merge(SMD,PriceDT,by=c("Region","NewArea_Name"))
  
  
  SMD <- SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]
  SMD <- SMD[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
  SMD <- SMD[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
  
  SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  SMD <- SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD <- SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD <- SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  #############################################
  A1<-SMD[(`71111`+`71112`+`71116`+`71117`>0),
                .(A1=weighted.mean(`71111`+`71112`+`71116`+
                                     `71117`,Weight)),by="Decile"]
  A2<-SMD[(`91128`+`91129`>0),
                .(A2=weighted.mean(`91128`+`91129`,Weight)),by="Decile"]
  A3<-SMD[`53112`>0 ,.(A3=weighted.mean(`53112`,Weight)),by="Decile"]
  A4<-SMD[`53116`>0 , .(A4=weighted.mean(`53116`,Weight)),by="Decile"]
  A5<-SMD[`53113`>0 ,.(A5=weighted.mean(`53113`,Weight)),by="Decile"]
  A6<-SMD[`82113`>0, .(A6=weighted.mean(`82113`,Weight)),by="Decile"]
  A7<-SMD[`53125`>0,.(A7=weighted.mean(`53125`,Weight)),by="Decile"]
  A8<-SMD[`91311`>0 ,.(A8= weighted.mean(`91311`,Weight)),by="Decile"]
  A9<-SMD[`72111`>0,.(A9=weighted.mean(`72111`,Weight)),by="Decile"]
  if (year!=90 & year!=92 & year!=93 & year!=95){
    A10<-SMD[`72118`>0 ,.(A10=weighted.mean(`72118`,Weight)),by="Decile"]
  }
  A11<-SMD[`72319`>0 ,.(A11=weighted.mean(`72319`,Weight)),by="Decile"]
  
  #SMD[,A1:=NULL]
  #SMD[,A2:=NULL]
  #SMD[,A3:=NULL]
  #SMD[,A4:=NULL]
  #SMD[,A5:=NULL]
  #SMD[,A6:=NULL]
  #SMD[,A7:=NULL]
  #SMD[,A8:=NULL]
  #SMD[,A9:=NULL]
  #SMD[,A10:=NULL]
  #SMD[,A11:=NULL]
  
  SMD<-merge(SMD,A1,by="Decile")
  SMD<-merge(SMD,A2,by="Decile")
  SMD<-merge(SMD,A3,by="Decile")
  SMD<-merge(SMD,A4,by="Decile")
  SMD<-merge(SMD,A5,by="Decile")
  SMD<-merge(SMD,A6,by="Decile")
  SMD<-merge(SMD,A7,by="Decile")
  if (year==98){
    A88<-data.table("3","200000")
    names(A88)<-c("Decile","A8")
    A8 <- rbind(A8, A88)
  }
   SMD<-merge(SMD,A8,by="Decile")
   SMD[,A8:=as.numeric(A8)]
  SMD<-merge(SMD,A9,by="Decile")
  
  if (year!=90 & year!=92 & year!=93 & year!=95){
    SMD<-merge(SMD,A10,by="Decile")
  }
  SMD<-merge(SMD,A11,by="Decile")
  

  
 # SMD[car=="True",Added1:=A1] ### We use 0.05 instead of 0.1
 # SMD[tvcr=="True",Added2:=A2]
 # SMD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
 #           Added3:=A3]
 # SMD[oven=="True",Added4:=A4]
#  SMD[washer=="True",Added5:=A5]
 # SMD[cellphone=="True",Added6:=A6]
 # SMD[cooler_gas=="True",Added7:=A7]
 # SMD[computer=="True",Added8:=A8]
 # SMD[car=="True",Added9:=A9]
 # if (year!=90 & year!=92 & year!=93 & year!=95){
 #   SMD[car=="True",Added10:=A10]
 # }
 # SMD[car=="True",Added11:=A11]
  
  SMD[car=="True",Added1:=A1-0.05*Auto_Sale]  #We use 0.05 instead of 0.1
  SMD[tvcr=="True",Added2:=A2-0.033*TV_Sale]
  SMD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
            Added3:=A3-0.033*yakhchal_Sale]
  SMD[oven=="True",Added4:=A4-0.033*ojaghgaz_Sale]
  SMD[washer=="True",Added5:=A5-0.033*lebasshooyi_Sale]
  SMD[cellphone=="True",Added6:=A6-0.11*Mobile_Sale]
  SMD[cooler_gas=="True",Added7:=A7-0.05*Coolergazi_Sale]
  SMD[computer=="True",Added8:=A8]
  SMD[computer=="True",Added8:=A8-0.06*PC_Sale]
  SMD[car=="True",Added9:=A9-0.5*lastik_Sale]
  if (year!=90 & year!=92 & year!=93 & year!=95){
    SMD[car=="True",Added10:=A10]
  }
  SMD[car=="True",Added11:=A11]
  
 # z1<-SMD[,.(HHID,Decile,Added1,Added2,Added3,Added4,Added5,
  #          Added6,Added7,Added8,Added9,Added10,Added11)]
  
SMD[Added1<0,Added1:=0]
SMD[Added2<0,Added2:=0]
SMD[Added3<0,Added3:=0]
SMD[Added4<0,Added4:=0]
SMD[Added5<0,Added5:=0]
SMD[Added6<0,Added6:=0]
SMD[Added7<0,Added7:=0]
SMD[Added8<0,Added8:=0]
SMD[Added9<0,Added9:=0]
if (year!=90 & year!=92 & year!=93 & year!=95){
SMD[Added10<0,Added10:=0]
}
SMD[Added11<0,Added11:=0]

 #z1<-SMD[,.(HHID,Decile,Added1,Added2,Added3,Added4,Added5,
 #         Added6,Added7,Added8,Added9,Added10,Added11)]
  
  if (year!=90 & year!=92 & year!=93 & year!=95){ 
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111", "72118","72319")
  }
  
  if (year==90 | year==92 | year==93 | year==95){
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111","72319")
  }
  
  SMD[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  SMD[is.na(SMD)] <- 0
  
  if (year!=90 & year!=92 & year!=93 & year!=95){
    SMD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
                Added7+Added8+Added9+Added10+Added11]
  }
  if (year==90 | year==92 | year==93 | year==95){
    SMD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
                Added7+Added8+Added9+Added11]
  }
  
 # z2<-SMD[,.(HHID,Decile,Added,Added1,Added2,Added3,Added4,Added5,
 #          Added6,Added7,Added8,Added9,Added10,Added11)]
  
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
  
  
  SMD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
  SMD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
  
  SMD[,weighted.mean(Total_Exp_Month,Weight)]
  SMD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]
  
  SMD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
  SMD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
  
  ###################################################################
  
  SMD <- SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]
  SMD <- SMD[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
  SMD <- SMD[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
  
  SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  SMD <- SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD <- SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD <- SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  
  SMD <- SMD[,InitialPoorBasedOnPercentile:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  
  
  #S2<-SMD[,.(HHID,Region,NewArea_Name,TFoodKCaloriesHH_Per,Bundle_Value)]
  
  
  #comparison <- compare(S1,S2,"row")
  #comparison <- setdiff(S1$HHID,S2$HHID)
  #x<-S1[HHID %in% comparison]
  
  SMD[,InitialPoorBasedOnPercentileLastIteration:=1]
  i <- 0
  while(SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)]>0.001*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,InitialPoorBasedOnPercentileLastIteration:=InitialPoorBasedOnPercentile]
    
    SMDIterationPoor <- SMD[InitialPoorBasedOnPercentileLastIteration==1 | First25==1 ]
    
    if(nrow(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)])<78)
      stop("HERE Some Area goes missing!")
    if(min(SMDIterationPoor[,.N,by=.(Region,NewArea_Name)]$N)==0)
      stop("HERE Some Area goes missing!")
    
    #    PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)
    X <- SMDIterationPoor[,.(N=.N,wi1=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
                      wi2=weighted.mean(ServiceExp/Total_Exp_Month,Weight,na.rm = TRUE),
                      pi1=weighted.mean(Bundle_Value,Weight,na.rm = TRUE),
                      pi2=weighted.mean(MetrPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
    
    X[,wi:=wi1+wi2]
    X[,wi1:=wi1/wi]
    X[,wi2:=wi2/wi]
    XTeh<-X[NewArea_Name=="Sh_Tehran"]
    wk1<-XTeh$wi1   # k == Sh_Tehran
    wk2<-XTeh$wi2
    pk1<-XTeh$pi1
    pk2<-XTeh$pi2
    
    X[,SimpleIndex:= .5 * pi1/pk1 + .5 * pi2/pk2]
    X[,AnotherIndex:= wi1 * pi1/pk1 + wi2 * pi2/pk2]
    
    X[,TornqvistIndex:= exp(   (wk1+wi1)/2 * log(pi1/pk1) + (wk2+wi2)/2 * log(pi2/pk2)  )      ]
    
    PriceDTBasedOnThisIterationPoor<-X[,.(Region,NewArea_Name,PriceIndex=TornqvistIndex)]
    

    #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
    #SMD <- DoDeciling_SetInitialPoor(SMD,PriceDTBasedOnThisIterationPoor)
    
    if("PriceIndex" %in% names(SMD)){
      SMD <- SMD[,PriceIndex:=NULL]
    }
    SMD <- merge(SMD,PriceDTBasedOnThisIterationPoor,by=c("Region","NewArea_Name"))
    
    
    SMD <- SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    
    SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]
    SMD <- SMD[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
    SMD <- SMD[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
    
    SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
    SMD <- SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
    
    #Calculate deciles by weights
    SMD <- SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
    SMD <- SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
    
    #############################################
    A1<-SMD[(`71111`+`71112`+`71116`+`71117`>0),
            .(A1=weighted.mean(`71111`+`71112`+`71116`+
                                 `71117`,Weight)),by="Decile"]
    A2<-SMD[(`91128`+`91129`>0),
            .(A2=weighted.mean(`91128`+`91129`,Weight)),by="Decile"]
    A3<-SMD[`53112`>0 ,.(A3=weighted.mean(`53112`,Weight)),by="Decile"]
    A4<-SMD[`53116`>0 , .(A4=weighted.mean(`53116`,Weight)),by="Decile"]
    A5<-SMD[`53113`>0 ,.(A5=weighted.mean(`53113`,Weight)),by="Decile"]
    A6<-SMD[`82113`>0, .(A6=weighted.mean(`82113`,Weight)),by="Decile"]
    A7<-SMD[`53125`>0,.(A7=weighted.mean(`53125`,Weight)),by="Decile"]
    A8<-SMD[`91311`>0 ,.(A8= weighted.mean(`91311`,Weight)),by="Decile"]
    A9<-SMD[`72111`>0,.(A9=weighted.mean(`72111`,Weight)),by="Decile"]
    if (year!=90 & year!=92 & year!=93 & year!=95){
      A10<-SMD[`72118`>0 ,.(A10=weighted.mean(`72118`,Weight)),by="Decile"]
    }
    A11<-SMD[`72319`>0 ,.(A11=weighted.mean(`72319`,Weight)),by="Decile"]
    
    SMD[,A1:=NULL]
    SMD[,A2:=NULL]
    SMD[,A3:=NULL]
    SMD[,A4:=NULL]
    SMD[,A5:=NULL]
    SMD[,A6:=NULL]
    SMD[,A7:=NULL]
    SMD[,A8:=NULL]
    SMD[,A9:=NULL]
    SMD[,A10:=NULL]
    SMD[,A11:=NULL]
    
    SMD<-merge(SMD,A1,by="Decile")
    SMD<-merge(SMD,A2,by="Decile")
    SMD<-merge(SMD,A3,by="Decile")
    SMD<-merge(SMD,A4,by="Decile")
    SMD<-merge(SMD,A5,by="Decile")
    SMD<-merge(SMD,A6,by="Decile")
    SMD<-merge(SMD,A7,by="Decile")
   # if (year==98){
  #    A88<-data.table("3","200000")
  #    names(A88)<-c("Decile","A8")
  #    A8 <- rbind(A8, A88)
  #  }
    SMD<-merge(SMD,A8,by="Decile")
    SMD[,A8:=as.numeric(A8)]
    SMD<-merge(SMD,A9,by="Decile")
    
    if (year!=90 & year!=92 & year!=93 & year!=95){
      SMD<-merge(SMD,A10,by="Decile")
    }
    SMD<-merge(SMD,A11,by="Decile")
    
    
    
    # SMD[car=="True",Added1:=A1] ### We use 0.05 instead of 0.1
    # SMD[tvcr=="True",Added2:=A2]
    # SMD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
    #           Added3:=A3]
    # SMD[oven=="True",Added4:=A4]
    #  SMD[washer=="True",Added5:=A5]
    # SMD[cellphone=="True",Added6:=A6]
    # SMD[cooler_gas=="True",Added7:=A7]
    # SMD[computer=="True",Added8:=A8]
    # SMD[car=="True",Added9:=A9]
    # if (year!=90 & year!=92 & year!=93 & year!=95){
    #   SMD[car=="True",Added10:=A10]
    # }
    # SMD[car=="True",Added11:=A11]
    
    SMD[car=="True",Added1:=A1-0.05*Auto_Sale]  #We use 0.05 instead of 0.1
    SMD[tvcr=="True",Added2:=A2-0.033*TV_Sale]
    SMD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
        Added3:=A3-0.033*yakhchal_Sale]
    SMD[oven=="True",Added4:=A4-0.033*ojaghgaz_Sale]
    SMD[washer=="True",Added5:=A5-0.033*lebasshooyi_Sale]
    SMD[cellphone=="True",Added6:=A6-0.11*Mobile_Sale]
    SMD[cooler_gas=="True",Added7:=A7-0.05*Coolergazi_Sale]
    SMD[computer=="True",Added8:=A8]
    SMD[computer=="True",Added8:=A8-0.06*PC_Sale]
    SMD[car=="True",Added9:=A9-0.5*lastik_Sale]
    if (year!=90 & year!=92 & year!=93 & year!=95){
      SMD[car=="True",Added10:=A10]
    }
    SMD[car=="True",Added11:=A11]
    
  #  z1<-SMD[,.(HHID,Added1,Added2,Added3,Added4,Added5,
   #            Added6,Added7,Added8,Added9,Added10,Added11)]
    
    SMD[Added1<0,Added1:=0]
    SMD[Added2<0,Added2:=0]
    SMD[Added3<0,Added3:=0]
    SMD[Added4<0,Added4:=0]
    SMD[Added5<0,Added5:=0]
    SMD[Added6<0,Added6:=0]
    SMD[Added7<0,Added7:=0]
    SMD[Added8<0,Added8:=0]
    SMD[Added9<0,Added9:=0]
    if (year!=90 & year!=92 & year!=93 & year!=95){
    SMD[Added10<0,Added10:=0]
    }
    SMD[Added11<0,Added11:=0]
    
    if (year!=90 & year!=92 & year!=93 & year!=95){ 
      dep <- c( "71111", "71112","71116", "71117",
                "91128", "91129","53112", "53116",
                "53113", "82113","53125", "91311",
                "72111", "72118","72319")
    }
    
    if (year==90 | year==92 | year==93 | year==95){
      dep <- c( "71111", "71112","71116", "71117",
                "91128", "91129","53112", "53116",
                "53113", "82113","53125", "91311",
                "72111","72319")
    }
    
    SMD[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
    SMD[is.na(SMD)] <- 0
    
    if (year!=90 & year!=92 & year!=93 & year!=95){
      SMD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
            Added7+Added8+Added9+Added10+Added11]
    }
    if (year==90 | year==92 | year==93 | year==95){
      SMD[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
            Added7+Added8+Added9+Added11]
    }
    
   # z2<-SMD[,.(HHID,Decile,Added,Added1,Added2,Added3,Added4,Added5,
   #            Added6,Added7,Added8,Added9,Added10,Added11)]
    
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
    
    
    SMD[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=w]
    SMD[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=nw]
    
    SMD[,weighted.mean(Total_Exp_Month,Weight)]
    SMD[,weighted.mean(Total_Exp_Month_nondurable,Weight)]
    
    SMD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeOECD]
    SMD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeOECD]
    
    ###################################################################
    
    SMD <- SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    
    SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]
    SMD <- SMD[,xr25th:=.SD[25,Total_Exp_Month_Per_nondurable_Real],by=.(Region,NewArea_Name)]
    SMD <- SMD[,First25:=ifelse(Total_Exp_Month_Per_nondurable_Real<=xr25th,1,0)]
    
    SMD <- SMD[order(Total_Exp_Month_Per_nondurable_Real)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
    SMD <- SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
    
    #Calculate deciles by weights
    SMD <- SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
    SMD <- SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
    
    
    SMD <- SMD[,InitialPoorBasedOnPercentile:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    
    
     cat("\n",i,":",SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)])
    
    
  }
  
#  A1<-SMD[(`71111`+`71112`+`71116`+`71117`>0),
#          .(A1=weighted.mean(`71111`+`71112`+`71116`+
#                               `71117`,Weight)),by="Decile"]
#  A2<-SMD[(`91128`+`91129`>0),
#          .(A2=weighted.mean(`91128`+`91129`,Weight)),by="Decile"]
#  A3<-SMD[`53112`>0 ,.(A3=weighted.mean(`53112`,Weight)),by="Decile"]
#  A4<-SMD[`53116`>0 , .(A4=weighted.mean(`53116`,Weight)),by="Decile"]
#  A5<-SMD[`53113`>0 ,.(A5=weighted.mean(`53113`,Weight)),by="Decile"]
#  A6<-SMD[`82113`>0, .(A6=weighted.mean(`82113`,Weight)),by="Decile"]
#  A7<-SMD[`53125`>0,.(A7=weighted.mean(`53125`,Weight)),by="Decile"]
#  A8<-SMD[`91311`>0 ,.(A8= weighted.mean(`91311`,Weight)),by="Decile"]
#  A9<-SMD[`72111`>0,.(A9=weighted.mean(`72111`,Weight)),by="Decile"]
#  if (year!=90 & year!=92 & year!=93 & year!=95){
#    A10<-SMD[`72118`>0 ,.(A10=weighted.mean(`72118`,Weight)),by="Decile"]
#  }
#  A11<-SMD[`72319`>0 ,.(A11=weighted.mean(`72319`,Weight)),by="Decile"]
  
#  SMD[,A1:=NULL]
#  SMD[,A2:=NULL]
#  SMD[,A3:=NULL]
#  SMD[,A4:=NULL]
#  SMD[,A5:=NULL]
#  SMD[,A6:=NULL]
#  SMD[,A7:=NULL]
#  SMD[,A8:=NULL]
#  SMD[,A9:=NULL]
#  SMD[,A10:=NULL]
#  SMD[,A11:=NULL]
  
#  SMD<-merge(SMD,A1,by="Decile")
#  SMD<-merge(SMD,A2,by="Decile")
#  SMD<-merge(SMD,A3,by="Decile")
#  SMD<-merge(SMD,A4,by="Decile")
#  SMD<-merge(SMD,A5,by="Decile")
#  SMD<-merge(SMD,A6,by="Decile")
#  SMD<-merge(SMD,A7,by="Decile")
#  SMD<-merge(SMD,A8,by="Decile")
#  SMD[,A8:=as.numeric(A8)]
#  SMD<-merge(SMD,A9,by="Decile")
  
 # if (year!=90 & year!=92 & year!=93 & year!=95){
#    SMD<-merge(SMD,A10,by="Decile")
#  }
 # SMD<-merge(SMD,A11,by="Decile")
  
  
#  SMD[car=="True",Added1:=A1-0.05*Auto_Sale]  #We use 0.05 instead of 0.1
#  SMD[tvcr=="True",Added2:=A2-0.033*TV_Sale]
#  SMD[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
#      Added3:=A3-0.033*yakhchal_Sale]
#  SMD[oven=="True",Added4:=A4-0.033*ojaghgaz_Sale]
 # SMD[washer=="True",Added5:=A5-0.033*lebasshooyi_Sale]
#  SMD[cellphone=="True",Added6:=A6-0.11*Mobile_Sale]
#  SMD[cooler_gas=="True",Added7:=A7-0.05*Coolergazi_Sale]
#  SMD[computer=="True",Added8:=A8]
#  SMD[computer=="True",Added8:=A8-0.06*PC_Sale]
#  SMD[car=="True",Added9:=A9-0.5*lastik_Sale]
#  if (year!=90 & year!=92 & year!=93 & year!=95){
#    SMD[car=="True",Added10:=A10]
#  }
 # SMD[car=="True",Added11:=A11]
  
  
  #SMD[Added1<0,Added1:=0]
  #SMD[Added2<0,Added2:=0]
  #SMD[Added3<0,Added3:=0]
  #SMD[Added4<0,Added4:=0]
  #SMD[Added5<0,Added5:=0]
  #SMD[Added6<0,Added6:=0]
  #SMD[Added7<0,Added7:=0]
  #SMD[Added8<0,Added8:=0]
  #SMD[Added9<0,Added9:=0]
  #if (year!=90 & year!=92 & year!=93 & year!=95){
  #SMD[Added10<0,Added10:=0]
  #}
  #SMD[Added11<0,Added11:=0]
  
  
#  cat(SMD[,weighted.mean(Calorie_Need_WorldBank,Weight)],"\n")
 # cat(SMD[,weighted.mean(TFoodKCaloriesHH_Per,Weight)],"\n")
 # cat(SMD[,weighted.mean(Bundle_Value,Weight)],"\n")
  
  #MD[,Added1:=NULL]
  #MD[,Added2:=NULL]
  #MD[,Added3:=NULL]
  #MD[,Added4:=NULL]
  #MD[,Added5:=NULL]
  #MD[,Added6:=NULL]
  #MD[,Added7:=NULL]
  #MD[,Added8:=NULL]
  #MD[,Added9:=NULL]
  #MD[,Added10:=NULL]
  #MD[,Added11:=NULL]
  #MD[,Added:=NULL]
  
  
  if (year!=90 & year!=92 & year!=93 & year!=95){ 
    MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile,
                          Added1,Added2,Added3,Added4,Added5,Added6,Added7,Added8,
                          Added9,Added10,Added11,Added)],by="HHID")
  }
  
  if (year==90 | year==92 | year==93 | year==95){
    MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile,
                          Added1,Added2,Added3,Added4,Added5,Added6,Added7,Added8,
                          Added9,Added11,Added)],by="HHID")
  }
  setnames(MD,"InitialPoorBasedOnPercentile","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !
  
  #cat(SMD[,.N],"\t")
  #cat(MD[,.N],"\t")
  ######################################################################
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
}
  #A<-merge(A3,A2)
  #A<-merge(A,A1)
  #A<-merge(A,A4)
  #A<-merge(A,A5)
  #A<-merge(A,A6)
  #A<-merge(A,A7)
  #A<-merge(A,A8)
  #A<-merge(A,A9)
  #A<-merge(A,A10)
  #A<-merge(A,A11)
  
  #z2<-MD[,.(HHID,Decile,Region,Added,Added1,Added2,Added3,Added4,Added5,
   #        Added6,Added7,Added8,Added9,Added10,Added11)]




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
