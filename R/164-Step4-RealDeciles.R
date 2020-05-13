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
  
  DataTable <- DataTable[,InitialPoorBasedOnPercentile:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
  
  return(DataTable)
}

#year<-97
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,
               ServiceExp,FoodExpenditure,Total_Exp_Month,
               NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
  PriceDT <- CalcTornqvistIndex(SMD)
  SMD <- DoDeciling_SetInitialPoor(SMD,PriceDT)
  
  
  
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
    
    PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)
    #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
    SMD <- DoDeciling_SetInitialPoor(SMD,PriceDTBasedOnThisIterationPoor)
    
    cat("\n",i,":",SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)])
  }

  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile)],by="HHID")
  setnames(MD,"InitialPoorBasedOnPercentile","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !
  
  

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

SMD <- MD[,.(HHID,Region,
             ServiceExp,FoodExpenditure,Total_Exp_Month,
             NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
             # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
             TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
             Weight,MetrPrice,Size,EqSizeOECD)]

#Choose one of these
SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]


SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years

PriceDT <- CalcTornqvistIndex(SMD)
SMD <- DoDeciling_SetInitialPoor(SMD,PriceDT)



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
  
  PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)
  #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
  SMD <- DoDeciling_SetInitialPoor(SMD,PriceDTBasedOnThisIterationPoor)
  
  cat("\n",i,":",SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)])
}

MD[,Bundle_Value:=NULL]
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
MD[,InitialPoor:=NULL]

MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile)],by="HHID")
setnames(MD,"InitialPoorBasedOnPercentile","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !



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

SMD <- MD[,.(HHID,Region,
             ServiceExp,FoodExpenditure,Total_Exp_Month,
             NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
             # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
             TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
             Weight,MetrPrice,Size,EqSizeOECD)]

#Choose one of these
SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
#SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]


SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years

PriceDT <- CalcTornqvistIndex(SMD)
SMD <- DoDeciling_SetInitialPoor(SMD,PriceDT)



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
  
  PriceDTBasedOnThisIterationPoor <- CalcTornqvistIndex(SMDIterationPoor)
  #   print(PriceDTBasedOnThisIterationPoor[Region=="Rural" & NewArea_Name=="Semnan",])
  SMD <- DoDeciling_SetInitialPoor(SMD,PriceDTBasedOnThisIterationPoor)
  
  cat("\n",i,":",SMD[,sum((InitialPoorBasedOnPercentile-InitialPoorBasedOnPercentileLastIteration)^2)])
}

MD[,Bundle_Value:=NULL]
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
MD[,InitialPoor:=NULL]

MD <- merge(MD,SMD[,.(HHID,Bundle_Value,InitialPoorBasedOnPercentile,Decile,Percentile)],by="HHID")
setnames(MD,"InitialPoorBasedOnPercentile","InitialPoor")  # or maybe InitialPoorBasedOnRealIterativePercentile !



######################################################################
save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

A<-merge(A3,A2)
A<-merge(A,A1)
A<-merge(A,A4)
A<-merge(A,A5)
A<-merge(A,A6)
A<-merge(A,A7)
A<-merge(A,A8)
A<-merge(A,A9)
A<-merge(A,A10)
A<-merge(A,A11)


}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
