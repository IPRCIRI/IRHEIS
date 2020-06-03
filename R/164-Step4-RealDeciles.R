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
  if (year!=92){
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
  if (year!=92){ 
  DataTable<-merge(DataTable,A10,by="Decile")
  }
  DataTable<-merge(DataTable,A11,by="Decile")
  
  DataTable[car=="True",Added1:=A1]
  DataTable[tvcr=="True",Added2:=A2]
  DataTable[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
     Added3:=A3]
  DataTable[oven=="True",Added4:=A4]
  DataTable[washer=="True",Added5:=A5]
  DataTable[cellphone=="True",Added6:=A6]
  DataTable[cooler_gas=="True",Added7:=A7]
  DataTable[computer=="True",Added8:=A8]
  DataTable[car=="True",Added9:=A9]
  if (year!=92){ 
  DataTable[car=="True",Added10:=A10]
  }
  DataTable[car=="True",Added11:=A11]
  
  if (year!=92){  
  dep <- c( "71111", "71112","71116", "71117",
            "91128", "91129","53112", "53116",
            "53113", "82113","53125", "91311",
            "72111", "72118","72319")
  }
  
  if (year==92){  
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111","72319")
  }
  
  DataTable[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  DataTable[is.na(DataTable)] <- 0
  
  if (year!=92){ 
  DataTable[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
       Added7+Added8+Added9+Added10+Added11]
  }
  if (year==92){ 
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


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  if (year!=92){  
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
               ,car,tvcr,freezer,frez_refrig,refrigerator,oven,
               washer,cellphone,cooler_gas,computer
               ,OriginalFoodExpenditure,FoodOtherExpenditure, Cigar_Exp, Cloth_Exp,
               Amusement_Exp, Communication_Exp, 
               HouseandEnergy_Exp, Furniture_Exp, HotelRestaurant_Exp, Hygiene_Exp, 
               Transportation_Exp, Other_Exp
               ,Add_to_NonDurable,Medical_Exp,
               Durable_NoDep,Durable_Emergency)]
  }
  if (year==92){  
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
  

######################################################################
save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))

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


}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
