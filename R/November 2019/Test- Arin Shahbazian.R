#Test- Arin Shahbazian
rm(list=ls())

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

year<-96

load( file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))

Total<-merge(MD,SMD[,.(HHID,Decile,Percentile)],by="HHID")

Total<-Total[,FoodExp_Per:=FoodExpenditure/EqSizeCalory]
Total2<-Total[FoodExp_Per>2000000]



Total2[,weighted.mean(FoodExp_Per,Weight)]
Total2[,weighted.mean(FoodExp_Per,Weight),by=.(Region,NewArea2)][order(V1)]
Total2<-Total2[,FoodExp_per_Ostani:=weighted.mean(FoodExp_Per,Weight),by=.(Region,NewArea2)]

Total2<-Total2[Size>3,Behdasht1:=weighted.mean(Behdasht_Exp,Weight),by=.(Size)]
x<-Total2[,.(HHID,Size,Behdasht_Exp,Behdasht1,Weight)]


x[,weighted.mean(Behdasht1,Weight,na.rm = TRUE)]

for (col in c("Behdasht1"))  x[is.na(get(col)), (col) := 0]


set1 <- c( "FoodExpenditure", "Behdasht_Exp","Amusement_Exp")
Total2[, Exp3 := Reduce(`+`, .SD), .SDcols=set1]


Total3<-Total2[,.(Region,NewArea2,FoodExpenditure,
                  Behdasht_Exp,Amusement_Exp,Weight)]
Total4 <- Total3[,lapply(.SD,sum),by=.(Region,NewArea2)]

load( file = paste0(Settings$HEISProcessedPath,"Y",year,"Farhangis2.rda"))

Total2<-merge(Total2,FarhangiData,all.x = TRUE)
for (col in c("Code","FarhangiExpenditure"))  Total2[is.na(get(col)), (col) := 0]

cat(Total2[,weighted.mean(FoodKCaloriesHH_Per,Weight)],"\n",
    Total2[,weighted.mean(FoodProtein_Per,Weight)],"\n",
    Total2[,weighted.mean(FoodExp_Per,Weight)],"\n",
    Total2[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight)])

