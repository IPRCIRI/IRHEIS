#learning R
rm(list=ls())

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(spatstat)

year <- 96

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
TD <-MD
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

Total <- merge(TD,SMD[,.(HHID,Decile,Percentile)],by="HHID")
Total <- merge(Total, MD[,.(HHID,Bundle_Value, PEngel,Engel,
                            FPLine, PovertyLine,FinalFoodPoor,
                            FinalPoor)],by="HHID")
Total[,weighted.mean(FinalPoor,Weight*Size), by=.(ProvinceCode)]

Total <- Total[,Diet:=ifelse(FinalFoodPoor==1 & FinalPoor==0,1,0)]
Total[,weighted.mean(Diet,Weight*Size), by=.(Region)]
Total[NewArea2=="Sh_Tehran",weighted.mean(Diet,Weight*Size), by=.(Region)]

TotalD <- Total[ Diet==1,
                 .(.N, Engle=weighted.mean(FoodExpenditure/Total_Exp_Month, Weight),
                 FPLine=mean(FPLine)), by=.(Region, NewArea2)]
TotalS <- Total[ FinalPoor==1,
                 .(.N, Engle_Poor=weighted.mean(FoodExpenditure/Total_Exp_Month, Weight),
                   FPLine=mean(FPLine)), by=.(Region, NewArea2)]

TotalD <- TotalD[,sample1:=N]
TotalD[,N:=NULL]

TotalT <- merge(TotalD,TotalS[,.(Region, NewArea2,Engle_Poor,N)])
TotalT[,NewArea2:=NULL]
TotalT <- TotalT[,lapply(.SD,sum),by=Region]


Total <-Total[,FoodExp_per:=FoodExpenditure/EqSizeCalory]
Total2 <- Total[FoodExp_per>2000000]
#for(col in C("HStudent")) Total2[is.na(get(col)),(col):=0]
Total2[,weighted.mean(FoodExp_per,Weight), by=.(Region, NewArea2)][order(V1)]
Total2 <- Total2[,FoodExp_per_ostan:=weighted.mean(FoodExp_per,Weight), by=.(Region, NewArea2)]

Total2 <- Total2[Size>3,Behdasht1:=weighted.mean(Behdasht_Exp,Weight),by=.(Size)]
x<- Total2[,.(HHID,Size,Behdasht1,Behdasht_Exp,Weight)]



x[,weighted.mean(Behdasht_Exp,Weight,na.rm = TRUE)]


for (col in c("Behdasht1")) x[is.na(get(col)), (col):= 0]


set1 <- c("FoodExpenditure","Behdasht_Exp","Amusement_Exp")
Total2[,Exp2:=Reduce(`+`,.SD),.SDcols=set1]



Total3 <- Total2[,.(Region, NewArea2,FoodExp_per,
                    FoodExpenditure,Behdasht_Exp,Amusement_Exp, Weight)]

Total4 <- Total3[,lapply(.SD, sum), by=.(Region,NewArea2)]

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Farhangis2.rda"))

Total2 <- merge(Total2, FarhangiData, all.x=TRUE)
for (col in c("Code","FarhangiExpenditure")) Total2[is.na(get(col)), (col):= 0]

cat(Total2[,weighted.mean(FoodKCaloriesHH_Per,Weight*Size)],"\t",
    Total2[,weighted.mean(FoodProtein_Per,Weight*Size)],"\t",
    Total2[,weighted.mean(FoodExp_per,Weight*Size)],"\t",
    Total2[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight*Size)])
