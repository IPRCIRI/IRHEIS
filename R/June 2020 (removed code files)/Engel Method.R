rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)
library(ggplot2)

cat("\n\n================ FoodGroups =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Foods2.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  BigFData[,OriginalFoodExpenditure:=Expenditure]
  NfoodExp<-BigFData[,.(HHID,OriginalFoodExpenditure)]     
  NfoodExp <- NfoodExp[,lapply(.SD,sum),by=HHID]  
  FoodData<-merge(FoodData,NfoodExp,all.x = TRUE)
  FoodData[is.na(FoodData)] <- 0
  FoodData[,FoodOtherExpenditure:=FoodExpenditure-OriginalFoodExpenditure]  
  BigFData<-merge(BigFData,MD[,.(HHID,FinalPoor,cluster3,Total_Exp_Month_Per,ProvinceCode,Percentile,TOriginalFoodExpenditure_Per,Weight,Size,EqSizeCalory)],by=c("HHID"))
  Sibzamini<-BigFData[FoodType=="Sibzamini"]
  Sibzamini<-as.data.table(Sibzamini[,FGrams_Per:=FGrams/EqSizeCalory])
  sistan<-Sibzamini[ProvinceCode==11]
  SibzaminiKol<-Sibzamini[,weighted.mean(FGrams_Per,Weight*Size),by=c("Percentile")]
  SibzaminiSistan<-sistan[,weighted.mean(FGrams_Per,Weight*Size),by=c("Percentile")]
  Nan<-BigFData[FoodType=="Nan"]
  Nan<-as.data.table(Nan[,FGrams_Per:=FGrams/EqSizeCalory])
  sistan<-Nan[ProvinceCode==11]
  NanKol<-Nan[,weighted.mean(FGrams_Per,Weight*Size),by=c("Percentile")]
  NanSistan<-sistan[,weighted.mean(FGrams_Per,Weight*Size),by=c("Percentile")]
  

  Sibzamini<-as.data.table(Sibzamini[,Calory_Per:=FoodKCalories/EqSizeCalory])
  sistan<-Sibzamini[ProvinceCode==11]
  SibzaminiKol<-Sibzamini[,weighted.mean(Calory_Per,Weight*Size),by=c("Percentile")]
  SibzaminiSistan<-sistan[,weighted.mean(Calory_Per,Weight*Size),by=c("Percentile")]
  Nan<-as.data.table(Nan[,Calory_Per:=FoodKCalories/EqSizeCalory])
  sistan<-Nan[ProvinceCode==11]
  NanKol<-Nan[,weighted.mean(Calory_Per,Weight*Size),by=c("Percentile")]
  NanSistan<-sistan[,weighted.mean(Calory_Per,Weight*Size),by=c("Percentile")]
  
   ggplot(data=SibzaminiSistan,aes(x=as.factor(Percentile), y=as.integer(V1)))+geom_line(group=1)

  save(FoodData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Foods.rda"))
  cat(FoodData[,mean(OriginalFoodExpenditure)])
}

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")