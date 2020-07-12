# 168- Step 8,9-Poverty Line.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Refah Calculations =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

Data1 <- data.table(Year=NA_integer_,Number=NA_real_,Region=NA_character_,
                    HSex=NA_character_,Decile=NA_real_)[0]
Data2 <- data.table(Year=NA_integer_,Number=NA_real_,HSex=NA_character_,
                    Decile=NA_real_)[0]
Data3 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                    HSex=NA_character_,Decile=NA_real_)[0]
Data4 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                    Decile=NA_real_)[0]
Data5 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data6 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                    Decile=NA_real_)[0]
Data7 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                    HSex=NA_character_,Decile=NA_real_)[0]
Data8 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                    Decile=NA_real_)[0]
Data9 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data10 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                    Decile=NA_real_)[0]
Data11 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                    HSex=NA_character_,Decile=NA_real_)[0]
Data12 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                    Decile=NA_real_)[0]
Data13 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data14 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data15 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data16 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data17 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data18 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data19 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data20 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data21 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data22 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data23 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data24 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data25 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data26 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data27 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data28 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data29 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data30 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data31 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data32 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data33 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data34 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data35 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data36 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data37 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data38 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data39 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data40 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data41 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data42 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data43 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data44 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data45 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data46 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data47 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data48 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data49 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data50 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data51 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data52 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data53 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data54 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data55 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     HSex=NA_character_,Decile=NA_real_)[0]
Data56 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,HSex=NA_character_,
                     Decile=NA_real_)[0]
Data57 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_, Decile=NA_real_)[0]
Data58 <- data.table(Year=NA_integer_,Total_Exp_Month=NA_real_,Region=NA_character_,
                     Decile=NA_real_)[0]
Data61 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     Region=NA_real_,HSex=NA_character_)[0]
Data62 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     Region=NA_real_)[0]
Data63 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     HSex=NA_character_)[0]
Data64 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_)[0]
Data65 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     Region=NA_real_,HSex=NA_character_)[0]
Data66 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     Region=NA_real_)[0]
Data67 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_,
                     HSex=NA_character_)[0]
Data68 <- data.table(Year=NA_integer_,FPLine=NA_real_,PovertyLine=NA_real_,
                     FinalFoodPoor=NA_real_,FinalPoor=NA_real_)[0]
Data71 <- data.table(Year=NA_integer_,Oil_NabatiGram=NA_real_,
                     Decile=NA_real_)[0]
Data72 <- data.table(Year=NA_integer_,BreadGrams=NA_real_,
                     Decile=NA_real_)[0]
Data73 <- data.table(Year=NA_integer_,SibzaminiGram=NA_real_,
                     Decile=NA_real_)[0]
Data81 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data82 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data83 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data84 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data85 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data86 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
Data87 <- data.table(Year=NA_integer_,x=NA_real_,
                     Decile=NA_real_)[0]
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodCon.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  #load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Durables.rda"))
  
  MD[,Durable_Exp:=NULL]
  MD[,Durable_Sale:=NULL]
  
  if (year >= 90){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
    MD[,Decile:=NULL]
    MD[,Percentile:=NULL]
    MD<-merge(MD,Deciles,by="HHID")
  }
  
  MD<-merge(MD,TotalFoodCon,by="HHID")
  MD<-merge(MD,TotalFoodExp[,.(HHID,G01111,G01114,G01121,G01123,G0114,G01153,`011731`)],by="HHID")
  MD<-merge(MD,DurableData,by="HHID")
    #cat(MD[,weighted.mean(FPLine*Size,Weight,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(PovertyLine*Size,Weight,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(FinalPoor,Weight,na.rm = TRUE)],"\t")
  
  #cat(MD[,weighted.mean(FPLine,Weight,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(PovertyLine,Weight,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(FinalFoodPoor,Weight*Size,na.rm = TRUE)],"\t")
  #cat(MD[,weighted.mean(FinalPoor,Weight*Size,na.rm = TRUE)],"\t")

  
  X <- MD[,.(Number=sum(Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data1 <- rbind(Data1,X)
  write.csv(Data1,file="Data1.csv")
  
  X <- MD[,.(Number=sum(Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data2 <- rbind(Data2,X)
  write.csv(Data2,file="Data2.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data3 <- rbind(Data3,X)
  write.csv(Data3,file="Data3.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data4 <- rbind(Data4,X)
  write.csv(Data4,file="Data4.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data5 <- rbind(Data5,X)
  write.csv(Data5,file="Data5.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data6 <- rbind(Data6,X)
  write.csv(Data6,file="Data6.csv")

  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month-FoodExpenditure,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data7 <- rbind(Data7,X)
  write.csv(Data7,file="Data7.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month-FoodExpenditure,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data8 <- rbind(Data8,X)
  write.csv(Data8,file="Data8.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month-FoodExpenditure,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data9 <- rbind(Data9,X)
  write.csv(Data9,file="Data9.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month-FoodExpenditure,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data10 <- rbind(Data10,X)
  write.csv(Data10,file="Data10.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data11 <- rbind(Data11,X)
  write.csv(Data11,file="Data11.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data12 <- rbind(Data12,X)
  write.csv(Data12,file="Data12.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data13 <- rbind(Data13,X)
  write.csv(Data13,file="Data13.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data14 <- rbind(Data14,X)
  write.csv(Data14,file="Data14.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(ServiceExp,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data15 <- rbind(Data15,X)
  write.csv(Data15,file="Data15.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(ServiceExp,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data16 <- rbind(Data16,X)
  write.csv(Data16,file="Data16.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(ServiceExp,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data17 <- rbind(Data17,X)
  write.csv(Data17,file="Data17.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(ServiceExp,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data18 <- rbind(Data18,X)
  write.csv(Data18,file="Data18.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Hygiene_Exp+Medical_Exp,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data19 <- rbind(Data19,X)
  write.csv(Data19,file="Data19.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Hygiene_Exp+Medical_Exp,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data20 <- rbind(Data20,X)
  write.csv(Data20,file="Data20.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Hygiene_Exp+Medical_Exp,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data21 <- rbind(Data21,X)
  write.csv(Data21,file="Data21.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Hygiene_Exp+Medical_Exp,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data22 <- rbind(Data22,X)
  write.csv(Data22,file="Data22.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Education_Exp,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data23 <- rbind(Data23,X)
  write.csv(Data23,file="Data23.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Education_Exp,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data24 <- rbind(Data24,X)
  write.csv(Data24,file="Data24.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Education_Exp,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data25 <- rbind(Data25,X)
  write.csv(Data25,file="Data25.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Education_Exp,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data26 <- rbind(Data26,X)
  write.csv(Data26,file="Data26.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(HotelRestaurant_Exp+Amusement_Exp,Weight)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data27 <- rbind(Data27,X)
  write.csv(Data27,file="Data27.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(HotelRestaurant_Exp+Amusement_Exp,Weight)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data28 <- rbind(Data28,X)
  write.csv(Data28,file="Data28.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(HotelRestaurant_Exp+Amusement_Exp,Weight)),by=c("Decile")]
  X[,Year:=year]
  Data29 <- rbind(Data29,X)
  write.csv(Data29,file="Data29.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(HotelRestaurant_Exp+Amusement_Exp,Weight)),by=c("Region","Decile")]
  X[,Year:=year]
  Data30 <- rbind(Data30,X)
  write.csv(Data30,file="Data30.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Durable_Exp-Durable_Sale,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data31 <- rbind(Data31,X)
  write.csv(Data31,file="Data31.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Durable_Exp-Durable_Sale,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data32 <- rbind(Data32,X)
  write.csv(Data32,file="Data32.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Durable_Exp-Durable_Sale,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data33 <- rbind(Data33,X)
  write.csv(Data33,file="Data33.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Durable_Exp-Durable_Sale,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data34 <- rbind(Data34,X)
  write.csv(Data34,file="Data34.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month_Per,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data35 <- rbind(Data35,X)
  write.csv(Data35,file="Data35.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month_Per,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data36 <- rbind(Data36,X)
  write.csv(Data36,file="Data36.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month_Per,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data37 <- rbind(Data37,X)
  write.csv(Data37,file="Data37.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(Total_Exp_Month_Per,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data38 <- rbind(Data38,X)
  write.csv(Data38,file="Data38.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data39 <- rbind(Data39,X)
  write.csv(Data39,file="Data39.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data40 <- rbind(Data40,X)
  write.csv(Data40,file="Data40.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data41 <- rbind(Data41,X)
  write.csv(Data41,file="Data41.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data42 <- rbind(Data42,X)
  write.csv(Data42,file="Data42.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(LivestockGrams,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data43 <- rbind(Data43,X)
  write.csv(Data43,file="Data43.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(LivestockGrams,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data44 <- rbind(Data44,X)
  write.csv(Data44,file="Data44.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(LivestockGrams,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data45 <- rbind(Data45,X)
  write.csv(Data45,file="Data45.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(LivestockGrams,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data46 <- rbind(Data46,X)
  write.csv(Data46,file="Data46.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(PoultryMeat_MGram,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data47 <- rbind(Data47,X)
  write.csv(Data47,file="Data47.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(PoultryMeat_MGram,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data48 <- rbind(Data48,X)
  write.csv(Data48,file="Data48.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(PoultryMeat_MGram,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data49 <- rbind(Data49,X)
  write.csv(Data49,file="Data49.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(PoultryMeat_MGram,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data50 <- rbind(Data50,X)
  write.csv(Data50,file="Data50.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(MilkGrams+MilkproductsGrams,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data51 <- rbind(Data51,X)
  write.csv(Data51,file="Data51.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(MilkGrams+MilkproductsGrams,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data52 <- rbind(Data52,X)
  write.csv(Data52,file="Data52.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(MilkGrams+MilkproductsGrams,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data53 <- rbind(Data53,X)
  write.csv(Data53,file="Data53.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(MilkGrams+MilkproductsGrams,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data54 <- rbind(Data54,X)
  write.csv(Data54,file="Data54.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(GrainGrams,Weight,na.rm = TRUE)),by=c("Region","HSex","Decile")]
  X[,Year:=year]
  Data55 <- rbind(Data55,X)
  write.csv(Data55,file="Data55.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(GrainGrams,Weight,na.rm = TRUE)),by=c("HSex","Decile")]
  X[,Year:=year]
  Data56 <- rbind(Data56,X)
  write.csv(Data56,file="Data56.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(GrainGrams,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data57 <- rbind(Data57,X)
  write.csv(Data57,file="Data57.csv")
  
  X <- MD[,.(Total_Exp_Month=weighted.mean(GrainGrams,Weight,na.rm = TRUE)),by=c("Region","Decile")]
  X[,Year:=year]
  Data58 <- rbind(Data58,X)
  write.csv(Data58,file="Data58.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine*Size,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine*Size,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight,na.rm = TRUE)),by=c("Region","HSex")]
  X[,Year:=year]
  Data61 <- rbind(Data61,X)
  write.csv(Data61,file="Data61.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine*Size,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine*Size,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight,na.rm = TRUE)),by=c("Region")]
  X[,Year:=year]
  Data62 <- rbind(Data62,X)
  write.csv(Data62,file="Data62.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine*Size,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine*Size,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight,na.rm = TRUE)),by=c("HSex")]
  X[,Year:=year]
  Data63 <- rbind(Data63,X)
  write.csv(Data63,file="Data63.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine*Size,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine*Size,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight,na.rm = TRUE))]
  X[,Year:=year]
  Data64 <- rbind(Data64,X)
  write.csv(Data64,file="Data64.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight*Size,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight*Size,na.rm = TRUE)),by=c("Region","HSex")]
  X[,Year:=year]
  Data65 <- rbind(Data65,X)
  write.csv(Data65,file="Data65.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight*Size,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight*Size,na.rm = TRUE)),by=c("Region")]
  X[,Year:=year]
  Data66 <- rbind(Data66,X)
  write.csv(Data66,file="Data66.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight*Size,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight*Size,na.rm = TRUE)),by=c("HSex")]
  X[,Year:=year]
  Data67 <- rbind(Data67,X)
  write.csv(Data67,file="Data67.csv")
  
  X <- MD[,.(FPLine=weighted.mean(FPLine,Weight,na.rm = TRUE),
             PovertyLine=weighted.mean(PovertyLine,Weight,na.rm = TRUE),
             FinalFoodPoor=weighted.mean(FinalFoodPoor,Weight,na.rm = TRUE),
             FinalPoor=weighted.mean(FinalPoor,Weight,na.rm = TRUE))]
  X[,Year:=year]
  Data68 <- rbind(Data68,X)
  write.csv(Data68,file="Data68.csv")
  
  
  X <- MD[,.(Oil_NabatiGram=weighted.mean(Oil_NabatiGram,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data71 <- rbind(Data71,X)
  write.csv(Data71,file="Data71.csv")
  
  X <- MD[,.(BreadGrams=weighted.mean(BreadGrams,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data72 <- rbind(Data72,X)
  write.csv(Data72,file="Data72.csv")
  
  X <- MD[,.(SibzaminiGram=weighted.mean(SibzaminiGram,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data73 <- rbind(Data73,X)
  write.csv(Data73,file="Data73.csv")
  
  
  X <- MD[,.(x=weighted.mean(G01111/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data81 <- rbind(Data81,X)
  write.csv(Data81,file="Data81.csv")
  
  X <- MD[,.(x=weighted.mean(G01114/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data82 <- rbind(Data82,X)
  write.csv(Data82,file="Data82.csv")
  
  X <- MD[,.(x=weighted.mean(G01121/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data83 <- rbind(Data83,X)
  write.csv(Data83,file="Data83.csv")
  
  X <- MD[,.(x=weighted.mean(G01123/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data84 <- rbind(Data84,X)
  write.csv(Data84,file="Data84.csv")
  
  X <- MD[,.(x=weighted.mean(G0114/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data85 <- rbind(Data85,X)
  write.csv(Data85,file="Data85.csv")
  
  X <- MD[,.(x=weighted.mean(G01153/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data86 <- rbind(Data86,X)
  write.csv(Data86,file="Data86.csv")
  
  X <- MD[,.(x=weighted.mean(`011731`/FoodExpenditure,Weight,na.rm = TRUE)),by=c("Decile")]
  X[,Year:=year]
  Data87 <- rbind(Data87,X)
  write.csv(Data87,file="Data87.csv")
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")