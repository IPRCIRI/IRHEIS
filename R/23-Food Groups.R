# 23-Food Groups.R
# Builds the Food Groups data.table for households
#
# Copyright © 2018: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

cat("\n\n================ FoodGroups =====================================\n")
TFoodGroups <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_FoodGroups))
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  BigFData <- data.table(HHID=NA_integer_,FGrams=NA_real_,
                         FoodType=NA_character_,FoodKCalories=NA_real_)[0]
  for(i in 1:nrow(TFoodGroups)){
    cat(paste0(TFoodGroups[i,SheetName],", "))
    
    ThisFoodTypeTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=TFoodGroups[i,SheetName]))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- ThisFoodTypeTable[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    if(year %in% 63:82){
    pcols <- intersect(names(TF),c("HHID","Code","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    
    TF[,Kilos:=as.numeric(Kilos)]

    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF[,FGrams:=(Kilos*1000)/30]
    FData <- TF[,lapply(.SD,sum),by=HHID]
    FData[, FoodType:=TFoodGroups[i,FoodType]]
    FData[, FoodKCalories:=TFoodGroups[i,KCalories]*FGrams]
    
    BigFData <- rbind(BigFData,FData[,.(HHID,FGrams,FoodType,FoodKCalories)])
    save(BigFData, file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
    }
    if(year %in% 83:95){
      pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
      
      
      TF[,Code:=NULL]
      TF[is.na(TF)] <- 0
      TF[,FGrams:=(Kilos*1000+Grams)/30]
      FData <- TF[,lapply(.SD,sum),by=HHID]
      FData[, FoodType:=TFoodGroups[i,FoodType]]
      FData[, FoodKCalories:=TFoodGroups[i,KCalories]*FGrams]
      
      BigFData <- rbind(BigFData,FData[,.(HHID,FGrams,FoodType,FoodKCalories)])
      save(BigFData, file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
    }
    }
}
cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat(endtime-starttime)