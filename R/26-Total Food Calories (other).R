# 26-All coleries.R
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
  BigFData <- data.table(HHID=NA_integer_,FGrams=NA_real_,Expenditure=NA_real_,
                         FoodType=NA_character_,FoodKCalories=NA_real_,FoodTKCalories=NA_real_)[0]
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
      pcols <- intersect(names(TF),c("HHID","Code","Kilos","Expenditure"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      
      TF[,Kilos:=as.numeric(Kilos)]
      
      load(file = "CaloriesTable.rda")
      TF<-merge(TF,CalooriesTablee)
      TF[is.na(TF)] <- 0
      TF[,FGrams:=(Kilos*1000)/30]
      FData <- TF[,lapply(.SD,sum),by=HHID]
      FData[, FoodType:=TFoodGroups[i,FoodType]]
      FData[, FoodKCalories:=TFoodGroups[i,KCalories]*FGrams]
      FData[, FoodTKCalories:=Calorie_per_gram*FGrams]
      
      BigFData <- rbind(BigFData,FData[,.(HHID,FGrams,Expenditure,FoodType,FoodKCalories,FoodTKCalories)])
      save(BigFData, file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
    }
    if(year %in% 83:96){
      pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos","Expenditure"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
      TF[,Expenditure:=as.numeric(Expenditure)]
      
      load(file = "CaloriesTable.rda")
      TF<-merge(TF,CalooriesTablee)
      TF[is.na(TF)] <- 0
      TF[,FGrams:=(Kilos*1000+Grams)/30]
      FData <- TF[,lapply(.SD,sum),by=HHID]
      FData[, FoodType:=TFoodGroups[i,FoodType]]
      FData[, FoodKCalories:=TFoodGroups[i,KCalories]*FGrams]
      FData[, FoodTKCalories:=Calorie_per_gram*FGrams]
      
      BigFData <- rbind(BigFData,FData[,.(HHID,FGrams,Expenditure,FoodType,FoodKCalories,FoodTKCalories)])
      save(BigFData, file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
    }
  }
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  HHBase <- merge(HHBase,HHI[,.(HHID,Size)],by="HHID")
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  FData <- BigFData[,.(FoodTKCalories=sum(FoodTKCalories),
                       FoodKCalories=sum(FoodKCalories)),by=HHID]
  FData <- merge(HHBase[,.(HHID,Region,Size)],
                 FData,by = "HHID",all.x = TRUE)
  
  FData <- FData[FoodTKCalories<100000] # arbitrary removal of outliers
  
  save(FData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
}


cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")