# 52-FoodGroup-Data.R
# Builds the food group price-weight data.table for households
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
library(imputeTS)

 for(year in (Settings$startyear:Settings$endyear)){

  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  TFoodGroups <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_FoodGroups))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD<-MD[,.(HHID,Percentile)]
  HHBase<-merge(HHBase,MD ,by =c("HHID"),all=FALSE)
  HHBase<-HHBase[Percentile %in% Settings$InitialPoorPercentile]
  
  BigFoodPrice <- HHBase[,.N,by=.(Region,NewArea)]
  for(i in 1:nrow(TFoodGroups)){
    cat(paste0(TFoodGroups[i,FoodType],", "))
    
    ThisFoodTypeTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=TFoodGroups[i,SheetName]))
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
    if(year %in% 76:82){
      pcols <- intersect(names(TF),c("HHID","Code","Kilos",
                                     "Price","Expenditure"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Price:=as.numeric(str_replace_all(Price,"[\r\n]",""))]
      TF[,Expenditure:=as.numeric(Expenditure)]
      
      
      TF[is.na(Kilos),Kilos:=0]
      TF[,FGrams:=(Kilos*1000)/30]
    }
    if(year >=83){
      pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos",
                                     "Price","Expenditure"))
      TF <- TF[,pcols,with=FALSE]
      TF <- TF[Code %in% ft$StartCode:ft$EndCode]
      
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
      TF[,Price:=as.numeric(str_replace_all(Price,"[\r\n]",""))]
      TF[,Expenditure:=as.numeric(Expenditure)]
      
      TF[is.na(Grams),Grams:=0]
      TF[is.na(Kilos),Kilos:=0]
      TF[,FGrams:=(Kilos*1000+Grams)/30]
    }
    TF[is.na(Price) & !is.na(Expenditure) & !is.na(FGrams), Price:=Expenditure/FGrams]
    TF[is.na(FGrams) & !is.na(Expenditure) & !is.na(Price), FGrams:=Expenditure/Price]
    TF[is.na(Expenditure) & !is.na(FGrams) & !is.na(Price), Expenditure:=Price*FGrams]
    
    load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
    
    TF <- merge(TF[,.(HHID,Code,Price,Expenditure,FGrams)],
                HHBase[,.(HHID,Region,NewArea)], by="HHID")
    TF <- merge(TF, HHWeights[,.(HHID,Weight)])
    TF1 <- TF[,.(P=sum(Expenditure*Weight,na.rm = TRUE)/sum(FGrams*Weight,na.rm = TRUE),
                 X=sum(Expenditure*Weight,na.rm = TRUE)/sum(Weight,na.rm = TRUE)),
              by=.(Region,NewArea)]
    
    setnames(TF1,"P",paste0("P_",TFoodGroups[i,FoodType]))
    setnames(TF1,"X",paste0("X_",TFoodGroups[i,FoodType]))
    
    BigFoodPrice <- merge(BigFoodPrice,TF1,all.x = TRUE)
  }
  n <- names(BigFoodPrice)
  w <- n%like% "X_"
  s <- n[w]
  BigFoodPrice[,TFX:=Reduce(`+`, .SD),.SDcols=s]
  for(i in which(w)){
    BigFoodPrice$Sh=BigFoodPrice[,..i]/BigFoodPrice$TFX
    setnames(BigFoodPrice,"Sh",paste(paste0("Sh",names(BigFoodPrice[,..i]))))
  }
  # n <- names(BigFoodPrice)
  # w <- n%like% "ShX_"
  # s <- n[w]
  # BigFoodPrice[,TSh:=Reduce(`+`, .SD),.SDcols=s]
  
  BigFoodPrice[] <- lapply(BigFoodPrice, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  

  save(BigFoodPrice,file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
}
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])
cat(" seconds. ")
