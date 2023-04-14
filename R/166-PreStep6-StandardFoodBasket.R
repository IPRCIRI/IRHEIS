
# 166-Step6-FoodPoor.R: Calculate base year basket Items based on standard food basket 


rm(list=ls())
starttime <- proc.time()
library(yaml)

Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(spatstat)
library(data.table)
library(isotone)


Groupings <- list("15","20","25","30")
names(Groupings) <- c("15","20","25","30")
FoodBasketStatsList <- list()
for(GroupingVarName in names(Groupings)){
  FoodBasketStatsList[[GroupingVarName]] <- data.table()
}

for (i in seq(15,30,by=5)) {
  cat(paste0("\n------------------------------\nRange:",i,"\n"))
  
  BigsdTable <- data.table()
  year<-Settings$baseBundleyear
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFDataTotalNutrition.rda"))
  
  Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
  Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
  
  Bfd2 <- merge(Bfd2,MD[,.(HHID,Region,Weight,Size,ProvinceCode,ProvinceName,FoodExpenditure,FoodExpenditure_Per,
                           EqSizeCalory,Dcil_IP_Cons_PAdj,Dcil_Gen_Cons_PAdj,Dcil_Gen_Cons_Nominal,Pctl_Gen_Cons_Nominal,Pctl_Gen_Cons_PAdj)],by="HHID")
  Bfd2[is.na(Bfd2)]<-0
  Bfd2[Price<0.1,Price:=NA]
  
  SelectedDecile <- Bfd2[,
                         .(FGrams_0=sum(FGrams),
                           FoodKCalories_0=sum(FoodKCalories),
                           FoodProtein=sum(FoodProtein),
                           FoodVitaminA=sum(FoodVitaminA),
                           FoodRiboflavin=sum(FoodRiboflavin),
                           FoodFe=sum(FoodFe),
                           FoodCalcium=sum(FoodCalcium),
                           Region=first(Region), Weight=first(Weight), FoodExpenditure_Per=first(FoodExpenditure_Per),
                           FoodExpenditure=first(FoodExpenditure),
                           Size=first(Size), EqSizeCalory=first(EqSizeCalory),
                           Dcil_IP_Cons_PAdj=first(Dcil_IP_Cons_PAdj),ProvinceName=first(ProvinceName),
                           Dcil_Gen_Cons_Nominal=first(Dcil_Gen_Cons_Nominal),Dcil_Gen_Cons_PAdj=first(Dcil_Gen_Cons_PAdj),
                           Pctl_Gen_Cons_Nominal=first(Pctl_Gen_Cons_Nominal),Pctl_Gen_Cons_PAdj=first(Pctl_Gen_Cons_PAdj)
                         ),
                         by=.(HHID)]
  
  SelectedSample <- SelectedDecile[,.(HHID=HHID,FGramspc=(FGrams_0/EqSizeCalory),
                                      FKCalspc=(FoodKCalories_0/EqSizeCalory),
                                      FoodProteinpc=(FoodProtein/EqSizeCalory),
                                      FoodVitaminApc=(FoodVitaminA/EqSizeCalory),
                                      FoodRiboflavinpc=(FoodRiboflavin/EqSizeCalory),
                                      FoodFepc=(FoodFe/EqSizeCalory),
                                      FoodCalciumpc=(FoodCalcium/EqSizeCalory),
                                      Region=Region, Weight=Weight,ProvinceName=ProvinceName,
                                      Size=Size, EqSizeCalory=EqSizeCalory,FoodExpenditure_Per=FoodExpenditure_Per,FoodExpenditure=FoodExpenditure,
                                      Dcil_IP_Cons_PAdj=Dcil_IP_Cons_PAdj,
                                      Dcil_Gen_Cons_Nominal=Dcil_Gen_Cons_Nominal,Dcil_Gen_Cons_PAdj=Dcil_Gen_Cons_PAdj,
                                      Pctl_Gen_Cons_Nominal=Pctl_Gen_Cons_Nominal,Pctl_Gen_Cons_PAdj=Pctl_Gen_Cons_PAdj)]
  
  
  
  
  #### 15 percent deviation from standard food basket
  if (i==15) {
    SelectedSample <- SelectedSample[(FKCalspc> Settings$KCaloryNeed_NutInst_L15 & FKCalspc<Settings$KCaloryNeed_NutInst_H15   & FoodProteinpc>Settings$ProteinNeed_NutInst_L15 & FoodProteinpc<Settings$ProteinNeed_NutInst_H15  & FoodVitaminApc>Settings$VitaminANeed_NutInst_L15 & FoodVitaminApc<Settings$VitaminANeed_NutInst_H15 &
                                        FoodRiboflavinpc>Settings$RiboflavinNeed_NutInst_L15 & FoodRiboflavinpc<Settings$RiboflavinNeed_NutInst_H15 & FoodFepc>Settings$FeNeed_NutInst_L15 & FoodFepc<Settings$FeNeed_NutInst_H15 & FoodCalciumpc>Settings$CalciumNeed_NutInst_L15 & FoodCalciumpc<Settings$CalciumNeed_NutInst_H15),]
    
  }
  
  ####20 Percent deviation from standard food basket
  if (i==20) {
    SelectedSample <- SelectedSample[(FKCalspc>Settings$KCaloryNeed_NutInst_L20 & FKCalspc<Settings$KCaloryNeed_NutInst_H20   & FoodProteinpc>Settings$ProteinNeed_NutInst_L20 & FoodProteinpc<Settings$ProteinNeed_NutInst_H20  & FoodVitaminApc>Settings$VitaminANeed_NutInst_L20 & FoodVitaminApc<Settings$VitaminANeed_NutInst_H20 &
                                        FoodRiboflavinpc>Settings$RiboflavinNeed_NutInst_L20 & FoodRiboflavinpc<Settings$RiboflavinNeed_NutInst_H20 & FoodFepc>Settings$FeNeed_NutInst_L20 & FoodFepc<Settings$FeNeed_NutInst_H20 & FoodCalciumpc>Settings$CalciumNeed_NutInst_L20 & FoodCalciumpc<Settings$CalciumNeed_NutInst_H20),]
    
  }
  ####25 Percent deviation from standard food basket
  if (i==25) {
    SelectedSample <- SelectedSample[(FKCalspc>Settings$KCaloryNeed_NutInst_L25 & FKCalspc<Settings$KCaloryNeed_NutInst_H25   & FoodProteinpc>Settings$ProteinNeed_NutInst_L25 & FoodProteinpc<Settings$ProteinNeed_NutInst_H25  & FoodVitaminApc>Settings$VitaminANeed_NutInst_L25 & FoodVitaminApc<Settings$VitaminANeed_NutInst_H25 &
                                        FoodRiboflavinpc>Settings$RiboflavinNeed_NutInst_L25 & FoodRiboflavinpc<Settings$RiboflavinNeed_NutInst_H25 & FoodFepc>Settings$FeNeed_NutInst_L25 & FoodFepc<Settings$FeNeed_NutInst_H25 & FoodCalciumpc>Settings$CalciumNeed_NutInst_L25 & FoodCalciumpc<Settings$CalciumNeed_NutInst_H25),]
    
    
  }
  
  ####30 Percent deviation from standard food basket
  if (i==30) {
    SelectedSample <- SelectedSample[(FKCalspc>Settings$KCaloryNeed_NutInst_L30 & FKCalspc<Settings$KCaloryNeed_NutInst_H30   & FoodProteinpc>Settings$ProteinNeed_NutInst_L30 & FoodProteinpc<Settings$ProteinNeed_NutInst_H30  & FoodVitaminApc>Settings$VitaminANeed_NutInst_L30 & FoodVitaminApc<Settings$VitaminANeed_NutInst_H30 &
                                        FoodRiboflavinpc>Settings$RiboflavinNeed_NutInst_L30 & FoodRiboflavinpc<Settings$RiboflavinNeed_NutInst_H30 & FoodFepc>Settings$FeNeed_NutInst_L30 & FoodFepc<Settings$FeNeed_NutInst_H30 & FoodCalciumpc>Settings$CalciumNeed_NutInst_L30 & FoodCalciumpc<Settings$CalciumNeed_NutInst_H30),]
    
  }
  
  
  
  SelectedSampleFoodBasket <- merge(SelectedSample,Bfd2[,.(HHID,FoodType,FoodCode,FGrams,Price,Expenditure,
                                                           FoodKCalories,FoodProtein,FoodVitaminA,FoodRiboflavin,
                                                           FoodFe,FoodCalcium)],all.x = T,by=c("HHID"))
  
  
  BaseYearBasket <- SelectedSampleFoodBasket[,
                                             .(FGrams_0=sum(FGrams),
                                               FoodKCalories_0=sum(FoodKCalories),
                                               FoodProtein=sum(FoodProtein),
                                               FoodVitaminA=sum(FoodVitaminA),
                                               FoodRiboflavin=sum(FoodRiboflavin),
                                               FoodFe=sum(FoodFe),
                                               FoodCalcium=sum(FoodCalcium),
                                               Region=first(Region), Weight=first(Weight), FoodExpenditure_Per=first(FoodExpenditure_Per),
                                               FoodExpenditure=first(FoodExpenditure),
                                               Size=first(Size), EqSizeCalory=first(EqSizeCalory),
                                               Dcil_IP_Cons_PAdj=first(Dcil_IP_Cons_PAdj),ProvinceName=first(ProvinceName),
                                               Dcil_Gen_Cons_Nominal=first(Dcil_Gen_Cons_Nominal),Dcil_Gen_Cons_PAdj=first(Dcil_Gen_Cons_PAdj),
                                               Pctl_Gen_Cons_Nominal=first(Pctl_Gen_Cons_Nominal),Pctl_Gen_Cons_PAdj=first(Pctl_Gen_Cons_PAdj),
                                               Region=first(Region), Weight=first(Weight),
                                               Size=first(Size), EqSizeCalory=first(EqSizeCalory)
                                             ),
                                             by=.(HHID,FoodType)]
  
  BaseYearBasket <- BaseYearBasket[Region=="Urban",
                                   .(FGramspc=weighted.mean(FGrams_0/EqSizeCalory,
                                                            Weight*Size),
                                     FKCalspc=weighted.mean(FoodKCalories_0/EqSizeCalory,
                                                            Weight*Size),
                                     FoodProteinpc=weighted.mean(FoodProtein/EqSizeCalory,Weight*Size),
                                     FoodVitaminApc=weighted.mean(FoodVitaminA/EqSizeCalory,Weight*Size),
                                     FoodRiboflavinpc=weighted.mean(FoodRiboflavin/EqSizeCalory, Weight*Size),
                                     FoodFepc=weighted.mean(FoodFe/EqSizeCalory,Weight*Size),
                                     FoodCalciumpc=weighted.mean(FoodCalcium/EqSizeCalory,Weight*Size)),
                                   by=.(FoodType)]
  
  BaseYearBasket[,BasketCals:=sum(FKCalspc)]
  
  BaseYearBasket[,StandardFGramspc:=FGramspc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardKcal:=FKCalspc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardProtein:=FoodProteinpc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardVitaminA:=FoodVitaminApc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardFe:=FoodFepc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardRibiflavin:=FoodRiboflavinpc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  BaseYearBasket[,StandardCalcium:=FoodCalciumpc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]
  
  FoodBasketStatsList[[paste0(i)]] <- rbind(FoodBasketStatsList[[paste0(i)]],BaseYearBasket)
  
}
  library(writexl)
  write_xlsx(FoodBasketStatsList,path = paste0(Settings$HEISResultsPath,"/FoodBasketStatsList.xlsx"))
