#61-FindInitialPoor.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
#library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  
  SMD <- MD[,.(HHID,Region,NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TFoodExpenditure_Per,
              # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
              TFoodTKCalories_Per,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  SMD[,Bundle_Value:=TFoodExpenditure_Per*Settings$KCaloryNeed_Adult/TFoodTKCalories_Per]
  
  #SMD <- MD[,.(HHID,Region,NewArea,Total_Exp_Month_Per_nondurable,FoodExpenditure_Per,FoodTKCalories_Per,
            #   Weight,MetrPrice)]
  
  #SMD[,Bundle_Value:=FoodExpenditure_Per*Settings$KCaloryNeed_Adult/FoodTKCalories_Per]
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodTKCalories_Per>=300]
  
  #Real Prices
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea)]
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
  SMD[,crw:=cumsum(Weight)/sum(Weight),by=Region]  # Cumulative Relative Weight
  
  #Calculate deciles by weights
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  FirstSMD<-SMD[,.(HHID,Region,NewArea,NewArea2,Percentile,Decile)]
  FirstSMD<-FirstSMD[,Realfirstpoor:=ifelse(Decile %in% 1:2,1,0)]
  save(FirstSMD, file=paste0(Settings$HEISProcessedPath,"Y",year,"FirstSMD.rda"))
  
  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=30 & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
    Index <- SMDIterationPoor[,.(PriceIndex= (weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1+
                                                weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2)/2)
                              ,by=.(Region,NewArea)]
    
    SMD[,PriceIndex:=NULL]
    SMD<- SMD[order(Region,NewArea)]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea"))
    
    SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    
    SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]
    SMD[,crw:=cumsum(Weight)/sum(Weight),by=Region]  # Cumulative Relative Weight
    
    #Calculate deciles by weights
    SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
    SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
    SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
    
    
    cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
  }
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  
  #Calculate per_Calory from resturants
  #MD[,Calory_Price:=(FoodExpenditure_Per/FoodTKCalories_Per)]
  #MD[,Calory_Price_Area:=weighted.median(Calory_Price,Weight,na.rm = TRUE),by=.(Region,NewArea)][order(Calory_Price)]
  #MD[,ResturantKCalories:=ifelse(Decile %in% 1:2,(Settings$OutFoodTKCXShare12*Resturant_Exp)/Calory_Price_Area,
                      #           ifelse(Decile %in% 3:6,(Settings$OutFoodTKCXShare3456*Resturant_Exp)/Calory_Price_Area,
                     #           (Settings$OutFoodTKCXShare78910*Resturant_Exp)/Calory_Price_Area))]
  #for (col in c("ResturantKCalories")) MD[is.na(get(col)), (col) := 0]
  #MD[,TFoodTKCalories:=FoodTKCalories+ResturantKCalories]
  #MD[,TFoodExpenditure:=FoodExpenditure+(Settings$OutFoodTKCXShare*Resturant_Exp)]
  #MD[,TFoodExpenditure:=FoodExpenditure+ifelse(Decile %in% 1:2,(Settings$OutFoodTKCXShare12*Resturant_Exp),
                #                 ifelse(Decile %in% 3:6,(Settings$OutFoodTKCXShare3456*Resturant_Exp),
                #                      (Settings$OutFoodTKCXShare78910*Resturant_Exp)))]
  
  #MD[,TFoodExpenditure_Per :=TFoodExpenditure/EqSizeCalory]
  #MD[,TFoodTKCalories_Per:=TFoodTKCalories/EqSizeCalory]

  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea,Region)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  MD2<-MD[,.(HHID,Region,NewArea,InitialPoor,Percentile,Weight)]
  MDU<-MD2[Region=="Urban"]
  MDR<-MD2[Region=="Rural"]
  save(MDU,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor2.rda"))
  save(MDR,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor3.rda"))
}

MD[,Benzin_Exp:=as.numeric(Benzin_Exp)]
MD2<-MD[,.(HHID,HIndivNo,Region,NewArea2,Decile,Percentile,Size,Weight,Total_Exp_Month_Per_nondurable,Benzin_Exp)]
MD2<-MD2[,Add_Benzin_Exp:=Benzin_Exp-600000]
MD2<-MD2[,Add_Benzin_Exp:=ifelse(Add_Benzin_Exp<0,0,Add_Benzin_Exp)]
MD2<-MD2[,Extra_Benzin:=ifelse(Add_Benzin_Exp>0,1,0)]
MD2<-MD2[,Benzin_Positive:=ifelse(Benzin_Exp>0,1,0)]

MD2<-MD2[,Decile_Add:=weighted.mean(Add_Benzin_Exp,Weight)*sum(Weight),by=.(Decile)]
MD2[,weighted.mean(Add_Benzin_Exp,Weight)*sum(Weight),by=.(Decile)][order(Decile)]

MD2<-MD2[,Decile_Total:=weighted.mean(Benzin_Exp,Weight)*sum(Weight),by=.(Decile)]
MD2[,weighted.mean(Benzin_Exp,Weight)*sum(Weight),by=.(Decile)][order(Decile)]

MD2<-MD2[,Ratio:=Decile_Add/Decile_Total]
MD2[,weighted.mean(Ratio,Weight),by=.(Decile)][order(Decile)]

MD2<-MD2[,Ratio2:=weighted.mean(Extra_Benzin,Weight),by=.(Decile)]
MD2[,weighted.mean(Extra_Benzin,Weight),by=.(Decile)][order(Decile)]

MD2<-MD2[Benzin_Positive==1,Ratio3:=weighted.mean(Extra_Benzin/HIndivNo,Weight),by=.(Decile)]
MD2[Benzin_Positive==1,weighted.mean(Extra_Benzin/HIndivNo,Weight),by=.(Decile)][order(Decile)]

MD2<-MD2[,Ratio4:=weighted.mean(Benzin_Positive,Weight),by=.(Decile)]
MD2[,weighted.mean(Benzin_Positive,Weight),by=.(Decile)][order(Decile)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")