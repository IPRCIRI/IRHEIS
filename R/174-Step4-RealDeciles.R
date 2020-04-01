#174-FindInitialPoor.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  SMD <- MD[,.(HHID,Region,ProvinceCode,
               ServiceExp,FoodExpenditure,Total_Exp_Month,
               NewArea,NewArea_Name,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
               # Total_Exp_Month_Per_nondurable2,TFoodExpenditure_Per2,
               Durable_Exp,
               TFoodKCaloriesHH_Per,Calorie_Need_WorldBank,Calorie_Need_Anstitoo,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  #Choose one of these
  SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Calorie_Need_Anstitoo/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_WorldBank/TFoodKCaloriesHH_Per]
  #SMD[,Bundle_Value:=TOriginalFoodExpenditure_Per*Settings$KCaloryNeed_Adult_Anstitoo/TFoodKCaloriesHH_Per]
  
  
  SMD <- SMD[Bundle_Value<=5000000 | TFoodKCaloriesHH_Per>=300] #arbitrary measures, TODO: check in diff years
  
  #Real Prices
  T_Bundle_Value <- SMD[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
  TBV1 <- T_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
  TBV2 <- T_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
  
  SMD[,PriceIndex2:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2)/2
      ,by=.(Region,NewArea_Name)]
  
  X <- SMD[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
             weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
  X[,V:=V1+V2]

  SMD <- merge(SMD,X,by=c("Region","NewArea_Name"))
  
  SMD[,PriceIndex:=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TBV1*V1
                    +weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TBV2*V2)/V
      ,by=.(Region,NewArea_Name)]
  
  Compare1<-SMD[,.(Old=mean(PriceIndex2),
                  New=mean(PriceIndex)),by=.(Region,NewArea_Name)]

  SMD[,V1:=NULL]
  SMD[,V2:=NULL]
  SMD[,V:=NULL]
  
  
  SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
  
  ###Real- Urban & Rural
  #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]  #Deciling in Urban- Rural
  #SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  #SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  #SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  ###Real- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
  SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
  
  ###Nominal-  Urban & Rural
  #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable)]  #Deciling in Urban- Rural(Nominal)
  #SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  #SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  #SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100),by=Region]
  
  ###Nominal- Country
  SMD<- SMD[order(Total_Exp_Month_Per_nondurable)]  #Deciling in Country(Nominal)
  SMD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  SMD[,Decile_Nominal:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10)]
  SMD[,Percentile_Nominal:=cut(crw2,breaks=seq(0,1,.01),labels=1:100)]
  
  

  SMD[,NewPoor:=1]
  SMD[,ThisIterationPoor:=0]
  i <- 0
  while(sum(SMD[,(ThisIterationPoor-NewPoor)^2])>=0.002*nrow(SMD) & i <=50){
    i <- i+1
    SMD[,pold:=Percentile]
    SMD[,ThisIterationPoor:=ifelse(pold %in% Settings$InitialPoorPercentile,1,0)]
    SMDIterationPoor<-SMD[ThisIterationPoor==TRUE]
    SMDIterationPoor[,sum(ThisIterationPoor),by=.(Region,NewArea_Name)][order(Region,NewArea_Name)]
    
    T_P_Bundle_Value <- SMDIterationPoor[NewArea==2301, .(Bundle_Value,MetrPrice,Weight)]
    TPBV1 <- T_P_Bundle_Value[,weighted.mean(Bundle_Value,Weight,na.rm = TRUE)]
    TPBV2 <- T_P_Bundle_Value[,weighted.mean(MetrPrice,Weight,na.rm = TRUE)]
    
   
    X <- SMDIterationPoor[,.(weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
                weighted.mean(ServiceExp/Total_Exp_Month,Weight)),by=.(Region,NewArea_Name)]
    X[,V:=V1+V2]
    
    SMDIterationPoor <- merge(SMDIterationPoor,X,by=c("Region","NewArea_Name"))
    SMDIterationPoor[,PriceIndex:=NULL]  
    
    Index <- SMDIterationPoor[,.(PriceIndex=(weighted.mean(Bundle_Value,Weight,na.rm = TRUE)/TPBV1*V1+
                      weighted.mean(MetrPrice,Weight,na.rm = TRUE)/TPBV2*V2)/V)
        ,by=.(Region,NewArea_Name)]
    Index <- Index[,.(PriceIndex=mean(PriceIndex)),by=.(Region,NewArea_Name)]
    
    
    SMD[,PriceIndex:=NULL]  
    SMD <- merge(SMD,Index,by=c("Region","NewArea_Name"))
    
    SMD[,Total_Exp_Month_Per_nondurable_Real:=Total_Exp_Month_Per_nondurable/PriceIndex] 
    

    ###Real- Urban & Rural
    #SMD<- SMD[order(Region,Total_Exp_Month_Per_nondurable_Real)]  #Deciling in Urban- Rural
    #SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
    #SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
    #SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100),by=Region]
    
    ###Real- Country
    SMD<- SMD[order(Total_Exp_Month_Per_nondurable_Real)]   #Deciling in Country
    SMD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
    SMD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
    SMD[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
    
    SMD[,NewPoor:=ifelse(Percentile %in% Settings$InitialPoorPercentile,1,0)]
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
    
    #cat("\n",sum(SMD[ProvinceCode==2,.N]))
    cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
    SMD[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]
    SMD[,sum(Size*Weight),by=.(Region,Decile)][order(Region,Decile)]
    
    
 #   NewDecile<-SMD[,.(HHID,Decile)]
 #   names(NewDecile)<-c("HHID","NewDecile")
 #   save(NewDecile,file=paste0(Settings$HEISProcessedPath,"Y",year,"NewDecile.rda"))
    
   #    OldDecile<-SMD[,.(HHID,Decile)]
    #   names(OldDecile)<-c("HHID","OldDecile")
     #  save(OldDecile,file=paste0(Settings$HEISProcessedPath,"Y",year,"OldDecile.rda"))
    
  }
  
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"NewDecile.rda"))
  #DecileCompare<-merge(as.data.table(OldDecile),NewDecile,by="HHID")
  #DecileCompare[,Diff:=as.numeric(NewDecile)-as.numeric(OldDecile)]
  #DecileCompare2<- DecileCompare[,.(.N),by=Diff]
  
  #ggplot(DecileCompare2, aes(fill=factor(Diff), y=N, x=factor(Diff))) + 
  #  geom_bar(position="dodge", stat="identity") + theme_bw() +
  #  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  #  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  MD <- merge(MD,SMD[,.(HHID,Bundle_Value,NewPoor,Decile,Percentile,Decile_Nominal,Percentile_Nominal)],by="HHID")
  setnames(MD,"NewPoor","InitialPoor")
  

  MD[,weighted.mean(InitialPoor,Weight), by=.(NewArea_Name,Region)]
  MD[,sum(Weight*Size), by=.(Decile,Region)][order(Region,Decile)]
  MD[,sum(Weight*Size), by=.(Decile_Nominal,Region)][order(Region,Decile_Nominal)]
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"AidWage.rda"))
  MD<-merge(MD,AidWageData,all.x = TRUE)
  for (col in c("aid")) MD[is.na(get(col)), (col) := 0]
  MD[,PositiveAid:=ifelse(aid>0,1,0)]
  MD[,weighted.mean(PositiveAid,Weight*Size),by=.(Region,Decile)][order(Region,Decile)]
  MD[,weighted.mean(PositiveAid,Weight*Size),by=.(Decile)][order(Decile)]
  
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
  MD<-merge(MD,SubsidyWageData,all.x = TRUE)
  for (col in c("Subsidy")) MD[is.na(get(col)), (col) := 0]
  MD[,PositiveSubsidy:=ifelse(Subsidy>0,1,0)]
  MD[,weighted.mean(PositiveSubsidy,Weight*Size),by=.(Region,Decile)][order(Region,Decile)]
  MD[,weighted.mean(PositiveSubsidy,Weight*Size),by=.(Decile)][order(Decile)]
  
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"UTSubsidyW.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"RTSubsidyW.rda"))
  
  TSubsidy<-rbind(UTSubsidyW,RTSubsidyW)
  TSubsidy<-TSubsidy[,HHID:=Address]
  MD<-merge(MD,TSubsidy[,.(HHID,check1)],all.x = TRUE)
  MD[,Subsidy3:=ifelse(check1>800000,1,0)]
  MD[,weighted.mean(Subsidy3,Weight*Size,na.rm = TRUE),by=.(Region,Decile)][order(Region,Decile)]
  MD[,weighted.mean(Subsidy3,Weight*Size,na.rm = TRUE),by=.(Decile)][order(Decile)]
  
  MD[,TotalAid:=(Subsidy+aid)/12]
  MD_Ok<- MD[TotalAid>0]
  MD_Ok[,ratio:=TotalAid/Total_Exp_Month]
  x<-MD_Ok[,.(.N,weighted.mean(ratio,Weight*Size,na.rm = TRUE)),by=.(Region,Decile)][order(Region,Decile)]
  MD_Ok[,.(weighted.mean(ratio,Weight*Size,na.rm = TRUE)),by=.(Decile)][order(Decile)]
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BreadExp.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"BreadCon.rda"))
  SMD<-merge(SMD,BreadData,by="HHID")
  SMD<-merge(SMD,BreadConsumption,by="HHID")
  
  SMD[is.na(SMD)] <- 0
  
  SMD[,weighted.mean(G01114+G01115,Weight),by=.(Region,Decile)][order(Region,Decile)]
  SMD[,weighted.mean(G01114+G01115,Weight),by=.(Decile)][order(Decile)]
  
  SMD[,weighted.mean(BreadGrams,Weight),by=.(Region,Decile)][order(Region,Decile)]
  SMD[,weighted.mean(BreadGrams,Weight),by=.(Decile)][order(Decile)]

  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"DrugsExp.rda"))
  SMD<-merge(SMD,DrugsExp,all.x = TRUE)
  SMD[is.na(SMD)] <- 0
  
  SMD[,weighted.mean(DrugsExp,Weight),by=.(Region,Decile)][order(Region,Decile)]
  SMD[,weighted.mean(DrugsExp,Weight),by=.(Decile)][order(Decile)]
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalFoodExp.rda"))
  TotalFoodExp<-merge(TotalFoodExp,SMD[,.(HHID,Decile)])
  
  TotalFoodExp[,weighted.mean(`011211`+`011212`+`011213`+
                                `011214`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011117`+`011118`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011231`+`011232`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011411`+`011412`+`011413`+`011414`+
                                `011421`+`011422`+`011423`+`011424`+
                                `011425`+`011426`+`011427`+`011428`+
                                `011429`+`011431`+`011432`+`011433`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011441`+`011442`+`011443`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011531`+`011532`+`011533`,Weight),by=.(Decile)][order(Decile)]
  
  TotalFoodExp[,weighted.mean(`011812`,Weight),by=.(Decile)][order(Decile)]
  
  }


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
