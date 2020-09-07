
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
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN398.rda"))
  
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
    save(SMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD98.rda"))
    
    #cat("\n",sum(SMD[ProvinceCode==2,.N]))
    cat("\n",sum(SMD[,(ThisIterationPoor-NewPoor)^2]))
    SMD[,weighted.mean(Size,Weight),by=.(Region)][order(Region)]
    SMD[,sum(Size*Weight),by=.(Region,Decile)][order(Region,Decile)]
    
    SMD[,weighted.mean(Size,Weight,na.rm = TRUE),by=.(Decile)][order(Decile)]
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
  
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor98.rda"))
  
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor98.rda"))
  
  #####Clustering#####
  dt2Urban<-MD[Region=="Urban",.(NewArea,NewArea_Name,Region,HHID)]
  dt2Rural<-MD[Region=="Rural",.(NewArea,NewArea_Name,Region,HHID)]
  
  #####Urban#####
  dt2Urban<-dt2Urban[NewArea_Name=="Sh_Tehran",cluster3:=1]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Sh_Shiraz" | NewArea_Name=="Sh_Esfahan" |
                       NewArea_Name=="Sh_Bandarabas" |
                       NewArea_Name=="Sh_Karaj" |
                       NewArea_Name=="Sh_Rasht",
                     cluster3:=2]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Gilan" | NewArea_Name=="Alborz" |
                       NewArea_Name=="Kohkilooye" | NewArea_Name=="Ghazvin" | 
                       NewArea_Name=="Markazi"   |  NewArea_Name=="Esfahan" |
                       NewArea_Name=="Ghom" |  NewArea_Name=="Sh_Arak" |
                       NewArea_Name=="Tehran" |  NewArea_Name=="Sh_Tabriz" |
                       NewArea_Name=="Mazandaran"  | NewArea_Name=="Sh_Yazd" ,
                     cluster3:=3]
  
  dt2Urban<-dt2Urban[  NewArea_Name=="Yazd" |
                         NewArea_Name=="Sh_Kerman" |
                         NewArea_Name=="Sh_Hamedan" |
                         NewArea_Name=="Chaharmahal" |
                         NewArea_Name=="Zanjan" | NewArea_Name=="Hamedan" |
                         NewArea_Name=="Sh_Mashhad" |  NewArea_Name=="Sh_Urmia" |
                         NewArea_Name=="Sh_Ahvaz" |   NewArea_Name=="Hormozgan"|
                         NewArea_Name=="Booshehr" |
                         NewArea_Name=="Semnan" | NewArea_Name=="Fars",
                       cluster3:=4]
  
  dt2Urban<-dt2Urban[ NewArea_Name=="Sh_Kermanshah" | NewArea_Name== "Az_Sharghi" |
                        NewArea_Name=="Kerman" | 
                        NewArea_Name=="Ardebil" |
                        NewArea_Name=="Ilam" ,
                      cluster3:=5]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Khoozestan" | NewArea_Name=="Kordestan" |
                       NewArea_Name=="Khorasan_Razavi" | 
                       NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Az_Gharbi" |
                       NewArea_Name=="Kermanshah" | NewArea_Name=="Lorestan" |
                       NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Golestan",
                     cluster3:=6]
  
  dt2Urban<-dt2Urban[NewArea_Name=="Sistan" | NewArea_Name=="Sh_Zahedan",
                     cluster3:=7]
  
  save(dt2Urban,file ="dt2Urban.rda")
  #####Rural#####
  dt2Rural<-dt2Rural[ NewArea_Name=="Tehran" | NewArea_Name=="Alborz",
                      cluster3:=8]
  
  dt2Rural<-dt2Rural[NewArea_Name=="Esfahan" | NewArea_Name=="Mazandaran" ,
                     cluster3:=9]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Ghazvin" | NewArea_Name=="Yazd"  |
                        NewArea_Name=="Gilan" |
                        NewArea_Name=="Kohkilooye" | NewArea_Name=="Booshehr",
                      cluster3:=10]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Zanjan" |
                        NewArea_Name=="Chaharmahal"| NewArea_Name=="Hormozgan" | NewArea_Name=="Az_Sharghi" | 
                        NewArea_Name=="Ghom" | 
                        NewArea_Name=="Fars" |  
                        NewArea_Name=="Markazi" |  NewArea_Name=="Semnan",
                      cluster3:=11]
  
  dt2Rural<-dt2Rural[  NewArea_Name=="Hamedan" |
                         NewArea_Name=="Az_Gharbi" |  NewArea_Name=="Kermanshah" |
                         NewArea_Name=="Kerman" | NewArea_Name=="Golestan" |
                         NewArea_Name=="Khorasan_Jonoobi" | NewArea_Name=="Lorestan" |
                         NewArea_Name=="Kordestan"|
                         NewArea_Name=="Khoozestan" |
                         NewArea_Name=="Ardebil" | NewArea_Name=="Ilam",
                       cluster3:=12]
  
  dt2Rural<-dt2Rural[ NewArea_Name=="Khorasan_Shomali" | NewArea_Name=="Khorasan_Razavi" |
                        NewArea_Name=="Sistan" ,
                      cluster3:=13]
  
  save(dt2Rural,file ="dt2Rural.rda")
  #####Merge#####
  dt2total<-rbind(dt2Urban,dt2Rural)
  
  
  dt2total[,HHID:=NULL]
  dt2total<-distinct(dt2total)
  
  MD<-merge(MD,dt2total,by=c("NewArea","NewArea_Name","Region"))
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered98.rda"))
}

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor98.rda"))
  MD<-MD[CountyCode==2301]
  #load(file = "CPI.rda")
  #CPI<-as.data.table(CPI)
  #CPI<-CPI[,Decile:=as.character(Decile)]
  #MD<-merge(MD,CPI,by="Decile")
  
  #Determine Food (Equal 2100 KCal) Bundle
  #MDPoors<-MD[InitialPoor==1]
  MD[,NewPoor:=InitialPoor]
  #MD[,NewPoor:=ifelse(Decile %in% c(1,2,3,4),1,0)]
  MD[,OldPoor:=1]
  
  i <- 0
  while(MD[(NewPoor-OldPoor)!=0,.N]>0.001*nrow(MD[NewPoor==1])  & i <=15){
    #    cat(nrow(MD[NewPoor==1]))
    i <- i + 1
    MD[,ThisIterationPoor:=NewPoor]
    MD[,FPLine:=NULL]    
    MDP <- MD[ThisIterationPoor==1,
              .(FPLine=weighted.mean(Bundle_Value,Weight,na.rm = TRUE)),
              by=.(Region)]
    MD <- merge(MD,MDP,by=c("Region"))
    #    print(MDP)
    #x<-MD[,.(NewArea,Region,FPLine,InitialPoor)]
    MD[,NewPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]
    print(table(MD[,.(ThisIterationPoor,NewPoor)]))
    MD[,OldPoor:=ThisIterationPoor]
  }
  
  MD[,FinalFoodPoor:=OldPoor]
  
  # MD <- MD[,.(HHID,HIndivNo,Region,NewArea,NewArea_Name,cluster3,ProvinceCode,Size,HAge,HSex,Month,ServiceExp,
  #    HLiterate,HEduLevel0,HActivityState,Area,Rooms,MetrPrice,Total_Exp_Month_nondurable,
  #   Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,
  #   OriginalFoodExpenditure_Per,FPLine,Weight,Percentile,FinalFoodPoor,
  #   Total_Exp_Month_Per,TFoodKCaloriesHH_Per,TOriginalFoodExpenditure,Total_Exp_Month,
  #  TFoodExpenditure2,Total_Exp_Month_nondurable2,Total_Exp_Month2,
  # Total_Exp_Month_Per2,
  #   EqSizeOECD,EqSizeCalory,Decile,Bundle_Value)]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor98.rda"))
  MD[,weighted.mean(FinalFoodPoor,Weight)]
  # MDFinalfood<-MD[,.(HHID,Region,NewArea,cluster3,Percentile,FinalFoodPoor)]
  # UrbanFinalfood<-MDFinalfood[Region=="Urban"]
  # RuralFinalfood<-MDFinalfood[Region=="Rural"]
  # save(UrbanFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"UrbanFinalfood.rda"))
  # save(RuralFinalfood, file=paste0(Settings$HEISProcessedPath,"Y",year,"RuralFinalfood.rda"))
  # 
  MD[,weighted.mean(FinalFoodPoor,Weight),by=c("Region","ProvinceCode")][order(Region,ProvinceCode)]
  #MD[,weighted.mean(FinalFoodPoor,Weight),by=cluster3][order(cluster3)]
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
