#Calculations.R

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Calculations =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  MD<-merge(MD,HHHouseProperties)
  
  SMD <- MD[,.(HHID,Region,HSex,HEduLevel,Decile,HActivityState,ServiceExp,MetrPrice,
               ServiceExp,FoodExpenditure,Total_Exp_Month,ProvinceCode,car,
               NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,Total_Exp_Month_nondurable,
               Weight,MetrPrice,Size,EqSizeOECD)]
  
  
  #SMD<-SMD[,Total_Exp98:=1.35*Total_Exp_Month_nondurable]
  SMD<-SMD[,Total_Exp98:=1.35*Total_Exp_Month]
  SMD<-SMD[,HomePrice98:=ServiceExp*(100/3)*5*1.35]

  WT<-SMD[ (Size==1 & Total_Exp98>40000000) |
          (Size==2 & Total_Exp98>50000000) |
          (Size==3 & Total_Exp98>60000000) |
          (Size==4 & Total_Exp98>70000000) |
          (Size>=5 & Total_Exp98>80000000)]
  
  
  HT1<-SMD[ProvinceCode==23 & HomePrice98>12000000000]
  HT2<-SMD[ProvinceCode!=23 & HomePrice98>9000000000]
  HT<-rbind(HT1,HT2)
  
  #HT1<-SMD[ProvinceCode==23 & MetrPrice98>360000]
  #HT2<-SMD[ProvinceCode!=23 & MetrPrice98>270000]
  #HT<-rbind(HT1,HT2)
  
  HousePrice<-HT[,.(HousePrice=sum(Weight*Size)),by=.(Region)]
  HousePrice2<-HT[,.(HousePrice2=sum(Weight*Size)),by=.(Region,Decile)]
  
  HighIncome<-WT[,.(HighIncome=sum(Weight*Size)),by=.(Region)]
  HighIncome2<-WT[,.(HighIncome2=sum(Weight*Size)),by=.(Region,Decile)]
  
  car<-SMD[car=="True",.(car=sum(Weight*Size)),by=.(Region)]
  car2<-SMD[car=="True",.(car2=sum(Weight*Size)),by=.(Region,Decile)]
  
  Active<-SMD[HActivityState=="Employed",.(Active=sum(Weight*Size)),by=.(Region)]
  Active2<-SMD[HActivityState=="Employed",.(Active2=sum(Weight*Size)),by=.(Region,Decile)]

  
  Having<-merge(HousePrice2,HighIncome2,all = TRUE)
  Having<-merge(Having,car2,all = TRUE)
  Having<-merge(Having,Active2,all = TRUE)
  Having[is.na(Having)] <- 0
  
  write.csv(HousePrice2,file = "HousePrice2.csv")
  write.csv(HighIncome2,file = "HighIncome2.csv")
  write.csv(car2,file = "car2.csv")
  write.csv(Active2,file = "Active2.csv")
  write.csv(Having,file = "Having.csv")
  }


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")