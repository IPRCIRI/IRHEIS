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
  
  MD <- merge(MD,HHHouseProperties)
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",97,"TbussW2.rda"))
  load(file = paste0(Settings$HEISProcessedPath,"Y",97,"TAgriW2.rda"))
  
  MD<-merge(MD,TbussW2,all.x = TRUE)
  MD<-merge(MD,TAgriW2,all.x = TRUE)
  
  SMD <- MD[,.(HHID,Region,HSex,HEduLevel,Decile,tenure,HActivityState,ServiceExp,MetrPrice,
               ServiceExp,FoodExpenditure,Total_Exp_Month,ProvinceCode,car,
               NewArea,NewArea2,Total_Exp_Month_Per_nondurable,TOriginalFoodExpenditure_Per,Total_Exp_Month_nondurable,
               Weight,MetrPrice,Size,EqSizeOECD,WorkType.x,WorkType.y)]
  SMD[is.na(SMD)] <- 0
  
  #SMD<-SMD[,Total_Exp98:=1.35*Total_Exp_Month_nondurable]
  SMD<-SMD[,Total_Exp98:=1.35*Total_Exp_Month]
  SMD<-SMD[,HomePrice98:=ServiceExp*(100/3)*5*2]
  
  WT<-SMD[ (Size==1 & Total_Exp98>40000000) |
             (Size==2 & Total_Exp98>50000000) |
             (Size==3 & Total_Exp98>60000000) |
             (Size==4 & Total_Exp98>70000000) |
             (Size>=5 & Total_Exp98>80000000) ]
  
  SMD<-SMD[ ,HighIncome:=ifelse((Size==1 & Total_Exp98>40000000) |
             (Size==2 & Total_Exp98>50000000) |
             (Size==3 & Total_Exp98>60000000) |
             (Size==4 & Total_Exp98>70000000) |
             (Size>=5 & Total_Exp98>80000000),1,0) ]
  
  SMD<-SMD[,Size2:=ifelse(Size<5,Size,5)]
  
  

  
  HighIncomeProv<-SMD[,weighted.mean(HighIncome,Weight),by=ProvinceCode]
  HighIncomeSize<-SMD[,weighted.mean(HighIncome,Weight),by=Size2]
  
  HT1<-SMD[ProvinceCode==23 & HomePrice98>12000000000]
  HT2<-SMD[ProvinceCode!=23 & HomePrice98>9000000000]
  HT <-rbind(HT1,HT2)
  
  SMD<-SMD[ ,HighHouse:=ifelse((ProvinceCode==23 & HomePrice98>12000000000) |
                                  (ProvinceCode!=23 & HomePrice98>9000000000),1,0) ]
  HighHouseProv<-SMD[,weighted.mean(HighHouse,Weight),by=ProvinceCode]
  HighHouseDecile<-SMD[,weighted.mean(HighHouse,Weight),by=c("Decile")]
  
  #HT1<-SMD[ProvinceCode==23 & MetrPrice98>360000]
  #HT2<-SMD[ProvinceCode!=23 & MetrPrice98>270000]
  #HT<-rbind(HT1,HT2)
  
  HousePrice <-HT[,.(HousePrice=sum(Weight*Size))]
  HousePrice2<-HT[,.(HousePrice2=sum(Weight*Size)),by=.(Region,Decile)]
  
  HighIncome <-WT[,.(HighIncome=sum(Weight*Size))]
  HighIncome2<-WT[,.(HighIncome2=sum(Weight*Size)),by=.(Region,Decile)]
  
  car <-SMD[car=="True",.(car=sum(Weight*Size))]
  car2<-SMD[car=="True",.(car2=sum(Weight*Size)),by=.(Region,Decile)]
  car3<-SMD[,weighted.mean(car=="True",Weight),by=.(Decile)]
  
  Active <-SMD[HActivityState=="Employed",.(Active=sum(Weight*Size))]
  Active2<-SMD[HActivityState=="Employed",.(Active2=sum(Weight*Size)),by=.(Region,Decile)]
  Active3<-SMD[,weighted.mean(HActivityState=="Employed",Weight),by=.(Region,Decile)]
  
  House <-SMD[tenure=="OwnLandandBuilding" | tenure=="Apartment" ,.(House=sum(Weight*Size)),by=.(Decile)]
  House2<-SMD[tenure=="OwnLandandBuilding" | tenure=="Apartment",.(House2=sum(Weight*Size)),by=.(Region,Decile)]
  House3<-SMD[,weighted.mean((tenure=="OwnLandandBuilding" | tenure=="Apartment"),Weight),by=.(Region,Decile)]
  
  
  All3 <-SMD[(tenure=="OwnLandandBuilding" | tenure=="Apartment") & HActivityState=="Employed" & car=="True" ,.(All3=sum(Weight*Size)),by=.(Decile)]
  All32<-SMD[(tenure=="OwnLandandBuilding" | tenure=="Apartment") & HActivityState=="Employed" & car=="True",.(All32=sum(Weight*Size)),by=.(Region,Decile)]
  All33<-SMD[,weighted.mean((tenure=="OwnLandandBuilding" | tenure=="Apartment") & 
                              HActivityState=="Employed" & car=="True",Weight),
                              by=.(Region,Decile)]
  All34<-SMD[,weighted.mean((tenure=="OwnLandandBuilding" | tenure=="Apartment") & 
                              HActivityState=="Employed" & car=="True",Weight),
             by=.(ProvinceCode)]
  
  AllFinal<-SMD[,weighted.mean(((tenure=="OwnLandandBuilding" | tenure=="Apartment") & 
                                     HActivityState=="Employed" & car=="True"),Weight),
                   by=.(Decile)]
  
  EmployeeProv<-SMD[,weighted.mean((WorkType.x==4 | WorkType.y==4),Weight),
             by=.(ProvinceCode)]
  EmployeeDecile<-SMD[,weighted.mean((WorkType.x==4 | WorkType.y==4),Weight),
                    by=.(Decile)]

  
  TotalProv<-SMD[,weighted.mean(((tenure=="OwnLandandBuilding" | tenure=="Apartment") & 
                              HActivityState=="Employed" & car=="True") |
                                (ProvinceCode==23 & HomePrice98>12000000000) |
                                (ProvinceCode!=23 & HomePrice98>9000000000) |
                                (WorkType.x==4 | WorkType.y==4) |
                                (Size==1 & Total_Exp98>40000000) |
                                (Size==2 & Total_Exp98>50000000) |
                                (Size==3 & Total_Exp98>60000000) |
                                (Size==4 & Total_Exp98>70000000) |
                                (Size>=5 & Total_Exp98>80000000),Weight),
             by=.(ProvinceCode)]
  
  Totaldecile<-SMD[,weighted.mean(((tenure=="OwnLandandBuilding" | tenure=="Apartment") & 
                                   HActivityState=="Employed" & car=="True") |
                                  (ProvinceCode==23 & HomePrice98>12000000000) |
                                  (ProvinceCode!=23 & HomePrice98>9000000000) |
                                    (WorkType.x==4 | WorkType.y==4) |
                                  (Size==1 & Total_Exp98>40000000) |
                                  (Size==2 & Total_Exp98>50000000) |
                                  (Size==3 & Total_Exp98>60000000) |
                                  (Size==4 & Total_Exp98>70000000) |
                                  (Size>=5 & Total_Exp98>80000000),Weight),
                 by=.(Decile)]
  
  
  Having<-merge(HousePrice2,HighIncome2,all = TRUE)
  Having<-merge(Having,All32,all = TRUE)
  Having[is.na(Having)] <- 0
  
  #write.csv(HousePrice2,file = "HousePrice2.csv")
  #write.csv(HighIncome2,file = "HighIncome2.csv")
  #write.csv(Having,file = "Having.csv")
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")