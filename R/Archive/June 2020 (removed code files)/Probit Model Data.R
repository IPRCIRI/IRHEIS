#Probit Model Data
#Providing Data For Model

#Zahra Shahidi
#2020

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Providing Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
year<-97
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/ProvinceCPI.xlsx",
                                        sheet = "Province"))
  I<-inflation[Year==year]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
 # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"demo.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Specific.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Job.rda"))
 # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  

  job<-job[,Region:=NULL]
  job<-job[,HActivityState:=NULL]
  job<-job[,Decile:=NULL]
  
 # demo[,HHID:=as.numeric(HHID)]
  #MD[,HHID:=as.numeric(HHID)]
  
  #MD<-merge(MD,demo[,.(NEmployed,NLiterate,HHID)],by=c("HHID"),all.x = T)
  MD<-merge(MD,job,by=c("HHID"),all.x = T)
  MD<-merge(MD,Specific,by=c("HHID"),all.x = T)
  MD<-merge(MD,I,by=c("ProvinceCode","Year","NewArea_Name"),all.x = T)
  MD<-merge(MD,IncomeTable[,.(HHID,NetIncome)],by=c("HHID"),all.x = T)
 # MD<-merge(MD,HHHouseProperties,by=c("HHID"),all.x = T)
  
  MD<-MD[!duplicated(MD$HHID)]
  MD<-MD[!is.na(FinalPoor)]
  
  goosht<-BigFData[FoodType=="Goosht"]
  goosht<-goosht[,Goosht_Grams:=FGrams]
  mahi<-BigFData[FoodType=="Mahi"]
  mahi<-mahi[,Mahi_Grams:=FGrams]
  morgh<-BigFData[FoodType=="Morgh"]
  morgh<-morgh[,Morgh_Grams:=FGrams]
  mive<-BigFData[FoodType=="Mive"]
  mive<-mive[,Mive_Grams:=FGrams]
  nan<-BigFData[FoodType=="Nan"]
  nan<-nan[,Nan_Grams:=FGrams]
  sibzamini<-BigFData[FoodType=="Sibzamini"]
  sibzamini<-sibzamini[,Sibzamini_Grams:=FGrams]
  makarooni<-BigFData[FoodType=="Makarooni"]
  makarooni<-makarooni[,Makarooni_Grams:=FGrams]
  khoshkbar<-BigFData[FoodType=="Khoshkbar"]
  khoshkbar<-khoshkbar[,Khoshkbar_Grams:=FGrams]
  
  MD<-merge(MD,goosht[,.(HHID,Goosht_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,mahi[,.(HHID,Mahi_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,morgh[,.(HHID,Morgh_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,mive[,.(HHID,Mive_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,nan[,.(HHID,Nan_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,sibzamini[,.(HHID,Sibzamini_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,makarooni[,.(HHID,Makarooni_Grams)],by=c("HHID"),all.x = T)
  MD<-merge(MD,khoshkbar[,.(HHID,Khoshkbar_Grams)],by=c("HHID"),all.x = T)
  
  
  
  for (col in c("Goosht_Grams","Mahi_Grams", "Morgh_Grams", "Mive_Grams", "Nan_Grams", 
                "Sibzamini_Grams", "Makarooni_Grams", "Khoshkbar_Grams"
  )) 
    MD[is.na(get(col)), (col) := 0]
  
  MD<-MD[,Goosht_Grams_Per:=Goosht_Grams/EqSizeCalory]
  MD<-MD[,Morgh_Grams_Per:=Morgh_Grams/EqSizeCalory]
  MD<-MD[,Mahi_Grams_Per:=Mahi_Grams/EqSizeCalory]
  MD<-MD[,Mive_Grams_Per:=Mive_Grams/EqSizeCalory]
  MD<-MD[,Nan_Grams_Per:=Nan_Grams/EqSizeCalory]
  MD<-MD[,Sibzamini_Grams_Per:=Sibzamini_Grams/EqSizeCalory]
  MD<-MD[,Makarooni_Grams_Per:=Makarooni_Grams/EqSizeCalory]
  MD<-MD[,Khoshkbar_Grams_Per:=Khoshkbar_Grams/EqSizeCalory]
  M<-MD[,weighted.mean(Goosht_Grams_Per,Weight*Size),]
  MD<-MD[,T_Meat_Grams_per:=Goosht_Grams_Per+Morgh_Grams_Per+Mahi_Grams_Per]
  MD<-MD[,T_Inferior_Grams_Per:=Nan_Grams_Per+Sibzamini_Grams_Per+Makarooni_Grams_Per]
  
  MD<-MD[,Ratio_TOriginalFoodExpenditure:=TOriginalFoodExpenditure/Total_Exp_Month]
  MD<-MD[,Ratio_Amusement_Exp:=Amusement_Exp/Total_Exp_Month]
  MD<-MD[,Ratio_HouseandEnergy_Exp:=HouseandEnergy_Exp/Total_Exp_Month]
  MD<-MD[,Ratio_MetrPrice:=MetrPrice/Total_Exp_Month]
  MD<-MD[,Ratio_Total_Exp_Month_nondurable:=Total_Exp_Month_nondurable/Total_Exp_Month]
  MD<-MD[,Ratio_Medical_Exp:=Medical_Exp/Total_Exp_Month]
  MD<-MD[,Ratio_Furniture_Exp:=Furniture_Exp/Total_Exp_Month]
  MD<-MD[,Ratio_Cloth_Exp:=Cloth_Exp/Total_Exp_Month]
  MD<-MD[,Ratio_ServiceExp:=ServiceExp/Total_Exp_Month]
  MD<-MD[,Ratio_Durable_Exp:=Durable_Exp/(Total_Exp_Month_nondurable+Durable_Exp)]
  MD<-MD[,Ratio_Hygiene_Exp:=Hygiene_Exp/Total_Exp_Month]

  
  
  
  MD<-MD[,Ratio_NUniv:=NUniv/Size]
  MD<-MD[,Square_NUniv:=Ratio_NUniv^2]
  
  MD<-MD[,Area_Per:=Area/Size]
  MD<-MD[,Log_Area_Per:=log(Area_Per)]
  MD<-MD[,Square_Area_Per:=Area_Per^2]
  
  MD<-MD[,Square_HAge:=HAge^2]
  
  MD<-MD[NDabestan!=0,Dabestan_Per:=Enrollment_Dabestan_G+Enrollment_Dabestan_NG/NDabestan]
  MD<-MD[NRahnamayi!=0,Rahnamayi_Per:=Enrollment_Rahnamayi_G+Enrollment_Rahnamayi_NG+Enrollment_Rahnamayi_Shabane/NRahnamayi]
  MD<-MD[NDabirestan!=0,Dabirestan_Per:=Enrollment_Dabirestan_G+Enrollment_Dabirestan_NG+Enrollment_Dabirestan_Shabane/NDabirestan]
  MD<-MD[NPish!=0,Pish_Per:=Enrollment_pish_G+Enrollment_pish_NG+Enrollment_pish_Shabane+KelasKonkoor/NPish]
  MD<-MD[,Education_Per:=Dabestan_Per+Rahnamayi_Per+Dabirestan_Per+Pish_Per]
  MD<-MD[,Ratio_Education_per:=Education_Per/(Total_Exp_Month_Per+Education_Per)]
  
  MD<-MD[,Visit:=Visit_Omoomi_G+Visit_Omoomi_NG+Visit_Motekhases_G+Visit_Motekhases_NG+Visit_Mama_G+Visit_Mama_NG+
           Visit_Ravanpezeshk_G+Visit_Ravanpezeshk_NG]
  MD<-MD[,Tooth:=tooth_G+tooth_NG+tooth_Jarahi_G+tooth_Jarahi_NG+Ortodensy_G+Ortodensy_NG]
  MD<-MD[,Medicine:=Visit+Tooth+Radiology_G+Radiology_NG+Phisioteraphy_G+Phisioteraphy_NG+Drug+Lab_G+Lab_NG+
           Ambulance_G+Ambulance_NG+Vaksan_G+Vaksan_NG]
  MD<-MD[,Ratio_Medicine:=Medicine/Total_Exp_Month]
  
  MD<-MD[,Real_Total_Exp_Month:=Total_Exp_Month/total]
  MD<-MD[,Real_FoodExpenditure:=FoodExpenditure/food]
  MD<-MD[,Real_Durable_Exp:=Durable_Exp/durables]
  
  MD<-MD[,Pub_Employee:=ifelse(Score_Job1==0,0,1)]
  MD<-MD[,Prv_Employee:=ifelse(Score_Job2==0,0,1)]
  MD<-MD[,Cooperative_Employee:=ifelse(Score_Job3==0,0,1)]
  MD<-MD[,Simple_Jobs_Staff:=ifelse(Score_Job1==9 | Score_Job2==9 | Score_Job3==9 | Score_Job4==9 | Score_Job5==9,1,0)]
  MD<-MD[,Opreators_machinery_equipment:=ifelse(Score_Job1==8 | Score_Job2==8 | Score_Job3==8 | Score_Job4==8 | Score_Job5==8,1,0)]
  MD<-MD[,Craftsman:=ifelse(Score_Job1==7 | Score_Job2==7 | Score_Job3==7 | Score_Job4==7 | Score_Job5==7,1,0)]
  MD<-MD[,Skilled_staff_agriculture_forestr_fishing:=ifelse(Score_Job1==6 | Score_Job2==6 | Score_Job3==6 | Score_Job4==6 | Score_Job5==6,1,0)]
  MD<-MD[,Staff_service_sales:=ifelse(Score_Job1==5 | Score_Job2==5 | Score_Job3==5 | Score_Job4==5 | Score_Job5==5,1,0)]
  MD<-MD[,Office_staff:=ifelse(Score_Job1==4 | Score_Job2==4 | Score_Job3==4 | Score_Job4==4 | Score_Job5==4,1,0)]
  MD<-MD[,Technician:=ifelse(Score_Job1==3 | Score_Job2==3 | Score_Job3==3 | Score_Job4==3 | Score_Job5==3,1,0)]
  MD<-MD[,Expert:=ifelse(Score_Job1==2 | Score_Job2==2 | Score_Job3==2 | Score_Job4==2 | Score_Job5==2,1,0)]
  MD<-MD[,Manager:=ifelse(Score_Job1==1 | Score_Job2==1 | Score_Job3==1 | Score_Job4==1 | Score_Job5==1,1,0)]
  
  MD<-MD[,Aid_Per:=Aid/Size]
  MD<-MD[,Ratio_Aid_Per:=Aid_Per/NetIncome]
  
  MD<-MD[Size>1,HFemale:=ifelse(HSex=="Female",1,0)]
  MD<-MD[,HSex:=ifelse(HSex=="Male",1,0)]
  MD<-MD[,HActivityState:=ifelse(HActivityState=="Employed",1,0)]
  
  
  MD<-MD[,Ratio_NEmployed:=NEmployed/Size]
  MD<-MD[,Square_NEmployed:=Ratio_NEmployed^2]
  
  MD<-MD[,Ratio_NLiterate:=NLiterate/Size]
  MD<-MD[,Square_NLiterate:=Ratio_NLiterate^2]
  
  MD<-MD[,Rental_House:=ifelse(tenure=="Rented",1,0)]
  MD<-MD[,Mortgage_House:=ifelse(tenure=="Mortgage",1,0)]
  MD<-MD[,Own_House:=ifelse(tenure=="OwnLandandBuilding",1,0)]
  MD<-MD[,Oghafi_House:=ifelse(tenure=="Apartment",1,0)]
  MD<-MD[,Metal_Skeleton:=ifelse(skeleton=="metal",1,0)]
  MD<-MD[,Concrete_Skeleton:=ifelse(skeleton=="concrete",1,0)]
  MD<-MD[,Other_Skeleton:=ifelse(skeleton=="other",1,0)]
  MD<-MD[,BrickSteel_StoneSteel_constmat:=ifelse(constmat=="BrickSteel_StoneSteel",1,0)]
  MD<-MD[,Brickwood_Stonewood_constmat:=ifelse(constmat=="Brickwood_Stonewood",1,0)]
  MD<-MD[,CementBlocks_constmat:=ifelse(constmat=="CementBlocks",1,0)]
  MD<-MD[,AllBrick_Stone_constmat:=ifelse(constmat=="AllBrick_Stone",1,0)]
  MD<-MD[,Allwood_constmat:=ifelse(constmat=="Allwood",1,0)]
  MD<-MD[,SundriedBrickwood_constmat:=ifelse(constmat=="SundriedBrickwood",1,0)]
  MD<-MD[,SundriedBrickmud_constmat:=ifelse(constmat=="SundriedBrickmud",1,0)]
  MD<-MD[,Phone:=ifelse(phone=="True",1,0)]
  MD<-MD[,Bike:=ifelse(bike=="True",1,0)]
  MD<-MD[,Radio:=ifelse(radio=="True",1,0)]
  MD<-MD[,Cassette:=ifelse(cassette=="True",1,0)]
  MD<-MD[,Tvbw:=ifelse(tvbw=="True",1,0)]
  MD<-MD[,Tvcr:=ifelse(tvcr=="True",1,0)]
  MD<-MD[,Vcr:=ifelse(vcr=="True",1,0)]
  MD<-MD[,CellPhone:=ifelse(cellphone=="True",1,0)]
  MD<-MD[,Freezer:=ifelse(freezer=="True",1,0)]
  MD<-MD[,Refrigerator:=ifelse(refrigerator=="True",1,0)]
  MD<-MD[,Frez_Refrig:=ifelse(frez_refrig=="True",1,0)]
  MD<-MD[,Oven:=ifelse(oven=="True",1,0)]
  MD<-MD[,Vacuum:=ifelse(vacuum=="True",1,0)]
  MD<-MD[,Washer:=ifelse(washer=="True",1,0)]
  MD<-MD[,Sewing:=ifelse(sewing=="True",1,0)]
  MD<-MD[,Fan:=ifelse(fan=="True",1,0)]
  MD<-MD[,Cooler_Water_Movable:=ifelse(pipewater=="True",1,0)]
  MD<-MD[,Cooler_Gas_Movable:=ifelse(cooler_gas_movable=="True",1,0)]
  MD<-MD[,Dishwasher:=ifelse(dishwasher=="True",1,0)]
  MD<-MD[,Microwave:=ifelse(Microwave=="True",1,0)]
  MD<-MD[,None:=ifelse(none=="True",1,0)]
  MD<-MD[,Pipewater:=ifelse(pipewater=="True",1,0)]
  MD<-MD[,Electricity:=ifelse(electricity=="True",1,0)]
  MD<-MD[,Pipegas:=ifelse(pipegas=="True",1,0)]
  MD<-MD[,Kitchen:=ifelse(kitchen=="True",1,0)]
  MD<-MD[,Cooler:=ifelse(cooler=="True",1,0)]
  MD<-MD[,CentralCooler:=ifelse(centralcooler=="True",1,0)]
  MD<-MD[,CentralHeat:=ifelse(centralheat=="True",1,0)]
  MD<-MD[,Pakage:=ifelse(pakage=="True",1,0)]
  MD<-MD[,Cooler_Gas:=ifelse(cooler_gas=="True",1,0)]
  #MD<-MD[,UnexpectedEvent_Month:=ifelse(party_month=="True" | ceremony_month=="True" | homerepaire_month=="True" | bastari_month=="True" | operation_month=="True",1,0) ]
  
  MD<-MD[,Car:=ifelse(car=="True",1,0)]
  MD<-MD[,Bathroom:=ifelse(bathroom=="True",1,0)]
  MD<-MD[,Computer:=ifelse(computer=="True",1,0)]
  MD<-MD[,Internet:=ifelse(internet=="True",1,0)]
  MD<-MD[,Motorcycle:=ifelse(motorcycle=="True",1,0)]
  MD<-MD[,Water:=ifelse(pipewater=="True",1,0)]
  MD<-MD[,Gas:=ifelse(pipegas=="True",1,0)]
  MD<-MD[,Sewage:=ifelse(ego=="True",1,0)]
  MD<-MD[,KarosineCook:=ifelse(cookfuel=="karosine",1,0)]
  MD<-MD[,KarosineHeat:=ifelse(heatfuel=="karosine",1,0)]
  MD<-MD[,GasHeat:=ifelse(heatfuel=="gas",1,0)]
  MD<-MD[,Pipedgas_CookFuel:=ifelse(cookfuel=="pipedgas",1,0)]
  MD<-MD[,Pipedgas_HeatFuel:=ifelse(heatfuel=="pipedgas",1,0)]
  MD<-MD[,Pipedgas_HotWater:=ifelse(hotwater=="pipedgas",1,0)]
  
  MD[ProvinceCode==0,ProvinceName:="Markazi"]
  MD[ProvinceCode==1,ProvinceName:="Gilan"]
  MD[ProvinceCode==2,ProvinceName:="Mazandaran"]
  MD[ProvinceCode==3,ProvinceName:="Az_Sharghi"]
  MD[ProvinceCode==4,ProvinceName:="Az_Gharbi"]
  MD[ProvinceCode==5,ProvinceName:="Kermanshah"]
  MD[ProvinceCode==6,ProvinceName:="Khoozestan"]
  MD[ProvinceCode==7,ProvinceName:="Fars"]
  MD[ProvinceCode==8,ProvinceName:="Kerman"]
  MD[ProvinceCode==9,ProvinceName:="Khorasan_Razavi"]
  MD[ProvinceCode==10,ProvinceName:="Esfahan"]
  MD[ProvinceCode==11,ProvinceName:="Sistan"]
  MD[ProvinceCode==12,ProvinceName:="Kordestan"]
  MD[ProvinceCode==13,ProvinceName:="Hamedan"]
  MD[ProvinceCode==14,ProvinceName:="Chaharmahal"]
  MD[ProvinceCode==15,ProvinceName:="Lorestan"]
  MD[ProvinceCode==16,ProvinceName:="Ilam"]
  MD[ProvinceCode==17,ProvinceName:="Kohkilooye"]
  MD[ProvinceCode==18,ProvinceName:="Booshehr"]
  MD[ProvinceCode==19,ProvinceName:="Zanjan"]
  MD[ProvinceCode==20,ProvinceName:="Semnan"]
  MD[ProvinceCode==21,ProvinceName:="Yazd"]
  MD[ProvinceCode==22,ProvinceName:="Hormozgan"]
  MD[ProvinceCode==23,ProvinceName:="Tehran"]
  MD[ProvinceCode==24,ProvinceName:="Ardebil"]
  MD[ProvinceCode==25,ProvinceName:="Ghom"]
  MD[ProvinceCode==26,ProvinceName:="Ghazvin"]
  MD[ProvinceCode==27,ProvinceName:="Golestan"]
  MD[ProvinceCode==28,ProvinceName:="Khorasan_Shomali"]
  MD[ProvinceCode==29,ProvinceName:="Khorasan_Jonoobi"]
  MD[ProvinceCode==30,ProvinceName:="Alborz"]
  
  Data<-MD[,.(HHID,Region,cluster3,NewArea,NewArea_Name,Year,ProvinceName,HSex,HAge,Square_HAge,NKids,HActivityState,HEduYears,
              Size,Weight,NUniv,Ratio_NUniv,Square_NUniv,EqSizeOECD,EqSizeCalory,FoodKCaloriesHH_Per,FoodProtein_Per,Ratio_TOriginalFoodExpenditure,
              TFoodKCaloriesHH_Per,Rooms,Area,Area_Per,Log_Area_Per,Square_Area_Per,Ratio_Amusement_Exp,Ratio_MetrPrice,Ratio_Hygiene_Exp,
              Ratio_Total_Exp_Month_nondurable,Ratio_Medical_Exp,Ratio_Furniture_Exp,Ratio_Cloth_Exp,Ratio_HouseandEnergy_Exp,Ratio_ServiceExp,
              Ratio_Durable_Exp,Ratio_Education_per,Ratio_Medicine,Decile,Percentile,InitialPoor,FinalPoor,
              FPLine,PovertyLine,Engel,HHEngle,Total_Exp_Month_Per_nondurable,Goosht_Grams_Per,Morgh_Grams_Per,
              Mahi_Grams_Per,Mive_Grams_Per,Nan_Grams_Per,Sibzamini_Grams_Per,Makarooni_Grams_Per,Khoshkbar_Grams_Per,T_Meat_Grams_per,
              T_Inferior_Grams_Per,Pub_Employee,Prv_Employee,Cooperative_Employee,Simple_Jobs_Staff,Opreators_machinery_equipment,Craftsman,
              Skilled_staff_agriculture_forestr_fishing,Staff_service_sales,Office_staff,Technician,Expert,Manager,Ratio_Aid_Per,HFemale,
              NEmployed,Ratio_NEmployed,Square_NEmployed,NLiterate,Ratio_NLiterate,Square_NLiterate,Rental_House,Mortgage_House,Own_House,
              Oghafi_House,Metal_Skeleton,Concrete_Skeleton,Other_Skeleton,BrickSteel_StoneSteel_constmat,Brickwood_Stonewood_constmat,
              CementBlocks_constmat,AllBrick_Stone_constmat,Allwood_constmat,SundriedBrickwood_constmat,SundriedBrickmud_constmat,Car,Bike,
              Radio,Bathroom,Computer,Internet,Motorcycle,Cassette,Tvbw,Tvcr,Vcr,CellPhone,Freezer,Refrigerator,Frez_Refrig,Oven,Vacuum,Washer,
              Sewing,Fan,Cooler_Water_Movable,Cooler_Gas_Movable,Dishwasher,Microwave,None,Pipewater,Electricity,Pipegas,Kitchen,Cooler,
              CentralCooler,CentralHeat,Pakage,Cooler_Gas,Water,Gas,Sewage,KarosineCook,KarosineHeat,GasHeat,Pipedgas_CookFuel,Pipedgas_HeatFuel,Pipedgas_HotWater)]
  save(Data, file=paste0(Settings$HEISProcessedPath,"Y",year,"Data.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
