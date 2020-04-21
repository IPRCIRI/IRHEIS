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

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/ProvinceCPI.xlsx",
                                        sheet = "Province"))
  I<-inflation[Year==year]
  
  load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Specific.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Job.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  job<-job[,Region:=NULL]
  job<-job[,HActivityState:=NULL]
  
  MD<-merge(MD,job,by=("HHID"))
  MD<-merge(MD,Specific,by=c("HHID"))
  MD<-merge(MD,I,by=c("ProvinceCode","Year","NewArea_Name"))
  MD<-merge(MD,IncomeTable[,.(HHID,NetIncome)],by=c("HHID"))
  MD<-merge(MD,HHHouseProperties,by=c("HHID"))
  
  MD<-MD[,Ratio_NUniv:=NUniv/Size]
  MD<-MD[,Square_NUniv:=Ratio_NUniv^2]
  
  MD<-MD[,Area_Per:=Area/Size]
  MD<-MD[,Log_Area_Per:=log(Area_Per)]
  MD<-MD[,Square_Area_Per:=Area_Per^2]
  
  MD<-MD[,Square_HAge:=HAge^2]
  
  MD<-MD[,SheepMeatExpenditure_Per:=SheepMeatExpenditure/EqSizeCalory]
  MD<-MD[,CowMeatExpenditure_Per:=CowMeatExpenditure/EqSizeCalory]
  MD<-MD[,CamelMeatExpenditure_Per:=CamelMeatExpenditure/EqSizeCalory]
  MD<-MD[,RedMeat_Per:=SheepMeatExpenditure_Per+CowMeatExpenditure_Per+CamelMeatExpenditure_Per]
  MD<-MD[,WhiteMeat_Per:=BirdsMeatExpenditure/EqSizeCalory]
  MD<-MD[,Meat_Per:=RedMeat_Per+WhiteMeat_Per]
  
  MD<-MD[,R_SheepMeatExpenditure_Per:=SheepMeatExpenditure_Per/r_meat]
  MD<-MD[,R_CowMeatExpenditure_Per:=CowMeatExpenditure_Per/r_meat]
  MD<-MD[,R_CamelMeatExpenditure_Per:=CamelMeatExpenditure_Per/r_meat]
  MD<-MD[,R_RedMeat_Per:=RedMeat_Per/r_meat]
  MD<-MD[,R_WhiteMeat_Per:=WhiteMeat_Per/w_meat]
  MD<-MD[,R_Meat_Per:=R_RedMeat_Per+R_WhiteMeat_Per]
  
  MD<-MD[NDabestan!=0,Dabestan_Per:=Enrollment_Dabestan_G+Enrollment_Dabestan_NG/NDabestan]
  MD<-MD[NRahnamayi!=0,Rahnamayi_Per:=Enrollment_Rahnamayi_G+Enrollment_Rahnamayi_NG+Enrollment_Rahnamayi_Shabane/NRahnamayi]
  MD<-MD[NDabirestan!=0,Dabirestan_Per:=Enrollment_Dabirestan_G+Enrollment_Dabirestan_NG+Enrollment_Dabirestan_Shabane/NDabirestan]
  MD<-MD[NPish!=0,Pish_Per:=Enrollment_pish_G+Enrollment_pish_NG+Enrollment_pish_Shabane+KelasKonkoor/NPish]
  MD<-MD[,Education_Per:=Dabestan_Per+Rahnamayi_Per+Dabirestan_Per+Pish_Per]
  MD<-MD[,Ratio_Education_Per:=Education_Per/Total_Exp_Month_Per+Education_Per]
  
  MD<-MD[,Visit:=Visit_Omoomi_G+Visit_Omoomi_NG+Visit_Motekhases_G+Visit_Motekhases_NG+Visit_Mama_G+Visit_Mama_NG+
           Visit_Ravanpezeshk_G+Visit_Ravanpezeshk_NG]
  MD<-MD[,Tooth:=tooth_G+tooth_NG+tooth_Jarahi_G+tooth_Jarahi_NG+Ortodensy_G+Ortodensy_NG]
  MD<-MD[,Medicine:=Visit+Tooth+Radiology_G+Radiology_NG+Phisioteraphy_G+Phisioteraphy_NG+Drug+Lab_G+Lab_NG+
           Ambulance_G+Ambulance_NG+Vaksan_G+Vaksan_NG]
  MD<-MD[,R_Medicine:=Medicine/medicine]
  MD<-MD[,Ratio_R_Medicine:=R_Medicine/Total_Exp_Month]
  
  MD<-MD[,Real_Total_Exp_Month:=Total_Exp_Month/total]
  MD<-MD[,Real_FoodExpenditure:=FoodExpenditure/food]
  MD<-MD[,Real_Durable_Exp:=NonFreeDurable_Exp/durables]
  
  MD<-MD[,Pub_Employee:=ifelse(Main_Job_Name_Pub==0,0,1)]
  MD<-MD[,Prv_Employee:=ifelse(Main_Job_Name_Prv==0,0,1)]
  MD<-MD[,Cooperative_Employee:=ifelse(Main_Job_Name_Cooperative==0,0,1)]
  MD<-MD[,Simple_Jobs_Staff:=ifelse(Job_Main_Code_Pub==9 | Job_Main_Code_Prv==9 | Job_Main_Code_Cooperative==9 | Job_Main_Code_Buss==9 | Job_Main_Code_Agri==9,1,0)]
  MD<-MD[,Opreators_machinery_equipment:=ifelse(Job_Main_Code_Pub==8 | Job_Main_Code_Prv==8 | Job_Main_Code_Cooperative==8 | Job_Main_Code_Buss==8 | Job_Main_Code_Agri==8,1,0)]
  MD<-MD[,Craftsman:=ifelse(Job_Main_Code_Pub==7 | Job_Main_Code_Prv==7 | Job_Main_Code_Cooperative==7 | Job_Main_Code_Buss==7 | Job_Main_Code_Agri==7,1,0)]
  MD<-MD[,Skilled_staff_agriculture_forestr_fishing:=ifelse(Job_Main_Code_Pub==6 | Job_Main_Code_Prv==6 | Job_Main_Code_Cooperative==6 | Job_Main_Code_Buss==6 | Job_Main_Code_Agri==6,1,0)]
  MD<-MD[,Staff_service_sales:=ifelse(Job_Main_Code_Pub==5 | Job_Main_Code_Prv==5 | Job_Main_Code_Cooperative==5 | Job_Main_Code_Buss==5 | Job_Main_Code_Agri==5,1,0)]
  MD<-MD[,Office_staff:=ifelse(Job_Main_Code_Pub==4 | Job_Main_Code_Prv==4 | Job_Main_Code_Cooperative==4 | Job_Main_Code_Buss==4 | Job_Main_Code_Agri==4,1,0)]
  MD<-MD[,Technician:=ifelse(Job_Main_Code_Pub==3 | Job_Main_Code_Prv==3 | Job_Main_Code_Cooperative==3 | Job_Main_Code_Buss==3 | Job_Main_Code_Agri==3,1,0)]
  MD<-MD[,Expert:=ifelse(Job_Main_Code_Pub==2 | Job_Main_Code_Prv==2 | Job_Main_Code_Cooperative==2 | Job_Main_Code_Buss==2 | Job_Main_Code_Agri==2,1,0)]
  MD<-MD[,Manager:=ifelse(Job_Main_Code_Pub==1 | Job_Main_Code_Prv==1 | Job_Main_Code_Cooperative==1 | Job_Main_Code_Buss==1 | Job_Main_Code_Agri==1,1,0)]
  
  MD<-MD[,Aid_Per:=Aid/Size]
  MD<-MD[,Ratio_Aid_Per:=Aid_Per/NetIncome]
  
  MD<-MD[Size>1,HFemale:=ifelse(HSex=="Female",1,0)]
  MD<-MD[,HSex:=ifelse(HSex=="Male",1,0)]
  MD<-MD[,HActivityState:=ifelse(HActivityState=="Employed",1,0)]
  
  
  MD<-MD[,Ratio_NEmployed:=NEmployed/Size]
  MD<-MD[,Square_NEmployed:=Ratio_NEmployed^2]
  
  MD<-MD[,Ratio_NLiterate:=NLiterate/Size]
  MD<-MD[,Square_NLiterate:=Ratio_NLiterate^2]
  
  MD<-MD[,RentalHouse:=ifelse(tenure=="Rented",1,0)]
  MD<-MD[,Phone:=ifelse(phone=="True",1,0)]
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
              Size,Weight,NUniv,Ratio_NUniv,Square_NUniv,EqSizeOECD,EqSizeCalory,FoodKCaloriesHH_Per,FoodProtein_Per,TOriginalFoodExpenditure_Per,
              TFoodKCaloriesHH_Per,Rooms,Area,Area_Per,Log_Area_Per,Square_Area_Per,Amusement_Exp,HouseandEnergy_Exp,MetrPrice,Hygiene_Exp,
              Total_Exp_Month_Per_nondurable,NetIncome,Medical_Exp,Furniture_Exp,Cloth_Exp,HouseandEnergy_Exp,ServiceExp,NonFreeDurable_Exp,Decile,
              Decile_Nominal,Percentile,Percentile_Nominal,InitialPoor,FinalPoor,FPLine,PovertyLine,Engel,HHEngle,RedMeat_Per,WhiteMeat_Per,Meat_Per,
              R_RedMeat_Per,R_WhiteMeat_Per,R_Meat_Per,Dabestan_Per,Rahnamayi_Per,Dabirestan_Per,Pish_Per,Education_Per,Ratio_Education_Per,Visit,
              Tooth,Medicine,R_Medicine,Ratio_R_Medicine,Real_Total_Exp_Month,Real_FoodExpenditure,Real_Durable_Exp,Pub_Employee,Prv_Employee,
              Cooperative_Employee,Simple_Jobs_Staff,Opreators_machinery_equipment,Craftsman,Skilled_staff_agriculture_forestr_fishing,
              Staff_service_sales,Office_staff,Technician,Expert,Manager,Aid,Aid_Per,Ratio_Aid_Per,HFemale,NEmployed,Ratio_NEmployed,Square_NEmployed,
              NLiterate,Ratio_NLiterate,Square_NLiterate,RentalHouse,Car,Bathroom,Computer,Motorcycle,Water,Gas,Sewage,KarosineCook,KarosineHeat,GasHeat)]
  save(Data, file=paste0(Settings$HEISProcessedPath,"Y",year,"Data.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
