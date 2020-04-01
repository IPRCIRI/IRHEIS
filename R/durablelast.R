# 401-Mereg all data.R
# 2019: Arin Shahbazian

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Mereg all data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(janitor)
library(dplyr)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  
  
  for(G in c("Durables","ClothDurables","ProtectHouseDurables",
             "FurnitureDurables","HouseDurableInstruments",
             "UtensilDurables","GardenInstruDurables",
             "HouseCommodityServDurables","MedicalInstruDurables",
             "MedicalStandingDurables","HospitalDurables",
             "BuyingAutoDurables","TransportInstruDurables",
             "VisualEquipDurables","OtherinstruDurables",
             "BookNewspaperDurables","TravelDurables",
             "PrimaryDurables","HighschoolDurables",
             "PreDurables","UniDurables",
             "OtherEduDurables","PersonalOtherDurables",
             "SocialProtDurables","InsuranceDurables",
             "OtherfinancialDurables","OtherserviceDurables",
             "TransitionsDurables","ClothDurable_WomanExp","ClothDurable_WomanExp2",
             "ClothDurable_OtherExp","ClothDurable_OtherExp2",
             "ClothDurable_RepairExp","ClothDurable_RepairExp2",
             "HouseRepairMaterialsExp","HouseRepairMaterialsExp2",
             "HouseRepairServicesExp","HouseRepairServicesExp2",
             "FurnitureCarpetDurableData",
             "OtherFurnitureExp","OtherFurnitureExp2","FurnituregoodsExp","FurnituregoodsExp2",
             "CarpetsExp","CarpetsExp2","Repair_FurnitureExp","Repair_FurnitureExp2",
             "MainEquipmentsExp","MainEquipmentsExp2","MinorEquipmentsExp","MinorEquipmentsExp2",
             "RepairRentEquipmentsExp","RepairRentEquipmentsExp2",
             "FoodOtherUtensilExp","FoodOtherUtensilExp2","DurableEquipmentsExp","DurableEquipmentsExp2",
             "HouseServicesRentExp","HouseServicesRentExp2","MedicalDurableData",
             "MedicalDurableEquipmentsExp","MedicalDurableEquipmentsExp2","MedicalDurableEquipmentsExp3",
             "MedicalDurableServicesExp","MedicalDurableServicesExp2","MedicalDurableServicesExp3",
             "HospitalServicesExp","HospitalServicesExp2","HospitalServicesExp3","TransportationDurableData",
             "BuyingAutoDurables","TransportInstruDurables",
             "BuyingAutomobileExp","BuyingAutomobileExp2","BuyingMotorExp","BuyingMotorExp2",
             "BuyingBicycleExp","BuyingBicycleExp2",
             "AccessoryExp","AccessoryExp2","CarRepairServiceExp","CarRepairServiceExp2",
             "OtherCar_DurableServicesExp","OtherCar_DurableServicesExp2",
             "TelDurableEquipExp","TelDurableEquipExp2","TelDurableEquipExp3","TelDurableEquipExp4",
             "VisualEquipDurables","OtherinstruDurables","OtherAmusingDurables",
             "BookNewspaperDurables","BookNewspaperDurables2","TravelDurables","AcousticProjective_EquipExp",
             "AcousticProjective_EquipExp2","CameraDurableEquipExp","CameraDurableEquipExp2",
             "PCetcExp","PCetcExp2","RepairAcousticProjectiveExp","RepairAcousticProjectiveExp2",
             "OtherAmusingDurableExp","OtherAmusingDurableExp2",
             "MusicEquipDurableExp","MusicEquipDurableExp2",
             "RepairAmusingDurableExp","RepairAmusingDurableExp2","GamesToysExp3",
             "GamesToysExp","GamesToysExp2","SportsEquipExp","SportsEquipExp2",
             "PlantsGardenExp","PlantsGardenExp2","PetsExp","PetsExp2",
             "Books_curriculumExp","Books_curriculumExp2",
             "Books_NoncurriculumExp","Books_NoncurriculumExp2",
             "TravelsExp","TravelsExp2","TravelsExp3","TravelsExp4","EducationData",
             "PrimaryschoolRegExp","PrimaryschoolRegExp2",
             "HighschoolRegExp","HighschoolRegExp2",
             "PreUniversityRegExp","PreUniversityRegExp2",
             "UniversityRegExp","UniversityRegExp2",
             "OtherClassRegExp","OtherClassRegExp2","OtherDurableData",
             "JewelryandWatchExp","JewelryandWatchExp2","BurialExp","BurialExp2",
             "Social_ProtectsExp","Social_ProtectsExp2","Premium_NonMedicalExp","Premium_NonMedicalExp2",
             "Premium_HouseExp","Premium_HouseExp2","Premium_MedicalExp","Premium_MedicalExp2",
             "Premium_TransportationExp","Premium_TransportationExp2",
             "Premium_OtherExp","Premium_OtherExp2","Financial_MediatorExp","Financial_MediatorExp2",
             "Financial_OtherExp","Financial_OtherExp2","Other_Nonsorted_ServicesExp",
             "Other_Nonsorted_ServicesExp2","ReligiousExp","ReligiousExp2",
             "Damage_AtonementExp","Damage_AtonementExp2","Durable12","Durable13","Durable11",
             "Durable011","Durable012","Durable013",
             "Durable021","Durable022",
             "Durable31","Durable32","Durable33","Durable34","Durable41",
             "Durable42","Durable43","Durable51","Durable52","Durable53",
             "Durable61","Durable62","Durable7","Durable81","Durable82","Durable83",
             "Durable9","Durable101","Durable102","Durable121","Durable122","Durable123",
             "Durable131","Durable132","Durable141","Durable142",
             "Durable151","Durable152","Durable153","Durable154","Durable161",
             "Durable162","Durable163","Durable17","Durable18",
             "Durable19","Durable20","Durable21","Durable22","Durable231","Durable232",
             "Durable241","Durable242","Durable243","Durable251","Durable252",
             "Durable261","Durable262","Durable27","Durable281","Durable282")){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
  }
  
  HHBase<-HHBase[,.(HHID)]
  
  TotalDurable<-merge(HHBase,DurableData,all.x=TRUE)                  
  TotalDurable<-merge(TotalDurable,Durable011,all.x=TRUE)    
  TotalDurable<-merge(TotalDurable,Durable012,all.x=TRUE)  
  TotalDurable<-merge(TotalDurable,Durable013,all.x=TRUE)  
  TotalDurable<-merge(TotalDurable,Durable021,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable022,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable31,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable32,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable33,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable34,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable41,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable42,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable43,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable51,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable52,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable53,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable61,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable62,all.x=TRUE, allow.cartesian=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable7,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable81,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable82,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable83,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable9,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable101,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable102,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable11,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable121,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable122,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable123,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable131,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable132,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable141,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable142,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable151,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable152,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable153,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable154,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable161,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable162,all.x=TRUE) 
  TotalDurable<-merge(TotalDurable,Durable163,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable17,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable18,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable19,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable20,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable21,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable22,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable231,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable232,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable241,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable242,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable243,all.x=TRUE,allow.cartesian=TRUE)
  TotalDurable<-merge(TotalDurable,Durable251,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable252,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable261,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable262,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable27,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable281,all.x=TRUE)
  TotalDurable<-merge(TotalDurable,Durable282,all.x=TRUE)
  
  
  TotalDurable[is.na(TotalDurable)] <- 0
  TotalDurable<-distinct(TotalDurable,HHID,.keep_all= TRUE)
  
  names(TotalDurable)
  
  save(TotalDurable, file = paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
