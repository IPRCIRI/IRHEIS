#Breaking Durable goods in two section
# 
# Copyright © 2020:  Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Breaking Durable goods in two section =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))

  g <- c("Cheragh_Electricy",
         "Parde",
         "Ayne",
         "Asar_Honari",
         "Sanaye_Dasti",
         "Gahvare",
         "Lavazem_Safar",
         "HazineNasb",
         "Takhtekhab",
         "Komod",
         "Anvae_Miz",
         "Mobl",
         "Ghali_Dastbaf",
         "Ghali_Mashini",
         "Mooket",
         "Ziloo",
         "Kafpoosh_Nasb",
         "Lavazem_TamirMobleman",
         "Hazine_TamirMobleman",
         "Yakhchal",
         "freezer2",
         "Mashin_Lebasshooyi",
         "Otoo",
         "OjaghGaz",
         "KhorakPaz",
         "CilandreGaz",
         "Tahvie",
         "Bokhari",
         "Cooler_Gaz",
         "Jaroobarghi",
         "Charkh_Khayati",
         "Machine_bafandei",
         "Sandoogh",
         "Generator",
         "LavazemRepair3",
         "HazineNasb3",
         "Polopaz",
         "ghahvejoosh",
         "Samavar",
         "Abmivergiri",
         "Panke",
         "Other_Barghi",
         "LavazemRepair4",
         "hazine_Ejare",
         "Patoo_Barghi",
         "Capsule_O2",
         "Visit_Jarahi_G",
         "Visit_Jarahi_NG",
         "Zibai_Jarahi_G",
         "Zibai_Jarahi_NG",
         "ShimiDarmani_G",
         "ShimiDarmani_NG",
         "Bastari_G",
         "Bastari_NG",
         "Azmayeshgah_G",
         "Azmayeshgah_NG",
         "Jarahi_G",
         "Jarahi_NG",
         "Visit_G",
         "Visit_NG",
         "Private_Nurse2",
         "Auto1_Khareji",
         "Auto2_Khareji",
         "Auto2_rani",
         "Auto1_Irani",
         "Motor",
         "Bycycle",
         "Secharkhe",
         "Bycycle_Repair",
         "Lastik_Mashin",
         "lastik_Motor",
         "Battery_Machin",
         "Motor_Machin",
         "Filter_Roghan",
         "KomakFanar",
         "Tolombe",
         "Chador_Mashin",
         "Zinatalat_Mashin",
         "Barband",
         "Dozdgir",
         "Separ",
         "Cheragh_Mashin",
         "Radiator_Mashin",
         "Lavazem_Yadak",
         "Doganesooz",
         "Safkari",
         "Tamirat_Asasi",
         "Simkeshi_Mashin",
         "OtherTamir_Mashin",
         "Akhz_Govahiname",
         "Talim_Ranandegi",
         "Moayene_Fani",
         "Keraye_Mashin",
         "Jarime_Mashin",
         "Taviz_Pelak",
         "Tarh_Traffic",
         "Telephone_Sabet",
         "Fax",
         "Mobile",
         "Software",
         "TV_SS",
         "Video_Player",
         "Anten",
         "Mahvare",
         "Zabtesot",
         "Hedphone",
         "Ghatayat_TV",
         "Tjhizat_Soti",
         "TV_Rangi_Irani",
         "TV_Rangi_Khareji",
         "Doorbin_Digital",
         "Repair_Doorbin",
         "Instrument_Doorbin",
         "PC",
         "Ertegha_System",
         "Printer",
         "Modem",
         "Software_PC",
         "Calculator",
         "Ghatayat_PC",
         "Other_Tajhozat_PC",
         "Gardanband_Gold",
         "Gardanband_Silver",
         "Gardanband_Badal",
         "Saat_Divari",
         "Saat_Mochi",
         "Gire_Kraat",
         "Saat_Repait",
         "Sekke_Prize",
         "Tadfin",
         "Aramgah",
         "Travel_Mazhabi_Iran",
         "Travel_Tafrihi_Iran",
         "Travel_Haj",
         "Travel_Khareji" )
  
  TotalDurable[, Remain_Durable := Reduce(`+`, .SD), .SDcols=g]
  TotalDurable[,Out_from_Durable:=G13-Remain_Durable]
  
  Durablele_Detail<-TotalDurable[,.(HHID,G13,Remain_Durable,Out_from_Durable)]
  save(Durablele_Detail, file=paste0(Settings$HEISProcessedPath,"Y",year,"Durablele_Detail.rda"))
  
  
  }



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)