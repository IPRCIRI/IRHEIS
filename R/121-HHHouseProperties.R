# 121-HHHouseProperties.R
# Builds the House Properties data.table for households
#
# Copyright © 2019:Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHHouseProperties =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(foreign)
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(spatstat)
library(scales)

P2Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P2Cols))

HighShare<-data.table(HHID=NA_real_,
                      G13=NA_real_,
                      Year=NA_real_,
                      Weight=NA_real_,
                      Auto2_rani=NA_real_,
                      Auto1_Irani=NA_real_,
                      Gardanband_Gold=NA_real_,
                      Ghali_Mashini=NA_real_,
                      freezer2=NA_real_,
                      Lastik_Mashin=NA_real_,
                      Mobile=NA_real_,
                      Tamirat_Asasi=NA_real_,
                      Travel_Haj=NA_real_,
                      Mobl=NA_real_,
                      Mashin_Lebasshooyi=NA_real_,
                      TV_Rangi_Khareji=NA_real_)[0]

Table<-data.table(Year=NA_integer_,Auto=NA_real_,Mobile=NA_real_,
                  Refrigerator=NA_real_,TV=NA_real_)[0]

Name<-data.table(HHID=NA_real_,
                 G13=NA_real_,
                 Year=NA_real_,
                 Paltopoost=NA_real_,
                 Lebas_Aroos=NA_real_,
                 Kolah_Imeni=NA_real_,
                 Rent_Lebas=NA_real_,
                 Ajor=NA_real_,
                 Siman=NA_real_,
                 Mase=NA_real_,
                 Mozaik=NA_real_,
                 Seramik=NA_real_,
                 Dar_Panjere=NA_real_,
                 Shishe=NA_real_,
                 Rang=NA_real_,
                 Tiner=NA_real_,
                 Ghalam_Moo=NA_real_,
                 Kaghaz_Divari=NA_real_,
                 Lole_Ab=NA_real_,
                 Lavazem_Behdashti=NA_real_,
                 Toor=NA_real_,
                 Iranit=NA_real_,
                 Kah_Gel=NA_real_,
                 Other_Masaleh=NA_real_,
                 Cooler_Repair=NA_real_,
                 Naghashi_Masaleh=NA_real_,
                 Naghashi_NoMasaleh=NA_real_,
                 KaghazDivari=NA_real_,
                 Nasb_Shishe=NA_real_,
                 Asfalt_Bam=NA_real_,
                 Tamirat_Shirvani=NA_real_,
                 Tamir_Narde=NA_real_,
                 Tamir_Chah=NA_real_,
                 Ojrat_Bana=NA_real_,
                 Ojrat_Kargar=NA_real_,
                 Tamirat_Detail=NA_real_,
                 Other_Negahdari=NA_real_,
                 Cheragh_Electricy=NA_real_,
                 Parde=NA_real_,
                 Ayne=NA_real_,
                 Asar_Honari=NA_real_,
                 Sanaye_Dasti=NA_real_,
                 Gahvare=NA_real_,
                 Lavazem_Safar=NA_real_,
                 MavadTamir1=NA_real_,
                 HazineNasb=NA_real_,
                 Takhtekhab=NA_real_,
                 Komod=NA_real_,
                 Anvae_Miz=NA_real_,
                 Mobl=NA_real_,
                 MavadTamir2=NA_real_,
                 Ghali_Dastbaf=NA_real_,
                 Ghali_Mashini=NA_real_,
                 Mooket=NA_real_,
                 Ziloo=NA_real_,
                 Kafpoosh_Nasb=NA_real_,
                 Lavazem_TamirMobleman=NA_real_,
                 Hazine_TamirMobleman=NA_real_,
                 Yakhchal=NA_real_,
                 freezer2=NA_real_,
                 Mashin_Lebasshooyi=NA_real_,
                 Otoo=NA_real_,
                 OjaghGaz=NA_real_,
                 KhorakPaz=NA_real_,
                 CilandreGaz=NA_real_,
                 Tahvie=NA_real_,
                 Bokhari=NA_real_,
                 Cooler_Gaz=NA_real_,
                 Jaroobarghi=NA_real_,
                 Charkh_Khayati=NA_real_,
                 Machine_bafandei=NA_real_,
                 Sandoogh=NA_real_,
                 Generator=NA_real_,
                 LavazemRepair3=NA_real_,
                 HazineNasb3=NA_real_,
                 Polopaz=NA_real_,
                 ghahvejoosh=NA_real_,
                 Samavar=NA_real_,
                 Abmivergiri=NA_real_,
                 Panke=NA_real_,
                 Other_Barghi=NA_real_,
                 LavazemRepair4=NA_real_,
                 hazine_Ejare=NA_real_,
                 Zoodpaz=NA_real_,
                 Samavar_Nafti=NA_real_,
                 Samavar_Gaz=NA_real_,
                 Keraye_Zoroof=NA_real_,
                 Other_Khanegi=NA_real_,
                 Abzar_Barghi=NA_real_,
                 Chamanzani=NA_real_,
                 TamiratAbzar=NA_real_,
                 Keraye_Mobleman=NA_real_,
                 Patoo_Barghi=NA_real_,
                 Capsule_O2=NA_real_,
                 Visit_Jarahi_G=NA_real_,
                 Visit_Jarahi_NG=NA_real_,
                 Zibai_Jarahi_G=NA_real_,
                 Zibai_Jarahi_NG=NA_real_,
                 ShimiDarmani_G=NA_real_,
                 ShimiDarmani_NG=NA_real_,
                 Bastari_G=NA_real_,
                 Bastari_NG=NA_real_,
                 Azmayeshgah_G=NA_real_,
                 Azmayeshgah_NG=NA_real_,
                 Jarahi_G=NA_real_,
                 Jarahi_NG=NA_real_,
                 Visit_G=NA_real_,
                 Visit_NG=NA_real_,
                 Private_Nurse2=NA_real_,
                 Auto1_Khareji=NA_real_,
                 Auto2_Khareji=NA_real_,
                 Auto2_rani=NA_real_,
                 Auto1_Irani=NA_real_,
                 Motor=NA_real_,
                 Bycycle=NA_real_,
                 Secharkhe=NA_real_,
                 Bycycle_Repair=NA_real_,
                 Lastik_Mashin=NA_real_,
                 lastik_Motor=NA_real_,
                 Battery_Machin=NA_real_,
                 Motor_Machin=NA_real_,
                 Filter_Roghan=NA_real_,
                 KomakFanar=NA_real_,
                 Tolombe=NA_real_,
                 Chador_Mashin=NA_real_,
                 Zinatalat_Mashin=NA_real_,
                 Barband=NA_real_,
                 Dozdgir=NA_real_,
                 Separ=NA_real_,
                 Cheragh_Mashin=NA_real_,
                 Radiator_Mashin=NA_real_,
                 Lavazem_Yadak=NA_real_,
                 Doganesooz=NA_real_,
                 Safkari=NA_real_,
                 Tamirat_Asasi=NA_real_,
                 Simkeshi_Mashin=NA_real_,
                 OtherTamir_Mashin=NA_real_,
                 Akhz_Govahiname=NA_real_,
                 Talim_Ranandegi=NA_real_,
                 Moayene_Fani=NA_real_,
                 Keraye_Mashin=NA_real_,
                 Jarime_Mashin=NA_real_,
                 Taviz_Pelak=NA_real_,
                 Tarh_Traffic=NA_real_,
                 Telephone_Sabet=NA_real_,
                 Fax=NA_real_,
                 Mobile=NA_real_,
                 Software=NA_real_,
                 TV_SS=NA_real_,
                 Video_Player=NA_real_,
                 Anten=NA_real_,
                 Mahvare=NA_real_,
                 Zabtesot=NA_real_,
                 Hedphone=NA_real_,
                 Ghatayat_TV=NA_real_,
                 Tjhizat_Soti=NA_real_,
                 TV_Rangi_Irani=NA_real_,
                 TV_Rangi_Khareji=NA_real_,
                 Doorbin_Digital=NA_real_,
                 Repair_Doorbin=NA_real_,
                 Instrument_Doorbin=NA_real_,
                 PC=NA_real_,
                 Ertegha_System=NA_real_,
                 Printer=NA_real_,
                 Modem=NA_real_,
                 Software_PC=NA_real_,
                 Calculator=NA_real_,
                 Ghatayat_PC=NA_real_,
                 Other_Tajhozat_PC=NA_real_,
                 Ojrat_Tamir_TV=NA_real_,
                 Ojrat_Tamir_PC=NA_real_,
                 Tamir_Other_Soti=NA_real_,
                 Durable_for_Amusement=NA_real_,
                 Musc_Instrument=NA_real_,
                 Biliard=NA_real_,
                 Repair_Durable_Amusement=NA_real_,
                 Dampezeshk_Visit=NA_real_,
                 Game_Equip=NA_real_,
                 Repair_Game_Equip=NA_real_,
                 Mask=NA_real_,
                 Atishbazi=NA_real_,
                 Koleksion=NA_real_,
                 Software_Game=NA_real_,
                 Other_Amusement=NA_real_,
                 Sofre_Aghd=NA_real_,
                 Lavazem_Sofre_Aghd=NA_real_,
                 BallSport=NA_real_,
                 Eski=NA_real_,
                 Kolah_Sport=NA_real_,
                 Dastkesh_Sport=NA_real_,
                 KiseKhab=NA_real_,
                 Tofang=NA_real_,
                 Ojagh_Pikniki=NA_real_,
                 Kafsh_Varzeshi=NA_real_,
                 Shamshirbazi=NA_real_,
                 Mahigiri=NA_real_,
                 Tolombe_Bad=NA_real_,
                 Varzeshi_Other=NA_real_,
                 Repair_Varzeshi=NA_real_,
                 Artificial_Flower=NA_real_,
                 Artificial_Tree=NA_real_,
                 Parande_Mahi=NA_real_,
                 Ghafas=NA_real_,
                 Aquarium=NA_real_,
                 Heyvan_Food=NA_real_,
                 Daroo_Heyvanat=NA_real_,
                 Book_Dabestan=NA_real_,
                 Book_Rahnamayi=NA_real_,
                 Book_Dabirestn=NA_real_,
                 Book_Uni=NA_real_,
                 Book_Komakdarsi=NA_real_,
                 Book_Reference=NA_real_,
                 Book_Mazhabi=NA_real_,
                 Travel_Mazhabi_Iran=NA_real_,
                 Travel_Tafrihi_Iran=NA_real_,
                 Travel_Haj=NA_real_,
                 Travel_Khareji=NA_real_,
                 Enrollment_Dabestan_G=NA_real_,
                 Enrollment_Dabestan_NG=NA_real_,
                 Enrollment_SavadAmoozi=NA_real_,
                 Taghviati_Dabestan=NA_real_,
                 HelptoSchool=NA_real_,
                 Enrollment_Rahnamayi_G=NA_real_,
                 Enrollment_Rahnamayi_NG=NA_real_,
                 Enrollment_Rahnamayi_Shabane=NA_real_,
                 Taghviati_Rahnamayi=NA_real_,
                 Enrollment_Dabirestan_G=NA_real_,
                 Enrollment_Dabirestan_NG=NA_real_,
                 Enrollment_Dabirestan_Shabane=NA_real_,
                 Taghviati_Dabirestan=NA_real_,
                 HelptoDabirestan=NA_real_,
                 Enrollment_pish_G=NA_real_,
                 Enrollment_pish_NG=NA_real_,
                 Enrollment_pish_Shabane=NA_real_,
                 Taghviati_pish=NA_real_,
                 KelasKonkoor=NA_real_,
                 Helptipish=NA_real_,
                 Enrollment_Uni_G=NA_real_,
                 Enrollment_Elmikarbordi=NA_real_,
                 Enrollment_Azad_Uni=NA_real_,
                 Taghviati_Uni=NA_real_,
                 Jabejai_Daneshjoo=NA_real_,
                 EnrollmentZaban=NA_real_,
                 Education_Fanni=NA_real_,
                 Education_Ashbazi=NA_real_,
                 Education_Quran=NA_real_,
                 Education_Other=NA_real_,
                 Gardanband_Gold=NA_real_,
                 Gardanband_Silver=NA_real_,
                 Gardanband_Badal=NA_real_,
                 Saat_Divari=NA_real_,
                 Saat_Mochi=NA_real_,
                 Gire_Kraat=NA_real_,
                 Saat_Repait=NA_real_,
                 Sekke_Prize=NA_real_,
                 Tadfin=NA_real_,
                 Aramgah=NA_real_,
                 Shirkhargah=NA_real_,
                 Premium_Omr=NA_real_,
                 Premium_Havades=NA_real_,
                 Premium_gheyredarmani_mostakhdem=NA_real_,
                 Premium_gheyredarmani_karfarma=NA_real_,
                 Premium_retirement_mostakhdem=NA_real_,
                 Premium_retirement_karfarma=NA_real_,
                 Premium_retirement_general=NA_real_,
                 Premium_retirement_Rural_Household=NA_real_,
                 Premium_retirement_Rural_Govern=NA_real_,
                 Premium_retirement_bank=NA_real_,
                 Premium_manzel=NA_real_,
                 Premium_asas_manzel=NA_real_,
                 Premium_Medical_Household=NA_real_,
                 Premium_Medical_Government=NA_real_,
                 Premium_Social_Mostakhdem=NA_real_,
                 Premium_Social_Karfarma=NA_real_,
                 Premium_Other=NA_real_,
                 Premium_Medical_General=NA_real_,
                 Premium_Medical_Rural=NA_real_,
                 Premium_Naghlie=NA_real_,
                 Premium_Bar=NA_real_,
                 Premium_Shakhsesales=NA_real_,
                 Premium_Other2=NA_real_,
                 Fee_Loan_Maskan=NA_real_,
                 Fee_Loan_NoMaskan=NA_real_,
                 BankServices=NA_real_,
                 MoshavereServices=NA_real_,
                 Hagholvekalat=NA_real_,
                 Other_Hoghooghi=NA_real_,
                 Karyabi=NA_real_,
                 Kafnodafn=NA_real_,
                 Dalali_Maskan=NA_real_,
                 Dalali_Mashin=NA_real_,
                 Avarez_Shahrdari=NA_real_,
                 Shenasname_Kartmelli=NA_real_,
                 Gozarname=NA_real_,
                 Daftarche_Bime=NA_real_,
                 Avareze_Khorooj=NA_real_,
                 Photocopy=NA_real_,
                 Sanad_Ezdevaj=NA_real_,
                 Parvane_Shekar=NA_real_,
                 Tablighat_GheyreShoghli=NA_real_,
                 Azmoon_Estekhdami=NA_real_,
                 Talebini=NA_real_,
                 Daroltarjome=NA_real_,
                 Khadamat_Motefarehje=NA_real_,
                 Dalali_Simcard=NA_real_,
                 Khoms=NA_real_,
                 Zokat=NA_real_,
                 Fetrie=NA_real_,
                 HelptoMasjed=NA_real_,
                 Madahi=NA_real_,
                 Sandoogh_Sadaghat=NA_real_,
                 Die=NA_real_,
                 KhesaratbeAfrad=NA_real_,
                 Maliat_Maskan=NA_real_,
                 Pardakht_Enteghali=NA_real_,
                 Cash_Prize_G=NA_real_,
                 Cash_Prize_NG=NA_real_,
                 Zendan=NA_real_,
                 Weight=NA_real_,
                 Decile=NA_real_)[0]

Dep<-data.table(Year=NA_integer_, Buy=NA_real_,Use=NA_real_)[0]

M_Buyers <- data.table(Year=NA_integer_,cluster3=NA_real_,Mobile_Buyers_Exp=NA_real_)[0]

years <- Settings$startyear:Settings$endyear

for(year in setdiff(years,63:88)){    # TODO: Add the metadata for 63 to 88 in P2Cols
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  

  P2 <- rbind(Tables[[paste0("R",year,"P2")]],Tables[[paste0("U",year,"P2")]])
  nP2 <- names(P2)
  if(length(which(sapply(P2, is.character)))>0){
    P2c <- P2[,lapply(.SD,iconv,"WINDOWS-1252","UTF-8"), .SDcols=sapply(P2,is.character)] 
    P2nc <- P2[,!sapply(P2,is.character),with=FALSE]
    P2 <- cbind(P2c,P2nc)[,nP2,with=FALSE]
  }
  
  if(year>=96){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[2:46]
    setnames(P2,names(a[ind]))
    
  }else if(year %in% 89:95){
    a <- unlist(P2Cols[P2Cols$Year==year,])
    ind <- which(!is.na(a))[-1]
    setnames(P2,names(a[ind]))
  }
  

  f <- function(x){as.numeric(str_trim(x))}
  P2 <- P2[, lapply(.SD, f)] 
  
  P2[is.na(P2)]<-0  
  
  P2[,tenure :=factor(tenure, levels=1:7, 
                    labels=c("OwnLandandBuilding","Apartment","Rented",
                             "Mortgage","AgainstService",
                             "Free","Other"))]
  
  P2[,skeleton :=factor(skeleton, levels=1:3, 
                      labels=c("metal","concrete","other"))]
  
  P2[,constmat :=factor(constmat, levels=1:8, 
                      labels=c("BrickSteel_StoneSteel","Brickwood_Stonewood","CementBlocks",
                               "AllBrick_Stone","Allwood",
                               "SundriedBrickwood","SundriedBrickmud",
                               "other"))]
  
  P2[,cookfuel :=factor(cookfuel, levels=1:10, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  
  P2[,heatfuel :=factor(heatfuel, levels=11:20, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  
  P2[,hotwater :=factor(hotwater, levels=21:30, 
                        labels=c("karosine","gasoline",
                                 "gas","pipedgas",
                                 "electricity","woodandcharcoal",
                                 "Animalfuel","charcoal","otherFuel","None"))]
  
  P2[,car := factor(car, levels = 0:1,
                    labels=c("False","True"))]
  
  P2[,motorcycle := factor(motorcycle, levels=0:1,
                    labels=c("False","True"))]
  
  P2[,bike := factor(bike, levels=0:1,
                           labels=c("False","True"))]

  P2[,radio := factor(radio, levels=0:1,
                           labels=c("False","True"))]
  
  P2[,cassette := factor(cassette, levels=0:1,
                           labels=c("False","True"))]
  
  P2[,tvbw := factor(tvbw, levels=0:1,
                         labels=c("False","True"))]
  
  P2[,tvcr := factor(tvcr, levels=0:1,
                         labels=c("False","True"))]
  
  P2[,vcr := factor(vcr, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,computer := factor(computer, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,cellphone := factor(cellphone, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,freezer := factor(freezer, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,refrigerator := factor(refrigerator, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,frez_refrig := factor(frez_refrig, levels=0:1,
                          labels=c("False","True"))]
  
  P2[,oven := factor(oven, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,vacuum := factor(vacuum, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,washer := factor(washer, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,sewing := factor(sewing, levels=0:1,
                            labels=c("False","True"))]
  
  P2[,fan := factor(fan, levels=0:1,
                       labels=c("False","True"))]
  
  P2[,cooler_water_movable := factor(cooler_water_movable,
                                     levels=0:1,
                       labels=c("False","True"))]
  
  P2[,cooler_gas_movable := factor(cooler_gas_movable, 
                                   levels=0:1,
                       labels=c("False","True"))]
  
  P2[,dishwasher := factor(dishwasher, levels=0:1,
                       labels=c("False","True"))]
  
  if(year %in% 91:97){
  P2[,Microwave := factor(Microwave, levels=0:1,
                           labels=c("False","True"))]
  }
  
  P2[,none := factor(none, levels=0:1,
                           labels=c("False","True"))]

  P2[,pipewater := factor(pipewater, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,electricity := factor(electricity, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,pipegas := factor(pipegas, levels=0:1,
                     labels=c("False","True"))]
  
  P2[,phone := factor(phone, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,internet := factor(internet, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,bathroom := factor(bathroom, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,kitchen := factor(kitchen, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,cooler := factor(cooler, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,centralcooler := factor(centralcooler, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,centralheat := factor(centralheat, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,pakage := factor(pakage, levels=0:1,
                        labels=c("False","True"))]
  
  P2[,cooler_gas := factor(cooler_gas, levels=0:1,
                       labels=c("False","True"))]
  
  P2[,ego := factor(ego, levels=0:1,
                       labels=c("False","True"))]
  
  if(year %in% 89:95){
    P2[,party_month := factor(party_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,party_year := factor(party_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,ceremony_month := factor(ceremony_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,ceremony_year := factor(ceremony_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,homerepaire_month := factor(homerepaire_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,homerepaire_year := factor(homerepaire_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,prtrip_month := factor(prtrip_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,prtrip_year := factor(prtrip_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,frtrip_month := factor(frtrip_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,frtrip_year := factor(frtrip_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,bastari_month := factor(bastari_month, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,bastari_year := factor(bastari_year, levels=0:2,
                            labels=c("False","none","True"))]
    
    P2[,operation_month := factor(operation_month, levels=0:1,
                            labels=c("False","True"))]
    
    if(year %in% 91:95){
    P2[,operation_year := factor(operation_year, levels=0:2,
                            labels=c("False","none","True"))]
    }
    
    P2[,other_year := factor(other_year, levels=0:1,
                            labels=c("False","True"))]
    
    P2[,noceremony := factor(noceremony, levels=0:1,
                            labels=c("False","True"))]
  }
  
  HHHouseProperties<-P2
  save(HHHouseProperties, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"TotalDurable.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHHouseProperties<-merge(HHHouseProperties,HHWeights)
  HHHouseProperties<-merge(HHHouseProperties,HHBase)
  HHHouseProperties<-merge(HHHouseProperties,MD[,.(HHID,Decile,FinalPoor,Total_Exp_Month,cluster3)])
  HHHouseProperties<-merge(HHHouseProperties,TotalDurable)
  
  X <- HHHouseProperties[,.(Auto=weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
                                  Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight),
                          Mobile=weighted.mean(Mobile>0,Weight),
             Refrigerator=weighted.mean(Yakhchal>0 | freezer2>0,Weight),
             TV=weighted.mean(TV_Rangi_Irani>0 |
                                TV_Rangi_Khareji>0,Weight))]
  X[,Year:=year]
  Table <- rbind(Table,X)
  
  HHHouseProperties[,weighted.mean(car=="True",Weight)]
  HHHouseProperties[,weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
                                     Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight)]
#  X1 <- HHHouseProperties[,.(Use=weighted.mean(car=="True",Weight),
 #                            Buy=weighted.mean(Auto1_Irani>0 | Auto2_rani>0 | 
  #                           Auto1_Khareji>0 | Auto2_Khareji>0 ,Weight))]
#  X1[,Year:=year]
#  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(motorcycle=="True",Weight)]
  HHHouseProperties[,weighted.mean(Motor>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(bike=="True",Weight)]
  HHHouseProperties[,weighted.mean(Bycycle>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(radio=="True",Weight)]
  HHHouseProperties[,weighted.mean(cassette=="True",Weight)]
  HHHouseProperties[,weighted.mean(Zabtesot>0 ,Weight)]
  
  
  HHHouseProperties[,weighted.mean(tvbw=="True",Weight)]
  HHHouseProperties[,weighted.mean(TV_SS>0 ,Weight)]

  
  
  HHHouseProperties[,weighted.mean(tvcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(TV_Rangi_Irani>0 |
                                     TV_Rangi_Khareji>0 ,Weight)]
#  X1 <- HHHouseProperties[,.(Use=weighted.mean(tvcr=="True",Weight),
 #                            Buy=weighted.mean(TV_Rangi_Irani>0 |
  #                                               TV_Rangi_Khareji>0  ,Weight))]
#  X1[,Year:=year]
#  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(vcr=="True",Weight)]
  HHHouseProperties[,weighted.mean(Video_Player>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(computer=="True",Weight)]
  HHHouseProperties[,weighted.mean(PC>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(cellphone=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mobile>0 ,Weight)]
  X1 <- HHHouseProperties[,.(Use=weighted.mean(cellphone=="True",Weight),
                             Buy=weighted.mean(Mobile>0  ,Weight))]
  X1[,Year:=year]
  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(freezer=="True",Weight)]
  HHHouseProperties[,weighted.mean(frez_refrig=="True",Weight)]
  HHHouseProperties[,weighted.mean(freezer2>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(refrigerator=="True",Weight)]
  HHHouseProperties[,weighted.mean(Yakhchal>0 ,Weight)]
  
#  X1 <- HHHouseProperties[,.(Use=weighted.mean(freezer=="True" | 
 #                                                refrigerator=="True" |
  #                                               frez_refrig=="True",Weight),
   #                          Buy=weighted.mean(freezer2>0 |
    #                                             Yakhchal>0  ,Weight))]
#  X1[,Year:=year]
#  Dep <- rbind(Dep,X1)

  HHHouseProperties[,weighted.mean(oven=="True",Weight)]
 # HHHouseProperties[,weighted.mean(Microwave=="True",Weight)]
  HHHouseProperties[,weighted.mean(OjaghGaz>0 ,Weight)]
#  X1 <- HHHouseProperties[,.(Use=weighted.mean(oven=="True" ,Weight),
 #                            Buy=weighted.mean(OjaghGaz>0  ,Weight))]
#  X1[,Year:=year]
#  Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(vacuum=="True",Weight)]
  HHHouseProperties[,weighted.mean(Jaroobarghi>0 ,Weight)]
  
  HHHouseProperties[,weighted.mean(washer=="True",Weight)]
  HHHouseProperties[,weighted.mean(dishwasher=="True",Weight)]
  HHHouseProperties[,weighted.mean(Mashin_Lebasshooyi>0,Weight)]
#  X1 <- HHHouseProperties[,.(Use=weighted.mean(washer=="True"  |
 #                                                dishwasher=="True",Weight),
  #                           Buy=weighted.mean(Mashin_Lebasshooyi>0  ,Weight))]
#  X1[,Year:=year]
 # Dep <- rbind(Dep,X1)
  
  HHHouseProperties[,weighted.mean(sewing=="True",Weight)]
  HHHouseProperties[,weighted.mean(Charkh_Khayati>0,Weight)]
  
  HHHouseProperties[,weighted.mean(fan=="True",Weight)]
  HHHouseProperties[,weighted.mean(cooler_water_movable=="True",Weight)]
  HHHouseProperties[,weighted.mean(Panke>0 ,Weight)]
  
   HHHouseProperties[,weighted.mean(cooler_gas_movable=="True",Weight)]
  HHHouseProperties[,weighted.mean(Cooler_Gaz>0,Weight)]
  
  load(file = "durable.rda")
  ggplot(durable)+
    geom_line(mapping = aes(x=Year,y=Ratio,col=factor(Type))) + ylim(0,0.13)
  
  HHHouseProperties[Auto1_Khareji>0,weighted.mean(Auto1_Khareji,Weight),by=.(Region,ProvinceCode)]
  HHHouseProperties[Auto1_Khareji>0,weighted.median(Auto1_Khareji,Weight),by=.(Region,ProvinceCode)]
  
  #Auto
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                      Auto1_Khareji>0 | Auto2_Khareji>0,
                      .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Auto[,Ratio:=N/Number]

  
  ggplot(Auto, aes(fill=factor(Region), y=Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                              Auto1_Khareji>0 | Auto2_Khareji>0,
                            .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Auto[,Auto_Ratio:=N/Number]
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  

  #Mobile
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Mobile<-HHHouseProperties[Mobile>0,
                          .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Mobile[,Mobile_Ratio:=N/Number]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Mobile<-HHHouseProperties[Mobile>0,
                          .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Mobile[,Mobile_Ratio:=N/Number]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Yakhchal
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0,
                            .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  Yakhchal[,Yakhchal_Ratio:=N/Number]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0,
                            .(.N,Number=mean(Number)),by=.(Region,Decile)]
  Yakhchal[,Yakhchal_Ratio:=N/Number]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #TV
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                                TV_Rangi_Khareji>0,
                              .(.N,Number=mean(Number)),by=.(Region,ProvinceName)]
  TV[,TV_Ratio:=N/Number]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Ratio, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |TV_Rangi_Khareji>0,
  .(.N,Number=mean(Number)),by=.(Region,Decile)]
  TV[,TV_Ratio:=N/Number]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Ratio, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)

  
  #########################Exp#############################
  #Auto
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                            Auto1_Khareji>0 | Auto2_Khareji>0,
                          .(.N,Auto_Exp=weighted.mean(Auto1_Irani+Auto2_rani+
                            Auto1_Khareji+Auto2_Khareji,Weight),
                            Auto_Exp2=weighted.median(Auto1_Irani+Auto2_rani+
                             Auto1_Khareji+Auto2_Khareji,Weight)),
                          by=.(Region,ProvinceName)]
  
  Auto<-Auto[,Mean_Median:=Auto_Exp-Auto_Exp2]

  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Auto<-HHHouseProperties[Auto1_Irani>0 | Auto2_rani>0 | 
                            Auto1_Khareji>0 | Auto2_Khareji>0,
                          .(.N,Auto_Exp=weighted.mean(Auto1_Irani+Auto2_rani+
                           Auto1_Khareji+Auto2_Khareji,Weight),
                            Auto_Exp2=weighted.median(Auto1_Irani+Auto2_rani+
                           Auto1_Khareji+Auto2_Khareji,Weight)),
                          by=.(Region,Decile)]
  Auto<-Auto[,Mean_Median:=Auto_Exp-Auto_Exp2]
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Auto_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Auto, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Mobile
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Mobile<-HHHouseProperties[Mobile>0 ,
                            .(.N,Mobile_Exp=weighted.mean(Mobile,Weight),
                              Mobile_Exp2=weighted.median(Mobile,Weight)),
                          by=.(Region,ProvinceName)]
  
  Mobile<-Mobile[,Mean_Median:=Mobile_Exp-Mobile_Exp2]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Mobile<-HHHouseProperties[Mobile>0 ,
                          .(.N,Mobile_Exp=weighted.mean(Mobile,Weight),
                            Mobile_Exp2=weighted.median(Mobile,Weight)),
                          by=.(Region,Decile)]
  Mobile<-Mobile[,Mean_Median:=Mobile_Exp-Mobile_Exp2]
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mobile_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Mobile, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  #Yakhchal
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0 ,
                            .(.N,Yakhchal_Exp=weighted.mean(Yakhchal+freezer2,Weight),
                              Yakhchal_Exp2=weighted.median(Yakhchal+freezer2,Weight)),
                            by=.(Region,ProvinceName)]
  
  Yakhchal<-Yakhchal[,Mean_Median:=Yakhchal_Exp-Yakhchal_Exp2]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  Yakhchal<-HHHouseProperties[Yakhchal>0 | freezer2>0 ,
                            .(.N,Yakhchal_Exp=weighted.mean(Yakhchal+freezer2,Weight),
                              Yakhchal_Exp2=weighted.median(Yakhchal+freezer2,Weight)),
                            by=.(Region,Decile)]
  Yakhchal<-Yakhchal[,Mean_Median:=Yakhchal_Exp-Yakhchal_Exp2]
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Yakhchal_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(Yakhchal, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)  
  
  #TV
  HHHouseProperties[,Number:=.N,by=.(Region,ProvinceName)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                          TV_Rangi_Khareji>0 ,
                              .(.N,TV_Exp=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight),
                                TV_Exp2=weighted.median(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),
                              by=.(Region,ProvinceName)]
  
  TV<-TV[,Mean_Median:=TV_Exp-TV_Exp2]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp2, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=Mean_Median, x=factor(ProvinceName))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  HHHouseProperties[,Number:=.N,by=.(Region,Decile)]
  TV<-HHHouseProperties[TV_Rangi_Irani>0 |
                          TV_Rangi_Khareji>0 ,
                              .(.N,TV_Exp=weighted.mean(TV_Rangi_Irani+TV_Rangi_Khareji,Weight),
                                TV_Exp2=weighted.median(TV_Rangi_Irani+TV_Rangi_Khareji,Weight)),
                              by=.(Region,Decile)]
  TV<-TV[,Mean_Median:=TV_Exp-TV_Exp2]
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=TV_Exp2, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
  
  ggplot(TV, aes(fill=factor(Region), y=Mean_Median, x=factor(Decile))) + 
    geom_bar(position="dodge", stat="identity") + theme_bw() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25) 
  

  
  MM_Buyers<-HHHouseProperties[Mobile>0,.(Mobile_Buyers=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  M_Holders<-HHHouseProperties[,.(Mobile_Holders=weighted.mean(cellphone=="True",Weight)),by=.(cluster3)]
  M_All<-HHHouseProperties[,.(All=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  
  Y <- HHHouseProperties[Mobile>0,.(Mobile_Buyers_Exp=weighted.mean(Mobile,Weight)),by=.(cluster3)]
  Y[,Year:=year]
  M_Buyers <- rbind(M_Buyers,Y)
  
  M<-merge(MM_Buyers,M_Holders)
  M<-merge(M,M_All)
  
  HHHouseProperties[cluster3==1,weighted.mean(Mobile/Total_Exp_Month,Weight,TotalDurable)]

  if (year<97){
    TotalDurable[,G13:=Durable_Exp]
    TotalDurable[,Durable_Exp:=NULL]
  }
  
  
#  TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Decile,Weight)],by="HHID")
#  s2 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
 # s2[,Year:=year]
  #Name <- rbind(Name,s2)
  
  TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Weight,Decile)],by="HHID")
  X1<-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4,
                   .(HHID,Weight,G13,Auto2_rani,Auto1_Irani,
                      Gardanband_Gold,
                      Ghali_Mashini,freezer2,Lastik_Mashin,
                      Mobile,Tamirat_Asasi,Travel_Haj,Mobl,
                      Mashin_Lebasshooyi,TV_Rangi_Khareji)]
  X1<-X1[, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
  
  X1[,Year:=year]
  HighShare <- rbind(HighShare,X1)
  }

ggplot(M_Buyers)+
  geom_line(mapping = aes(x=Year,y=Mobile_Buyers_Exp,col=factor(cluster3)))


HHHouseProperties[,weighted.mean((Auto1_Irani+Auto2_rani+ 
                                   Auto1_Khareji+Auto2_Khareji+
                                   TV_Rangi_Irani+TV_Rangi_Khareji+
                                   Mobile+Yakhchal)/G13,Weight,na.rm = TRUE)]

#HHHouseProperties[,weighted.mean(EducationTExp/G13,na.rm = TRUE)]
#######################################################
m<-HHHouseProperties[,.(.N,Share=weighted.mean(G13/Total_Exp_Month)),by=.(FinalPoor,Region)]
m<-m[,FinalPoor:=ifelse(FinalPoor==1,"Poors","NonPoors")]

ggplot(m, aes(fill=factor(Region), y=Share, x=factor(FinalPoor))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_text(aes(label=N), position=position_dodge(width=0.9), vjust=-0.25)
########################################################

TotalDurable<-merge(TotalDurable,HHHouseProperties[,.(HHID,Weight,Decile)],by="HHID")
z1 <-TotalDurable[, {lapply(.SD, function(x) sum(x*Weight)/sum(Weight))}][]
z2 <-TotalDurable[, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]

#write.csv(z1,"z1.csv")
#write.csv(z2,"z2.csv")

s1 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(Weight))}][]
s2 <-TotalDurable[as.numeric(Decile)>1 & as.numeric(Decile)<4, {lapply(.SD, function(x) sum(x*Weight)/sum(G13*Weight))}][]
#s2[,Year:=year]
#Name <- rbind(Name,s2)

#write.csv(s1,"s1.csv")
#write.csv(s2,"s2.csv")





endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
