#Maskan

#Zahra Shahidi
#2021


cat("\n\n================ Maskan Index =====================================\n")

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(writexl)


Geoo <- as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx",
                                 sheet = "Sheet1"))
Geo_O<-as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx", 
                                sheet = "Sheet2"))
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  MD<-merge(MD,HHHouseProperties,by=c("HHID"),all.x = T)
  
  MD<-MD[,Rental_House:=ifelse(tenure=="Rented" | tenure=="Mortgage",1,0)]
  MD<-MD[,Mortgage_House:=ifelse(tenure=="Mortgage",1,0)]
  MD<-MD[,Own_House:=ifelse(tenure=="OwnLandandBuilding",1,0)]
  MD<-MD[,Oghafi_House:=ifelse(tenure=="Apartment",1,0)]
  MD<-MD[,Metal_Skeleton:=ifelse(skeleton=="metal",1,0)]
  MD<-MD[,Concrete_Skeleton:=ifelse(skeleton=="concrete",1,0)]
  MD<-MD[,Other_Skeleton:=ifelse(skeleton=="other" & constmat!="BrickSteel_StoneSteel",1,0)]
  MD<-MD[,BrickSteel_StoneSteel_constmat:=ifelse(constmat=="BrickSteel_StoneSteel",1,0)]
  MD<-MD[,Brickwood_Stonewood_constmat:=ifelse(constmat=="Brickwood_Stonewood",1,0)]
  MD<-MD[,CementBlocks_constmat:=ifelse(constmat=="CementBlocks",1,0)]
  MD<-MD[,AllBrick_Stone_constmat:=ifelse(constmat=="AllBrick_Stone",1,0)]
  MD<-MD[,Allwood_constmat:=ifelse(constmat=="Allwood",1,0)]
  MD<-MD[,SundriedBrickwood_constmat:=ifelse(constmat=="SundriedBrickwood",1,0)]
  MD<-MD[,SundriedBrickmud_constmat:=ifelse(constmat=="SundriedBrickmud",1,0)]
  MD<-MD[,O_Other_Skeleton:=ifelse(constmat=="other",1,0)]
  MD<-MD[,area_per:=area/Size]
  MD<-MD[,None:=ifelse(none=="FALSE",1,0)]
  MD<-MD[,Pipewater:=ifelse(pipewater=="FALSE",1,0)]
  MD<-MD[,Electricity:=ifelse(electricity=="FALSE",1,0)]
  MD<-MD[,Pipegas:=ifelse(pipegas=="TRUE",1,0)]
  MD<-MD[,Kitchen:=ifelse(kitchen=="FALSE",1,0)]
  MD<-MD[,Cooler:=ifelse(cooler=="FALSE",1,0)]
  MD<-MD[,CentralCooler:=ifelse(centralcooler=="FALSE",1,0)]
  MD<-MD[,CentralHeat:=ifelse(centralheat=="FALSE",1,0)]
  MD<-MD[,Pakage:=ifelse(pakage=="FALSE",1,0)]
  MD<-MD[,Cooler_Gas:=ifelse(cooler_gas=="FALSE",1,0)]
  MD<-MD[,Sewage_Network:=ifelse(seweragenetwork=="FALSE",1,0)]
  MD<-MD[,Phone:=ifelse(phone=="FALSE",1,0)]
  MD<-MD[,Bike:=ifelse(bike=="FALSE",1,0)]
  MD<-MD[,Radio:=ifelse(radio=="FALSE",1,0)]
  MD<-MD[,Cassette:=ifelse(cassette=="FALSE",1,0)]
  MD<-MD[,Tvbw:=ifelse(tvbw=="FALSE",1,0)]
  MD<-MD[,Tvcr:=ifelse(tvcr=="FALSE",1,0)]
  MD<-MD[,Vcr:=ifelse(vcr=="FALSE",1,0)]
  MD<-MD[,CellPhone:=ifelse(cellphone=="FALSE",1,0)]
  MD<-MD[,Freezer:=ifelse(freezer=="FALSE",1,0)]
  MD<-MD[,Refrigerator:=ifelse(refrigerator=="FALSE",1,0)]
  MD<-MD[,Frez_Refrig:=ifelse(frez_refrig=="FALSE",1,0)]
  MD<-MD[,Oven:=ifelse(oven=="FALSE",1,0)]
  MD<-MD[,Vacuum:=ifelse(vacuum=="FALSE",1,0)]
  MD<-MD[,Washer:=ifelse(washer=="FALSE",1,0)]
  MD<-MD[,Sewing:=ifelse(sewing=="FALSE",1,0)]
  MD<-MD[,Fan:=ifelse(fan=="FALSE",1,0)]
  MD<-MD[,Cooler_Water_Movable:=ifelse(cooler_water_movable=="FALSE",1,0)]
  MD<-MD[,Cooler_Gas_Movable:=ifelse(cooler_gas_movable=="FALSE",1,0)]
  MD<-MD[,Dishwasher:=ifelse(dishwasher=="FALSE",1,0)]
  MD<-MD[,Internet:=ifelse(internet=="FALSE",1,0)]
  MD<-MD[,Bathroom:=ifelse(bathroom=="FALSE",1,0)]
  for (col in c("BrickSteel_StoneSteel_constmat","O_Other_Skeleton","Brickwood_Stonewood_constmat",
                "CementBlocks_constmat","AllBrick_Stone_constmat","Allwood_constmat",
                "SundriedBrickwood_constmat","SundriedBrickmud_constmat"))
    MD[is.na(get(col)), (col) := 0]
  

MD<-MD[,Facility:=ifelse(Kitchen==1 | Bathroom==1,1,0)]
MD<-MD[,Space:=ifelse(area_per<12,1,0)]
MD<-MD[,Zaghe:=ifelse(Pipewater==1 | Space==1 | Bathroom ==1 | Other_Skeleton==1,1,0)]
MD<-MD[,Namonaseb:=Pipewater+Space+Bathroom+Other_Skeleton] 
MD<-MD[,Namonaseb_Maskan:=ifelse(Namonaseb<2,0,1)] 
MD<-MD[,House_Share:=House_Exp/Total_Exp_Month]
MD<-MD[,N_Maskan:=ifelse(House_Share>0.3,1,0)]
MD<-MD[,Kids:=ifelse(UN18All>0,1,0)]
MD<-MD[,Room_Per:=room/Size]

MD<-MD[,ZanSarparast:=ifelse(HSex=="Female",1,0)]
MD<-MD[,DarayeKoodak:=ifelse(UN18All>0,1,0)]
MD<-MD[,Foghara:=FinalPoor]
#Select The desirable group--->>>
MD<-MD[,Group:=ZanSarparast]
#-------------------Country Resaults-------------
Features<-MD[,.(ZagheNeshin=weighted.mean(Zaghe,Weight),
                      DoMahrumiat=weighted.mean(Namonaseb_Maskan,Weight),
                      MaskaneNamonaseb=weighted.mean(N_Maskan,Weight),
                      MaskaneEstijari=weighted.mean(Rental_House,Weight),
                      Masaleh=weighted.mean(Other_Skeleton,Weight*Size),
                      AbAshamidani=weighted.mean(Pipewater,Weight*Size),
                      FazayeKafi=weighted.mean(Space,Weight*Size),
                      Hammam=weighted.mean(Bathroom,Weight*Size))]
#-------------------Region Resaults--------------
Features_Region<-MD[,.(ZagheNeshin=weighted.mean(Zaghe,Weight),
                      DoMahrumiat=weighted.mean(Namonaseb_Maskan,Weight),
                      MaskaneNamonaseb=weighted.mean(N_Maskan,Weight),
                      MaskaneEstijari=weighted.mean(Rental_House,Weight),
                      Masaleh=weighted.mean(Other_Skeleton,Weight*Size),
                      AbAshamidani=weighted.mean(Pipewater,Weight*Size),
                      FazayeKafi=weighted.mean(Space,Weight*Size),
                      Hammam=weighted.mean(Bathroom,Weight*Size)),by=c("Region")]
#-------------------Group Resaults------------
Features_Group<-MD[,.(ZagheNeshin=weighted.mean(Zaghe,Weight),
                DoMahrumiat=weighted.mean(Namonaseb_Maskan,Weight),
                MaskaneNamonaseb=weighted.mean(N_Maskan,Weight),
                MaskaneEstijari=weighted.mean(Rental_House,Weight),
                Masaleh=weighted.mean(Other_Skeleton,Weight*Size),
                AbAshamidani=weighted.mean(Pipewater,Weight*Size),
                FazayeKafi=weighted.mean(Space,Weight*Size),
                Hammam=weighted.mean(Bathroom,Weight*Size)),by=c("Group")]
#-------------------Group Region Resaults------------
Features_Group_Region<-MD[,.(ZagheNeshin=weighted.mean(Zaghe,Weight),
                      DoMahrumiat=weighted.mean(Namonaseb_Maskan,Weight),
                      MaskaneNamonaseb=weighted.mean(N_Maskan,Weight),
                      MaskaneEstijari=weighted.mean(Rental_House,Weight),
                      Masaleh=weighted.mean(Other_Skeleton,Weight*Size),
                      AbAshamidani=weighted.mean(Pipewater,Weight*Size),
                      FazayeKafi=weighted.mean(Space,Weight*Size),
                      Hammam=weighted.mean(Bathroom,Weight*Size)),by=c("Group","Region")]
#---------------Build TimeSeries-----------------  
Features<-Features[,Year:=year]
if (year==Settings$startyear){
  Shakhes<-Features
}else{
  Shakhes<-rbind(Shakhes,Features)
}

Features_Region<-Features_Region[,Year:=year]
if (year==Settings$startyear){
  Shakhes_Region<-Features_Region
}else{
  Shakhes_Region<-rbind(Shakhes_Region,Features_Region)
}

Features_Group<-Features_Group[,Year:=year]
if (year==Settings$startyear){
  Shakhes_Group<-Features_Group
}else{
  Shakhes_Group<-rbind(Shakhes_Group,Features_Group)
}

Features_Group_Region<-Features_Group_Region[,Year:=year]
if (year==Settings$startyear){
  Shakhes_Group_Region<-Features_Group_Region
}else{
  Shakhes_Group_Region<-rbind(Shakhes_Group_Region,Features_Group_Region)
}

}

write_xlsx(Shakhes,path=paste0(Settings$HEISResultsPath,"Shakhes_Maskan.xlsx"))
write_xlsx(Shakhes_Region,path=paste0(Settings$HEISResultsPath,"Shakhes_Maskan_Region.xlsx"))
write_xlsx(Shakhes_Group,path=paste0(Settings$HEISResultsPath,"Shakhes_Maskan_Group.xlsx"))
write_xlsx(Shakhes_Group_Region,path=paste0(Settings$HEISResultsPath,"Shakhes_Maskan_Group_Region.xlsx"))
