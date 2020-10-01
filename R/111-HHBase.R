# 111-HHBase.R
# Builds the base data.table for households
#
# Copyright © 2016-2020: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBase =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)

year <- 83
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  if(year >86 & year < 92 ){ 
  load(file=paste0(Settings$HEISCountyCodePath,"Y",year,
                   Settings$HEISCountyCodeFileName,".rda"))
  }
  
  if(year < 87){           # RxxData & UxxData tables are provided Since 1387
    RData <- Tables[[paste0("R",year,"P2")]][,1,with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"P2")]][,1,with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Region"))
    HHBase[,Year:=year]
    if(year==74){
      HHBase[,HHIDs:=formatC(HHID, width = 8, flag = "0")]
    }else if(year<77){
      HHBase[,HHIDs:=formatC(HHID, width = 7, flag = "0")]
    }else if(year %in% 77:86){
      HHBase[,HHIDs:=formatC(HHID, width = 9, flag = "0")]
    }
    if(year < 77){
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,4,4))]
    }else{
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,6,6))]
    }
    HHBase[,Month:=NA_integer_]
    
  }else{
    RData <- Tables[[paste0("R",year,"DATA")]][,c(1:2),with=FALSE]
    RData[, Region:=factor(x="Rural",levels=c("Urban","Rural"))]
    UData <- Tables[[paste0("U",year,"DATA")]][,c(1:2),with=FALSE]
    UData[, Region:=factor(x="Urban",levels=c("Urban","Rural"))]
    HHBase <- rbind(RData, UData)
    rm(RData,UData)
    setnames(HHBase,c("HHID","Month","Region"))
    HHBase[,Month:=ifelse(Month==1,12,Month - 1)]
    HHBase[HHID=="10107019605" & year==97,Month:=2]  # Odd Month (-1) in 1397
    if(length(which(HHBase$Month<=0))>0)
      stop("Odd Month Number Here!")
    HHBase[,Quarter:=(Month-1)%/%3+1]
    HHBase[,HHIDs:=as.character(HHID)]
  }
  
  if(year >86 & year < 92 ){ 
    HHBase<-merge(HHBase,ShCode,by="HHID",all.x = TRUE)
  }
  
  HHBase <- HHBase[!is.na(HHID)]
 # HHBase[,ProvinceCode:=as.integer(str_sub(HHIDs,2,3))]
  
  if(year <= 86 | year >= 92 ){
    HHBase[,CountyCode:=as.integer(str_sub(HHIDs,2,5))]
  }
  
  if(year >= 87 & year <= 91 ){ 
    HHBase[,CountyCode:=as.integer(SHCode)]
  }
  
  #Tehran-Alborz
  if(year >76 & year < 92 ){ 
    HHBase[CountyCode==2305, CountyCode:=3001] # Karaj
    HHBase[CountyCode==2308, CountyCode:=3002] # Savojbolagh 
  }
  
  
  #Khorasan
  if(year >76 & year < 87 ){ 
    HHBase[CountyCode==901, CountyCode:=2801] # Esfarayen 
    HHBase[CountyCode==902, CountyCode:=2802] # Bojnourd 
    HHBase[CountyCode==909, CountyCode:=2804] # Shirvan 
    HHBase[CountyCode==924, CountyCode:=2803] # Jajarm 
    HHBase[CountyCode==925, CountyCode:=2806] # Maneh & Samalqan 
    
    HHBase[CountyCode==903, CountyCode:=2901] # Birjand 
    HHBase[CountyCode==911, CountyCode:=2907] # Ferdows
    HHBase[CountyCode==912, CountyCode:=2904] # Qaenat
    HHBase[CountyCode==921, CountyCode:=2905] # Nehbandan
    
    
    HHBase[CountyCode==2809, CountyCode:=2804] # Shirvan
  }
    if(year %in% 84:86){
      HHBase[CountyCode==2824, CountyCode:=2803] # Jaram
      HHBase[CountyCode==2809, CountyCode:=2804] # Shirvan
      HHBase[CountyCode==2813, CountyCode:=2805] # Faruj     # Guess!
      HHBase[CountyCode==2825, CountyCode:=2806] # Mane & Semelqan
      
      HHBase[CountyCode==2903, CountyCode:=2901] # Birjand
      HHBase[CountyCode==2912, CountyCode:=2906] # Sarbishe  # Just a guess
      HHBase[CountyCode==2921, CountyCode:=2905] # Nehbandan
      HHBase[CountyCode==2911, CountyCode:=2903] # Serayan   # Just a guess
      #HHBase[CountyCode==0911, CountyCode:=2907] # Ferdows
    }
    
 
    
  HHBase[CountyCode==2110, CountyCode:=2911] # Tabas
  HHBase[CountyCode==2315, CountyCode:=3003] # Nazarabad
  

  # #Ghazvin
  # if(year >76 & year < 82 ){ 
  #   HHBase[CountyCode %in% c(2311),
  #          NewArea:=26]
  # }
  # 
  #Golestan
  # if(year >76 & year < 82 ){ 
  #   HHBase[CountyCode %in% c(212,203,209,211,213,217),
  #          NewArea:=27]
  # }
  
  HHBase[,ProvinceCode:=CountyCode %/% 100]
  
  HHBase[,Year:=year]
  
  Geo2 <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_Geo2))
  Geo2 <- Geo2[,.(ProvinceCode=SCINo,ProvinceName=NameEnglish)]
  HHBase <- merge(HHBase,Geo2,by="ProvinceCode")
  
  Geo4 <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_Geo4))
  Geo4 <- Geo4[,.(CountyCode=as.numeric(Geo4),CountyName=CountyEn)]
  

  HHBase <- merge(HHBase,Geo4,by="CountyCode",all.x = TRUE)

  HHBase <- HHBase[,.(HHID,Year,Quarter,Month,
                      Region,ProvinceCode,ProvinceName,
                      CountyCode,CountyName)]
  
#  print(table(HHBase[is.na(CountyName),CountyCode]))
  
  
  HHBase[,NewArea:=ProvinceCode]
  HHBase[Region=="Urban" & 
           CountyCode %in% c(2301,303,             # Tehran (County), Tabriz,
                             603,707,              # Ahvaz, Shiraz
                             916,1002,             # Mashhad, Isfahan (County)
                             3001,502,             # Karaj, Kermanshah (County),
                             2202,401,             # Bandarabbas, Urmia
                             808,1105,             # Kerman (County), Zahedan
                             1304,                 # Hamedan (County)
                             2105,105),            # Yazd (County), Rasht
         NewArea:=CountyCode]
  HHBase[Region=="Urban" & CountyCode ==1, 
         NewArea:=-1]                              # Arak [0001] to not be 
                                                   #  confused with [01] Gilan
  

  HHBase[,NewArea_Name:=NA_character_]
  HHBase[,NewArea_Name:=ifelse(NewArea==ProvinceCode,
                               ProvinceName,
                               paste0("Sh_",CountyName))]
  HHBase[,NewArea_Name:=as.factor(NewArea_Name)]

  
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))

  cat(HHBase[,.N])
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])