# 04-HHBase.R
# Builds the base data.table for households
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBase =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

# BigD <- data.table(HHID=NA,Region=NA,Year=NA,Quarter=NA,Month=NA,ProvinceCode=NA)[0]


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  if(year >86 & year < 92 ){ 
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"ShCode.rda"))
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
    if(year==74)
      HHBase[,HHIDs:=formatC(HHID, width = 8, flag = "0")]
    else if(year<77)
      HHBase[,HHIDs:=formatC(HHID, width = 7, flag = "0")]
    else if(year %in% 77:86)
      HHBase[,HHIDs:=formatC(HHID, width = 9, flag = "0")]
    
    if(year < 77)
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,4,4))]
    else
      HHBase[,Quarter:=as.integer(str_sub(HHIDs,6,6))]
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
    HHBase[,Quarter:=(Month-1)%/%3+1]
    HHBase[,HHIDs:=as.character(HHID)]
  }
  
  if(year >86 & year < 92 ){ 
 HHBase<-merge(HHBase,ShCode,by="HHID",all.x = TRUE)
    }
  
  HHBase <- HHBase[!is.na(HHID)]
  HHBase[,ProvinceCode:=as.integer(str_sub(HHIDs,2,3))]
  
  if(year <87 | year > 91 ){
  HHBase[,CountyCode:=as.integer(str_sub(HHIDs,2,5))]
  }
  
  if(year >86 & year < 92 ){ 
    HHBase[,CountyCode:=as.integer(SHCode)]
  }
  
  HHBase[,Year:=year]
  HHBase <- HHBase[,.(HHID,Region,Year,Quarter,Month,ProvinceCode,CountyCode)]
  
  HHBase[,NewArea:=ProvinceCode]
  HHBase[Region=="Urban" & 
           CountyCode %in% c(2301,303,603,707,
                             916,1002,3001,502,2202,
                             401,808,1),
                            # 1304,2105,105,1105
         NewArea:=CountyCode]
  
  HHBase[,NewArea2:=as.factor(NewArea)]
  
  HHBase[NewArea==0,NewArea2:="Markazi"]
  HHBase[NewArea==1,NewArea2:="Gilan"]
  HHBase[NewArea==2,NewArea2:="Mazandaran"]
  HHBase[NewArea==3,NewArea2:="Az_Sharghi"]
  HHBase[NewArea==4,NewArea2:="Az_Gharbi"]
  HHBase[NewArea==5,NewArea2:="Kermanshah"]
  HHBase[NewArea==6,NewArea2:="Khoozestan"]
  HHBase[NewArea==7,NewArea2:="Fars"]
  HHBase[NewArea==8,NewArea2:="Kerman"]
  HHBase[NewArea==9,NewArea2:="Khorasan_Razavi"]
  HHBase[NewArea==10,NewArea2:="Esfahan"]
  HHBase[NewArea==11,NewArea2:="Sistan"]
  HHBase[NewArea==12,NewArea2:="Kordestan"]
  HHBase[NewArea==13,NewArea2:="Hamedan"]
  HHBase[NewArea==14,NewArea2:="Chaharmahal"]
  HHBase[NewArea==15,NewArea2:="Lorestan"]
  HHBase[NewArea==16,NewArea2:="Ilam"]
  HHBase[NewArea==17,NewArea2:="Kohkilooye"]
  HHBase[NewArea==18,NewArea2:="Booshehr"]
  HHBase[NewArea==19,NewArea2:="Zanjan"]
  HHBase[NewArea==20,NewArea2:="Semnan"]
  HHBase[NewArea==21,NewArea2:="Yazd"]
  HHBase[NewArea==22,NewArea2:="Hormozgan"]
  HHBase[NewArea==23,NewArea2:="Tehran"]
  HHBase[NewArea==24,NewArea2:="Ardebil"]
  HHBase[NewArea==25,NewArea2:="Ghom"]
  HHBase[NewArea==26,NewArea2:="Ghazvin"]
  HHBase[NewArea==27,NewArea2:="Golestan"]
  HHBase[NewArea==28,NewArea2:="Khorasan_Shomali"]
  HHBase[NewArea==29,NewArea2:="Khorasan_Jonoobi"]
  HHBase[NewArea==30,NewArea2:="Alborz"]
  HHBase[NewArea==2301,NewArea2:="Sh_Tehran"]
  HHBase[NewArea==303,NewArea2:="Sh_Tabriz"]
  HHBase[NewArea==603,NewArea2:="Sh_Ahvaz"]
  HHBase[NewArea==707,NewArea2:="Sh_Shiraz"]
  HHBase[NewArea==916,NewArea2:="Sh_Mashhad"]
  HHBase[NewArea==1002,NewArea2:="Sh_Esfahan"]
  HHBase[NewArea==3001,NewArea2:="Sh_Karaj"]
  HHBase[NewArea==502,NewArea2:="Sh_Kermanshah"]
  HHBase[NewArea==2202,NewArea2:="Sh_Bandarabas"]
  HHBase[NewArea==401,NewArea2:="Sh_Urmia"]
  #HHBase[NewArea==105,NewArea2:="Sh_Rasht"]
  #HHBase[NewArea==1105,NewArea2:="Sh_Zahedan"]
  HHBase[NewArea==808,NewArea2:="Sh_Kerman"]
  #HHBase[NewArea==1304,NewArea2:="Sh_Hamedan"]
  #HHBase[NewArea==2105,NewArea2:="Sh_Yazd"]
  HHBase[NewArea==1 & CountyCode==1 ,NewArea2:="Sh_Arak"]

  
  #Tehran-Alborz
  if(year >76 & year < 92 ){ 
    HHBase[ Region=="Urban" & CountyCode %in% c(2305),
    NewArea:=3001] 
              }
  
  if(year >76 & year < 92 ){ 
    HHBase[CountyCode %in% c(2308,2315,2309),
           NewArea:=30]
              }
  
  #Khorasan
  if(year >76 & year < 87 ){ 
    HHBase[Region=="Urban" & CountyCode %in% c(916),
           NewArea:=916]
                           }
  
  if(year >76 & year < 87 ){ 
    HHBase[CountyCode %in% c(901,902,909,924,925),
           NewArea:=28]
                           }
  
  if(year >76 & year < 87 ){ 
      HHBase[CountyCode %in% c(903,911,912,921),
             NewArea:=29]
  }
  
  #Ghazvin
  if(year >76 & year < 82 ){ 
    HHBase[CountyCode %in% c(2311),
           NewArea:=26]
  }
  
  #Golestan
  if(year >76 & year < 82 ){ 
    HHBase[CountyCode %in% c(212,203,209,211,213,217),
           NewArea:=27]
  }
  
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))

 # rm(HHBase)
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)