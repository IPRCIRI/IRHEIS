# Wooman Heads.R
# 
#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHBase =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)


year<-96
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  if(year >86 & year < 92 ){ 
    load(file=paste0(Settings$HEISCountyCodePath,"Y",year,Settings$HEISCountyCodeFileName,".rda"))
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
                             401,808,1,1105,
                             1304,2105,105),
         NewArea:=CountyCode]
  
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
  HHBase[NewArea==105,NewArea2:="Sh_Rasht"]
  HHBase[NewArea==1105,NewArea2:="Sh_Zahedan"]
  HHBase[NewArea==808,NewArea2:="Sh_Kerman"]
  HHBase[NewArea==1304,NewArea2:="Sh_Hamedan"]
  HHBase[NewArea==2105,NewArea2:="Sh_Yazd"]
  HHBase[NewArea==1 & CountyCode==1 ,NewArea2:="Sh_Arak"]
 

library(readxl)
library(data.table)
library(stringr)

#P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))







  
  if(year<=84){
    EduCodeT <- EduCodesA
  }else if(year %in% 85:92){
    EduCodeT <- EduCodesB
  }else{
    EduCodeT <- EduCodesC
  }
  
  P1 <- rbind(Tables[[paste0("R",year,"P1")]],Tables[[paste0("U",year,"P1")]])
  nP1 <- names(P1)
  if(length(which(sapply(P1, is.character)))>0){
    P1c <- P1[,lapply(.SD,iconv,"WINDOWS-1252","UTF-8"), .SDcols=sapply(P1,is.character)] 
    P1nc <- P1[,!sapply(P1,is.character),with=FALSE]
    P1 <- cbind(P1c,P1nc)[,nP1,with=FALSE]
  }
  
  a <- unlist(P1Cols[P1Cols$Year==year,])
  ind <- which(!is.na(a))[-1]
  setnames(P1,a[ind],names(a[ind]))
  
  f <- function(x){as.numeric(str_trim(x))}
  P1 <- P1[, lapply(.SD, f)] #  , .SDcols=which(sapply(P1, class)=="character")]
  
  
  P1[is.na(Age),Age:=0L]
  P1[,Relationship :=factor(Relationship, levels=1:9, 
                            labels=c("Head","Spouse","Child","Child-in-Law",
                                     "Grand-Child","Parent","Sister/Brother",
                                     "Other Family","Non-Family"))]
  
  P1[,Sex := factor(Sex, levels=1:2,
                    labels=c("Male","Female"))]
  
  if(year %in% 63:64){
    P1[,Literate:=LitState<=3]
  }else if(year %in% 65:68){
    P1[,Literate:=LitState<=2]
  }else{
    P1[,Literate:=Literate==1]
  }
  
  if(year %in% 63:68){
    P1[,Student:=LitState==1]
  }else{
    P1[,Student:=Student==1]
  }
  
  P1[,EduYears:=EduCodeT$yoe[match(EduCode,EduCodeT$Code)]]
  P1$EduYears[P1$Literate==FALSE] <- 0
  P1[,EduLevel:=cut(EduYears,breaks=c(-1,0,6,12,22),
                    labels= c("Illiterate","Primary","Secondary","University"))]
  P1[,EduLevel0:=cut(EduYears,breaks=c(-1,0,6,9,11,12,22),
                     labels= c("Illiterate","Elementary","Middle","High","Pre","University"))]
  
  P1[,ActivityState:=as.numeric(substr(as.character(ActivityState),1,1))]
  
  if(year %in% 63:64){
    P1[,ActivityState:=factor(ActivityState,1:9,
                              c("Employed","Seasonal Unemployed","Income without Work",
                                "Unemployed","Student","Housekeeper","Unemployed Not Looking for Job",
                                "Other","Former Member"))]
    levels(P1$ActivityState) <- c(c("Employed","Unemployed","Income without Work",
                                    "Unemployed","Student","Housekeeper","Other",
                                    "Other","Other"))
  }else if(year %in% 65:68){
    P1[,ActivityState:=factor(ActivityState,1:8,
                              c("Employed","Seasonal Unemployed","Income without Work",
                                "Unemployed","Student","Housekeeper",
                                "Other","Former Member"))]
    levels(P1$ActivityState) <- c(c("Employed","Unemployed","Income without Work",
                                    "Unemployed","Student","Housekeeper","Other",
                                    "Other"))
  }else{
    P1[,ActivityState:=factor(ActivityState,1:6,
                              c("Employed","Unemployed","Income without Work",
                                "Student","Housekeeper","Other"))]
  }
  
  if(year %in% 66:68){
    P1[,MarritalState:=factor(NA,1:4,
                              c("Married","Widowed","Divorced","Bachelor"))]
  }else{
    P1[,MarritalState:=factor(MarritalState,1:4,
                              c("Married","Widowed","Divorced","Bachelor"))]
  }
  
  
  
  
  P <- copy(P1)
  
  P <- P[order(P$HHID),]
  
 
  B <- P[IndivNo==1]
  setnames(B,2:length(B),sapply(X=names(B)[2:length(B)],function(X){paste("H",X,sep="")}))
  B <- B[order(HHID,HIndivNo)]
  B <- B[!duplicated(B$HHID),]
  
  B[,HEmployed:=HActivityState=="Employed"]
  B[,HUnemployed:=HActivityState=="Unemployed"]
  B[,HIncomeWOWork:=HActivityState=="Income without Work"]
  
  B <- B[!is.na(HActivityState) & !is.na(HLiterate)]
  
  
  P[,Size:=1]
  P[,NKids:=ifelse(Age<18,1,0)]
  
  P[,NInfants:=ifelse(Age<=2,1,0)]
  P[,NSmallKids:=ifelse(Age>=3 & Age<=13, 1, 0)]
  
  P[,NElementary:= ifelse(EduLevel0=="Elementary" & Student==TRUE,1,0)]
  P[,NMiddle:= ifelse(EduLevel0=="Middle" & Student==TRUE,1,0)]
  P[,NHigh:= ifelse(EduLevel0=="High" & Student==TRUE,1,0)]
  P[,NPre:= ifelse(EduLevel0=="Pre" & Student==TRUE,1,0)]
  
  
  PSum <- P[,lapply(.SD,sum,na.rm=TRUE),
            .SDcols=c("Size","NKids","NInfants","NSmallKids","NElementary",
                      "NMiddle","NHigh","NPre"),#,"TotalIncome"),
            by="HHID"]
  

  HHI <- merge(B,PSum,by="HHID")
  HHI[,EqSizeRevOECD := ifelse(Size==NKids,1+(NKids-1)*0.5,
                               1 + (Size-NKids-1)*0.7 + (NKids)*0.5)]
  HHI <- HHI[!is.na(HLiterate)]
  
  rm(P,B,PSum)

  
  HHBase<-merge(HHI,HHBase)
    rm(HHI)
  
    load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
    HHWeights<- as.data.table(HHWeights)
    HHWeights[,Year:=NULL]
    HHBase<-merge(HHBase,HHWeights)
  
  HHBase[HSex=="Female",weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Size==1,
         weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female",weighted.mean(HAge,Weight),by=.(HMarritalState)]
  HHBase[HSex=="Female",weighted.mean(HAge,Weight)]
  
  HHBase[HSex=="Female" & HAge<35,
         weighted.mean(HAge,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & HAge>35 & HAge<65,
         weighted.mean(HAge,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & HAge>65,
         weighted.mean(HAge,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female",
         weighted.mean(Size,Weight),by=.(HMarritalState)]
  HHBase[HSex=="Female",weighted.mean(Size,Weight)]
  
  HHBase[HSex=="Female",
         weighted.mean(HIndivNo,Weight)*sum(Weight),
         by=.(HLiterate,HMarritalState)][order(HMarritalState,HLiterate)]
  
  HHBase[HSex=="Female",
         weighted.mean(HLiterate==FALSE,Weight)]

  HHBase[HSex=="Female" & HEduYears>12,
         weighted.mean(HIndivNo,Weight)*sum(Weight),
         by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & (HLiterate==TRUE | NElementary>0 |
                             NMiddle>0 | NHigh>0) ,
         weighted.mean(HIndivNo,Weight)*sum(Weight),
         by=.(HMarritalState)]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoor.rda"))
  HHBase<-merge(HHBase,MD[,.(HHID,Decile,Total_Exp_Month_Per_nondurable,
                             FoodExpenditure,Behdasht_Exp)])
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  HHBase<-merge(HHBase,HHHouseProperties[,.(HHID,tenure)])
  
  HHBase[HSex=="Female" & Decile==1,
         weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Decile==2,
         weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Urban",
         weighted.mean(Total_Exp_Month_Per_nondurable,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Rural",
         weighted.mean(Total_Exp_Month_Per_nondurable,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Urban",
         weighted.mean(Behdasht_Exp/EqSizeRevOECD,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Rural",
         weighted.mean(Behdasht_Exp/EqSizeRevOECD,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Urban",
         weighted.mean(FoodExpenditure/Size,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & Region=="Rural",
         weighted.mean(FoodExpenditure/Size,Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & (tenure=="OwnLandandBuilding" | tenure=="Apartment"),
         weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
  HHBase[HSex=="Female" & (tenure=="Rented" | tenure=="Mortgage"),
         weighted.mean(HIndivNo,Weight)*sum(Weight),by=.(HMarritalState)]
  
endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)