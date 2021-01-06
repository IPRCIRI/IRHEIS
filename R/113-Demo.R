# 113-Demo.R
# Builds the demographics information data.table for households, and then based
# on the age structure and presence of lactating women, calculated the calories
# needed for each household (Based on calorie need tables by the World Bank and
# and the Nutrition Institute)
#
# Copyright © 2016-2020: Majid Einian and Zahra Shahidi and ?
# License: GPL-3
rm(list=ls())

starttime <- proc.time()
cat("\n\n================== Demographics & Calorie Need ====================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))
EduCodesD <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_D))

years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InfantMilk.rda"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  
  
  if(year<=84){
    EduCodeT <- EduCodesA
  }else if(year %in% 85:92){
    EduCodeT <- EduCodesB
  }else if(year %in% 93:96){
    EduCodeT <- EduCodesC
  }else if(year >=97){
    EduCodeT <- EduCodesD
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
  
  P1<-P1[Sex=="Male" | Sex=="Female"]
  
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
  P1[,EduLevel0:=cut(EduYears,breaks=c(-1,0,6,12,22),
                    labels= c("Illiterate","Primary","Secondary","University"))]
  P1[,EduLevel1:=cut(EduYears,breaks=c(-1,0,6,9,11,12,22),
                     labels= c("Illiterate","Elementary","Middle","High","Pre","University"))]
  P1[,EduLevel:=cut(EduYears,breaks=c(-1,0,6,12,14,16,18,23),
                     labels= c("Illiterate","Primary","Secondary",
                               "College","Bachelors","Masters","PhD"))]
  
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
  
### Define Age Groups ==========================================  
  P[,Size:=1]
  P[,Kids:=ifelse(Relationship=="Child",1,0)]
  P[,NKids:=ifelse(Age<15,1,0)]
  P[,NInfants:=ifelse(Age<=2,1,0)]
  P[,NInfants0:=ifelse(Age==0,1,0)]
  P[,NStudents:=ifelse(Age<=18 & Age>=6,1,0)]
  
  P[,NSmallKids:=ifelse(Age>=3 & Age<=13, 1, 0)]

  P[,NElementary:= ifelse(EduLevel=="Elementary" & Student==TRUE,1,0)]
  P[,NMiddle:= ifelse(EduLevel=="Middle" & Student==TRUE,1,0)]
  P[,NHigh:= ifelse(EduLevel=="High" & Student==TRUE,1,0)]
  P[,NCollege:=ifelse(EduLevel=="College" & Student==TRUE,1,0)]
  P[,NBachelors:=ifelse(EduLevel=="Bachelors" & Student==TRUE,1,0)]
  P[,NMasters:=ifelse(EduLevel=="Masters" & Student==TRUE,1,0)]
  P[,NPhD:=ifelse(EduLevel=="PhD" & Student==TRUE,1,0)]
  P[,NUniv:=NCollege+NBachelors+NMasters+NPhD]
  P[,NLiterate:=ifelse(EduLevel!="Illiterate",1,0)]
  P[,NEmployed:=ifelse(ActivityState=="Employed",1,0)]
  
  #Age Groups 1
  P[,NAge1B:=ifelse(Age==0 & Sex=="Male",1,0)]
  P[,NAge1G:=ifelse(Age==0 & Sex=="Female",1,0)]
  P[,NAge2B:=ifelse(Age==1 & Sex=="Male",1,0)]
  P[,NAge2G:=ifelse(Age==1 & Sex=="Female",1,0)]
  P[,NAge3B:=ifelse(Age==2 & Sex=="Male",1,0)]
  P[,NAge3G:=ifelse(Age==2 & Sex=="Female",1,0)]
  P[,NAge4B:=ifelse(Age==3 & Sex=="Male",1,0)]
  P[,NAge4G:=ifelse(Age==3 & Sex=="Female",1,0)]
  P[,NAge5B:=ifelse(Age==4 & Sex=="Male",1,0)]
  P[,NAge5G:=ifelse(Age==4 & Sex=="Female",1,0)]
  P[,NAge6B:=ifelse(Age<=9 & Age>4 & Sex=="Male",1,0)]
  P[,NAge6G:=ifelse(Age<=9 & Age>4 & Sex=="Female",1,0)]
  P[,NAge7B:=ifelse(Age<=14 & Age>9 & Sex=="Male",1,0)]
  P[,NAge7G:=ifelse(Age<=14 & Age>9 & Sex=="Female",1,0)]
  P[,NAge8B:=ifelse(Age<=19 & Age>14 & Sex=="Male",1,0)]
  P[,NAge8G:=ifelse(Age<=19 & Age>14 & Sex=="Female",1,0)]
  P[,NAge9B:=ifelse(Age<=59 & Age>19 & Sex=="Male",1,0)]
  P[,NAge9G:=ifelse(Age<=59 & Age>19 & Sex=="Female",1,0)]
  P[,NAge10B:=ifelse(Age>59 & Sex=="Male",1,0)]
  P[,NAge10G:=ifelse(Age>59 & Sex=="Female",1,0)]
  P[,NDabestan:=ifelse(Age>=7 & Age<=11,1,0)]
  P[,NRahnamayi:=ifelse(Age>=12 & Age<=14,1,0)]
  P[,NDabirestan:=ifelse(Age>=15 & Age<=17,1,0)]
  P[,NPish:=ifelse(Age==18,1,0)]
  #Age Groups 2
  P[,NAge1_NutInst_B:=ifelse(Age==0 & Sex=="Male",1,0)]
  P[,NAge1_NutInst_G:=ifelse(Age==0 & Sex=="Female",1,0)]
  P[,NAge2_NutInst_B:=ifelse(Age==1 & Sex=="Male",1,0)]
  P[,NAge2_NutInst_G:=ifelse(Age==1 & Sex=="Female",1,0)]
  P[,NAge3_NutInst_B:=ifelse(Age>=2 & Age<=3 & Sex=="Male",1,0)]
  P[,NAge3_NutInst_G:=ifelse(Age>=2 & Age<=3 & Sex=="Female",1,0)]
  P[,NAge4_NutInst_B:=ifelse(Age>=4 & Age<=5 & Sex=="Male",1,0)]
  P[,NAge4_NutInst_G:=ifelse(Age>=4 & Age<=5 & Sex=="Female",1,0)]
  P[,NAge5_NutInst_B:=ifelse(Age<=11 & Age>=6 & Sex=="Male",1,0)]
  P[,NAge5_NutInst_G:=ifelse(Age<=11 & Age>=6 & Sex=="Female",1,0)]
  P[,NAge6_NutInst_B:=ifelse(Age<=17 & Age>=12 & Sex=="Male",1,0)]
  P[,NAge6_NutInst_G:=ifelse(Age<=17 & Age>=12 & Sex=="Female",1,0)]
  P[,NAge7_NutInst_B:=ifelse(Age<=29 & Age>=18 & Sex=="Male",1,0)]
  P[,NAge7_NutInst_G:=ifelse(Age<=29 & Age>=18 & Sex=="Female",1,0)]
  P[,NAge8_NutInst_B:=ifelse(Age<=60 & Age>=30 & Sex=="Male",1,0)]
  P[,NAge8_NutInst_G:=ifelse(Age<=60 & Age>=30 & Sex=="Female",1,0)]
  P[,NAge9_NutInst_B:=ifelse(Age>60 & Sex=="Male",1,0)]
  P[,NAge9_NutInst_G:=ifelse(Age>60 & Sex=="Female",1,0)]
  
  PSum <- P[,lapply(.SD,sum,na.rm=TRUE),
            .SDcols=c("Size","NKids","NInfants","NInfants0","NSmallKids",
                      "NElementary","NHigh","NMiddle","NStudents",
                      "NCollege","NBachelors","NMasters","NPhD","NUniv",
                      "NAge1B","NAge1G",
                      "NAge2B","NAge2G",
                      "NAge3B","NAge3G",
                      "NAge4B","NAge4G",
                      "NAge5B","NAge5G",
                      "NAge6B","NAge6G",
                      "NAge7B","NAge7G",
                      "NAge8B","NAge8G",
                      "NAge9B","NAge9G",
                      "NAge10B","NAge10G",
                      "NAge1_NutInst_B","NAge1_NutInst_G",
                      "NAge2_NutInst_B","NAge2_NutInst_G",
                      "NAge3_NutInst_B","NAge3_NutInst_G",
                      "NAge4_NutInst_B","NAge4_NutInst_G",
                      "NAge5_NutInst_B","NAge5_NutInst_G",
                      "NAge6_NutInst_B","NAge6_NutInst_G",
                      "NAge7_NutInst_B","NAge7_NutInst_G",
                      "NAge8_NutInst_B","NAge8_NutInst_G",
                      "NAge9_NutInst_B","NAge9_NutInst_G",
                      "NDabestan","NRahnamayi",
                      "NDabirestan","NPish",
                      "NEmployed","NLiterate"),
            by="HHID"]
  
  
  PSum<- merge(PSum,TInfantMilk,by="HHID",all.x = TRUE)
  PSum[is.na(InfantMilkExpenditure), InfantMilkExpenditure := 0]
  PSum<-PSum[,Lactating:=ifelse(NInfants0>0 & InfantMilkExpenditure==0,1,0)]
  
  PSum[,Calorie_Need_WorldBank:=NAge1B*Settings$KCaloryNeed_B1+
         NAge2B*Settings$KCaloryNeed_B2+
         NAge3B*Settings$KCaloryNeed_B3+
         NAge4B*Settings$KCaloryNeed_B4+
         NAge5B*Settings$KCaloryNeed_B5+
         NAge6B*Settings$KCaloryNeed_B6+
         NAge7B*Settings$KCaloryNeed_B7+
         NAge8B*Settings$KCaloryNeed_B8+
         NAge9B*Settings$KCaloryNeed_B9+
         NAge10B*Settings$KCaloryNeed_B10+
         NAge1G*Settings$KCaloryNeed_G1+
         NAge2G*Settings$KCaloryNeed_G2+
         NAge3G*Settings$KCaloryNeed_G3+
         NAge4G*Settings$KCaloryNeed_G4+
         NAge5G*Settings$KCaloryNeed_G5+
         NAge6G*Settings$KCaloryNeed_G6+
         NAge7G*Settings$KCaloryNeed_G7+
         NAge8G*Settings$KCaloryNeed_G8+
         NAge9G*Settings$KCaloryNeed_G9+
         NAge10G*Settings$KCaloryNeed_G10+
         Lactating*(Settings$KCaloryNeed_lactating)]
  
  PSum[,Calorie_Need_NutritionInstitute:=NAge1_NutInst_B*Settings$KCaloryNeed_NutInst_B1+
         NAge2_NutInst_B*Settings$KCaloryNeed_NutInst_B2+
         NAge3_NutInst_B*Settings$KCaloryNeed_NutInst_B3+
         NAge4_NutInst_B*Settings$KCaloryNeed_NutInst_B4+
         NAge5_NutInst_B*Settings$KCaloryNeed_NutInst_B5+
         NAge6_NutInst_B*Settings$KCaloryNeed_NutInst_B6+
         NAge7_NutInst_B*Settings$KCaloryNeed_NutInst_B7+
         NAge8_NutInst_B*Settings$KCaloryNeed_NutInst_B8+
         NAge9_NutInst_B*Settings$KCaloryNeed_NutInst_B9+
         NAge1_NutInst_G*Settings$KCaloryNeed_NutInst_G1+
         NAge2_NutInst_G*Settings$KCaloryNeed_NutInst_G2+
         NAge3_NutInst_G*Settings$KCaloryNeed_NutInst_G3+
         NAge4_NutInst_G*Settings$KCaloryNeed_NutInst_G4+
         NAge5_NutInst_G*Settings$KCaloryNeed_NutInst_G5+
         NAge6_NutInst_G*Settings$KCaloryNeed_NutInst_G6+
         NAge7_NutInst_G*Settings$KCaloryNeed_NutInst_G7+
         NAge8_NutInst_G*Settings$KCaloryNeed_NutInst_G8+
         NAge9_NutInst_G*Settings$KCaloryNeed_NutInst_G9+
         Lactating*(Settings$KCaloryNeed_lactating)]
  
  HHWeights <- as.data.table(HHWeights)
  HHWeights <- HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  HHI <- merge(B,PSum,by="HHID")
  HHI <- merge(HHI,HHWeights,by="HHID")
  # Calorie_Need<-HHI[,.(Calorie_Need_WorldBank=weighted.mean(Calorie_Need1,Weight),
  #                     Calorie_Need_NutritionInstitute=weighted.mean(Calorie_Need2,Weight)),by="HHID"]
  
  HHI[,EqSizeOECD := ifelse(Size==NKids,1+(NKids-1)*0.5,
                               1 + (Size-NKids-1)*0.7 + (NKids)*0.5)]
  #HHI[,EqSizeOECD := ifelse(Size==NKids,1+(NKids-1)*0.3,
  #                         1 + (Size-NKids-1)*0.5 + (NKids)*0.3)]
  #HHI[,EqSizeOECD := sqrt(Size)]
  HHI <- HHI[!is.na(HLiterate)]

  rm(P,B)
  save(HHI,year,file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
#  save(Calorie_Need,file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))

  HHI2<-HHI[NInfants0>0]
 cat(HHI2[,weighted.mean(Lactating,Weight)]) 
  
  }

endtime <- proc.time()
cat("\n\n============================\nIt took ",(endtime-starttime)[3],"seconds")