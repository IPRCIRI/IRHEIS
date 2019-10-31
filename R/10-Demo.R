# 10-Demo.R
# Builds the demographics information data.table for households
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Demo =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(plotrix)

#P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))
EduCodesD <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_D))

years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
  if(year<=84){
    EduCodeT <- EduCodesA
  }else if(year %in% 85:92){
    EduCodeT <- EduCodesB
  }else if(year %in% 93:96){
    EduCodeT <- EduCodesC
  }else{
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
  
  # B <- P[P[Age>=10,.SD,#.I[TotalIncome==max(TotalIncome)],
  #          by=HHID][,V1]][,c("HHID","IndivNo","Sex","Age",
  #                            "Literate","Student",#"EduYears","EduLevel",
  #                            "ActivityState","MarritalState"),with=FALSE]  
  B <- P[IndivNo==1]
  setnames(B,2:length(B),sapply(X=names(B)[2:length(B)],function(X){paste("H",X,sep="")}))
  B <- B[order(HHID,HIndivNo)]
  B <- B[!duplicated(B$HHID),]
  
  B[,HEmployed:=HActivityState=="Employed"]
  B[,HUnemployed:=HActivityState=="Unemployed"]
  B[,HIncomeWOWork:=HActivityState=="Income without Work"]
  
  B <- B[!is.na(HActivityState) & !is.na(HLiterate)]
  
  
  P[,Size:=1]
  P[,NKids:=ifelse(Age<15,1,0)]
  
  P[,NInfants:=ifelse(Age<=2,1,0)]
  P[,NSmallKids:=ifelse(Age>=3 & Age<=13, 1, 0)]

  P[,NElementary:= ifelse(EduLevel0=="Elementary" & Student==TRUE,1,0)]
  P[,NMiddle:= ifelse(EduLevel0=="Middle" & Student==TRUE,1,0)]
  P[,NHigh:= ifelse(EduLevel0=="High" & Student==TRUE,1,0)]
  P[,NPre:= ifelse(EduLevel0=="Pre" & Student==TRUE,1,0)]
  
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
  
  #Age Groups 2
  P[,NAge1_A_B:=ifelse(Age==0 & Sex=="Male",1,0)]
  P[,NAge1_A_G:=ifelse(Age==0 & Sex=="Female",1,0)]
  P[,NAge2_A_B:=ifelse(Age==1 & Sex=="Male",1,0)]
  P[,NAge2_A_G:=ifelse(Age==1 & Sex=="Female",1,0)]
  P[,NAge3_A_B:=ifelse(Age>=2 & Age<=3 & Sex=="Male",1,0)]
  P[,NAge3_A_G:=ifelse(Age>=2 & Age<=3 & Sex=="Female",1,0)]
  P[,NAge4_A_B:=ifelse(Age>=4 & Age<=5 & Sex=="Male",1,0)]
  P[,NAge4_A_G:=ifelse(Age>=4 & Age<=5 & Sex=="Female",1,0)]
  P[,NAge5_A_B:=ifelse(Age<=11 & Age>=6 & Sex=="Male",1,0)]
  P[,NAge5_A_G:=ifelse(Age<=11 & Age>=6 & Sex=="Female",1,0)]
  P[,NAge6_A_B:=ifelse(Age<=17 & Age>=12 & Sex=="Male",1,0)]
  P[,NAge6_A_G:=ifelse(Age<=17 & Age>=12 & Sex=="Female",1,0)]
  P[,NAge7_A_B:=ifelse(Age<=29 & Age>=18 & Sex=="Male",1,0)]
  P[,NAge7_A_G:=ifelse(Age<=29 & Age>=18 & Sex=="Female",1,0)]
  P[,NAge8_A_B:=ifelse(Age<=60 & Age>=30 & Sex=="Male",1,0)]
  P[,NAge8_A_G:=ifelse(Age<=60 & Age>=30 & Sex=="Female",1,0)]
  P[,NAge9_A_B:=ifelse(Age>60 & Sex=="Male",1,0)]
  P[,NAge9_A_G:=ifelse(Age>60 & Sex=="Female",1,0)]
  P<- merge(P,lactating,by="HHID",all.x = TRUE)
  P[,Calorie_Need1:=NAge1B*Settings$KCaloryNeed_B1+
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
      lactating*(Settings$KCaloryNeed_lactating)]
  
  
  P[,Calorie_Need2:=NAge1_A_B*Settings$KCaloryNeed_A_B1+
      NAge2_A_B*Settings$KCaloryNeed_A_B2+
      NAge3_A_B*Settings$KCaloryNeed_A_B3+
      NAge4_A_B*Settings$KCaloryNeed_A_B4+
      NAge5_A_B*Settings$KCaloryNeed_A_B5+
      NAge6_A_B*Settings$KCaloryNeed_A_B6+
      NAge7_A_B*Settings$KCaloryNeed_A_B7+
      NAge8_A_B*Settings$KCaloryNeed_A_B8+
      NAge9_A_B*Settings$KCaloryNeed_A_B9+
      NAge1_A_G*Settings$KCaloryNeed_A_G1+
      NAge2_A_G*Settings$KCaloryNeed_A_G2+
      NAge3_A_G*Settings$KCaloryNeed_A_G3+
      NAge4_A_G*Settings$KCaloryNeed_A_G4+
      NAge5_A_G*Settings$KCaloryNeed_A_G5+
      NAge6_A_G*Settings$KCaloryNeed_A_G6+
      NAge7_A_G*Settings$KCaloryNeed_A_G7+
      NAge8_A_G*Settings$KCaloryNeed_A_G8+
      NAge9_A_G*Settings$KCaloryNeed_A_G9+
      lactating*(Settings$KCaloryNeed_lactating)]
  

  
  PSum <- P[,lapply(.SD,sum,na.rm=TRUE),
            .SDcols=c("Size","NKids","NInfants","NSmallKids","NElementary",
                     "NMiddle","NHigh","NPre","NAge1B","NAge1G",
                     "NAge2B","NAge2G","NAge3B","NAge3G","NAge4B","NAge4G",
                     "NAge5B","NAge5G","NAge6B","NAge6G","NAge7B","NAge7G"
                     ,"NAge8B","NAge8G","NAge9B","NAge9G","NAge10B","NAge10G",
                     "NAge1_A_B","NAge1_A_G","Calorie_Need1","Calorie_Need2",
                     "NAge2_A_B","NAge2_A_G","NAge3_A_B","NAge3_A_G","NAge4_A_B","NAge4_A_G",
                     "NAge5_A_B","NAge5_A_G","NAge6_A_B","NAge6_A_G","NAge7_A_B","NAge7_A_G"
                     ,"NAge8_A_B","NAge8_A_G","NAge9_A_B","NAge9_A_G"),#,"TotalIncome"),
            by="HHID"]

#  PSum <- PSum[TotalIncome>0]
  
  HHI <- merge(B,PSum,by="HHID")
  HHI[,EqSizeRevOECD := ifelse(Size==NKids,1+(NKids-1)*0.5,
                               1 + (Size-NKids-1)*0.7 + (NKids)*0.5)]
  HHI <- HHI[!is.na(HLiterate)]

  rm(P,B,PSum)
  save(HHI,year,file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  
  
  rm(HHI)
  
  # P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]
  # P1<-P1[Relationship== 'Head']
  # HHBase<-merge(HHBase,P1,by =c("HHID"),all=TRUE)
  # save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights>-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  P1<-merge(P1,HHWeights)
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
  
  
  weighted.hist(P1$Age,P1$Weight,breaks=1:99,main="Age weighted histogram in Iran (1397)")

  P1<-merge(P1,HHBase)
  P1<- merge(P1,lactating,by="HHID",all.x = TRUE)
  
  P1U<-P1[Region=="Urban"]
  weighted.hist(P1U$Age,P1U$Weight,breaks=1:99,main="Age weighted histogram in Urban Areas (1397)")
  
  P1R<-P1[Region=="Rural"]
  weighted.hist(P1R$Age,P1R$Weight,breaks=1:99,main="Age weighted histogram in Rural reas (1397)")
  
  plot(density(P1$Age),weights =P1$Weight)
  lines(density(P1U$Age),weights =P1U$Weight)
  lines(density(P1R$Age),weights =P1R$Weight)
  
  weighted.hist(P1$Age,P1$Weight,breaks=c(0,1,2,3,4,10,15,20,60),
                main="Age weighted histogram in Iran (1397)")
 

  
  P1[,B1:=ifelse(Age==0 & Sex=="Male",1,0)]
  P1[,B2:=ifelse(Age==1 & Sex=="Male",1,0)]
  P1[,B3:=ifelse(Age==2 &  Sex=="Male",1,0)]
  P1[,B4:=ifelse(Age==3 &  Sex=="Male",1,0)]
  P1[,B5:=ifelse(Age==4 &  Sex=="Male",1,0)]
  P1[,B6:=ifelse(Age>=5 & Age<=9 & Sex=="Male",1,0)]
  P1[,B7:=ifelse(Age>=10 & Age<=14 & Sex=="Male",1,0)]
  P1[,B8:=ifelse(Age>=15 & Age<=19 & Sex=="Male",1,0)]
  P1[,B9:=ifelse(Age>=20 & Age<=59 & Sex=="Male",1,0)]
  P1[,B10:=ifelse(Age>60 & Sex=="Male",1,0)] 
  P1[,G1:=ifelse(Age==0 & Sex=="Female",1,0)]
  P1[,G2:=ifelse(Age==1 & Sex=="Female",1,0)]
  P1[,G3:=ifelse(Age==2 &  Sex=="Female",1,0)]
  P1[,G4:=ifelse(Age==3 &  Sex=="Female",1,0)]
  P1[,G5:=ifelse(Age==4 &  Sex=="Female",1,0)]
  P1[,G6:=ifelse(Age>=5 & Age<=9 & Sex=="Female",1,0)]
  P1[,G7:=ifelse(Age>=10 & Age<=14 & Sex=="Female",1,0)]
  P1[,G8:=ifelse(Age>=15 & Age<=19 & Sex=="Female",1,0)]
  P1[,G9:=ifelse(Age>=20 & Age<=59 & Sex=="Female",1,0)]
  P1[,G10:=ifelse(Age>60 & Sex=="Female",1,0)] 
  
  P1[,weighted.mean(B1,Weight)]
  P1[,weighted.mean(B2,Weight)]
  P1[,weighted.mean(B3,Weight)]
  P1[,weighted.mean(B4,Weight)]
  P1[,weighted.mean(B5,Weight)]
  P1[,weighted.mean(B6,Weight)]
  P1[,weighted.mean(B7,Weight)]
  P1[,weighted.mean(B8,Weight)]
  P1[,weighted.mean(B9,Weight)]
  P1[,weighted.mean(B10,Weight)]
  P1[,weighted.mean(G1,Weight)]
  P1[,weighted.mean(G2,Weight)]
  P1[,weighted.mean(G3,Weight)]
  P1[,weighted.mean(G4,Weight)]
  P1[,weighted.mean(G5,Weight)]
  P1[,weighted.mean(G6,Weight)]
  P1[,weighted.mean(G7,Weight)]
  P1[,weighted.mean(G8,Weight)]
  P1[,weighted.mean(G9,Weight)]
  P1[,weighted.mean(G10,Weight)]
  
  P1<-P1[,Calorie_Need_WorldBank:=
           weighted.mean(B1,Weight)*Settings$KCaloryNeed_B1 +
           weighted.mean(G1,Weight)*Settings$KCaloryNeed_G1 +
           weighted.mean(B2,Weight)*Settings$KCaloryNeed_B2 +
           weighted.mean(G2,Weight)*Settings$KCaloryNeed_G2 +
           weighted.mean(B3,Weight)*Settings$KCaloryNeed_B3 +
           weighted.mean(G3,Weight)*Settings$KCaloryNeed_G3 +
           weighted.mean(B4,Weight)*Settings$KCaloryNeed_B4 +
           weighted.mean(G4,Weight)*Settings$KCaloryNeed_G4 +
           weighted.mean(B5,Weight)*Settings$KCaloryNeed_B5 +
           weighted.mean(G5,Weight)*Settings$KCaloryNeed_G5 +
           weighted.mean(B6,Weight)*Settings$KCaloryNeed_B6 +
           weighted.mean(G6,Weight)*Settings$KCaloryNeed_G6 +
           weighted.mean(B7,Weight)*Settings$KCaloryNeed_B7 +
           weighted.mean(G7,Weight)*Settings$KCaloryNeed_G7 +
           weighted.mean(B8,Weight)*Settings$KCaloryNeed_B8 +
           weighted.mean(G8,Weight)*Settings$KCaloryNeed_G8 +
           weighted.mean(B9,Weight)*Settings$KCaloryNeed_B9 +
           weighted.mean(G9,Weight)*Settings$KCaloryNeed_G9 +
           weighted.mean(B10,Weight)*Settings$KCaloryNeed_B10 +
           weighted.mean(G10,Weight)*Settings$KCaloryNeed_G10+
           weighted.mean(lactating,Weight)*(Settings$KCaloryNeed_lactating)]
  
  
  
  P1[,BA1:=ifelse(Age==0 & Sex=="Male",1,0)]
  P1[,BA2:=ifelse(Age==1 & Sex=="Male",1,0)]
  P1[,BA3:=ifelse(Age>=2 & Age<=3 & Sex=="Male",1,0)]
  P1[,BA4:=ifelse(Age>=4 & Age<=5 & Sex=="Male",1,0)]
  P1[,BA5:=ifelse(Age>=6 & Age<=11 & Sex=="Male",1,0)]
  P1[,BA6:=ifelse(Age>=12 & Age<=17 & Sex=="Male",1,0)]
  P1[,BA7:=ifelse(Age>=18 & Age<=29 & Sex=="Male",1,0)]
  P1[,BA8:=ifelse(Age>=30 & Age<=60 & Sex=="Male",1,0)]
  P1[,BA9:=ifelse(Age>60 & Sex=="Male",1,0)] 
  P1[,GA1:=ifelse(Age==0 & Sex=="Female",1,0)]
  P1[,GA2:=ifelse(Age==1 & Sex=="Female",1,0)]
  P1[,GA3:=ifelse(Age>=2 & Age<=3 & Sex=="Female",1,0)]
  P1[,GA4:=ifelse(Age>=4 & Age<=5 & Sex=="Female",1,0)]
  P1[,GA5:=ifelse(Age>=6 & Age<=11 & Sex=="Female",1,0)]
  P1[,GA6:=ifelse(Age>=12 & Age<=17 & Sex=="Female",1,0)]
  P1[,GA7:=ifelse(Age>=18 & Age<=29 & Sex=="Female",1,0)]
  P1[,GA8:=ifelse(Age>=30 & Age<=60 & Sex=="Female",1,0)]
  P1[,GA9:=ifelse(Age>60 & Sex=="Female",1,0)]
  
  P1[,weighted.mean(BA1,Weight)]
  P1[,weighted.mean(BA2,Weight)]
  P1[,weighted.mean(BA3,Weight)]
  P1[,weighted.mean(BA4,Weight)]
  P1[,weighted.mean(BA5,Weight)]
  P1[,weighted.mean(BA6,Weight)]
  P1[,weighted.mean(BA7,Weight)]
  P1[,weighted.mean(BA8,Weight)]
  P1[,weighted.mean(BA9,Weight)]
  P1[,weighted.mean(GA1,Weight)]
  P1[,weighted.mean(GA2,Weight)]
  P1[,weighted.mean(GA3,Weight)]
  P1[,weighted.mean(GA4,Weight)]
  P1[,weighted.mean(GA5,Weight)]
  P1[,weighted.mean(GA6,Weight)]
  P1[,weighted.mean(GA7,Weight)]
  P1[,weighted.mean(GA8,Weight)]
  P1[,weighted.mean(GA9,Weight)]
  
  P1<-P1[,Calorie_Need_Anstitoo:=
           weighted.mean(BA1,Weight)*Settings$KCaloryNeed_A_B1 +
           weighted.mean(GA1,Weight)*Settings$KCaloryNeed_A_G1 +
           weighted.mean(BA2,Weight)*Settings$KCaloryNeed_A_B2 +
           weighted.mean(GA2,Weight)*Settings$KCaloryNeed_A_G2 +
           weighted.mean(BA3,Weight)*Settings$KCaloryNeed_A_B3 +
           weighted.mean(GA3,Weight)*Settings$KCaloryNeed_A_G3 +
           weighted.mean(BA4,Weight)*Settings$KCaloryNeed_A_B4 +
           weighted.mean(GA4,Weight)*Settings$KCaloryNeed_A_G4 +
           weighted.mean(BA5,Weight)*Settings$KCaloryNeed_A_B5 +
           weighted.mean(GA5,Weight)*Settings$KCaloryNeed_A_G5 +
           weighted.mean(BA6,Weight)*Settings$KCaloryNeed_A_B6 +
           weighted.mean(GA6,Weight)*Settings$KCaloryNeed_A_G6 +
           weighted.mean(BA7,Weight)*Settings$KCaloryNeed_A_B7 +
           weighted.mean(GA7,Weight)*Settings$KCaloryNeed_A_G7 +
           weighted.mean(BA8,Weight)*Settings$KCaloryNeed_A_B8 +
           weighted.mean(GA8,Weight)*Settings$KCaloryNeed_A_G8 +
           weighted.mean(BA9,Weight)*Settings$KCaloryNeed_A_B9 +
           weighted.mean(GA9,Weight)*Settings$KCaloryNeed_A_G9 +
           weighted.mean(lactating,Weight)*Settings$KCaloryNeed_lactating]
  
  cat(P1[,mean(Calorie_Need_WorldBank)],"\n")
  cat(P1[,mean(Calorie_Need_Anstitoo)],"\n")
  
  Calorie_Need<-P1[,.(Calorie_Need_WorldBank=mean(Calorie_Need_WorldBank),
                      Calorie_Need_Anstitoo=mean(Calorie_Need_Anstitoo)),by="HHID"]
  save(Calorie_Need,file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))
    }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)