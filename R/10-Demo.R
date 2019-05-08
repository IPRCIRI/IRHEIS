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

#P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))


years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  if(year<=87){
    EduCodeT <- EduCodesA
  }else if(year %in% 88:92){
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

#  PSum <- PSum[TotalIncome>0]
  
  HHI <- merge(B,PSum,by="HHID")
  HHI[,EqSizeRevOECD := ifelse(Size==NKids,1+(NKids-1)*0.5,
                               1 + (Size-NKids-1)*0.7 + (NKids)*0.5)]
  HHI <- HHI[!is.na(HLiterate)]

  rm(P,B,PSum)
  save(HHI,year,file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  
  
  rm(HHI)
  
  P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]
  P1<-P1[Relationship== 'Head']
  HHBase<-merge(HHBase,P1,by =c("HHID"),all=TRUE)
  save(HHBase, file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  #load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  #HHWeights<- as.data.table(HHWeights)
  #HHWeights[,Year:=NULL]
  #HHBase<-merge(HHBase,HHWeights)
  #HHBase<-HHBase[Region=="Urban"]
  #cat(paste0("--",year))
  #cat(paste0(",",HHBase[,mean(Dimension,na.rm = TRUE)],"\n"))
  #cat(paste0(",",HHBase[,weighted.mean(Dimension,Weight,na.rm = TRUE)],"\n"))
    }

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)


# for(year in  (Settings$startyear:Settings$endyear)){
# #cat("\n=======================\n",year,":\n")
# load(paste0("D:/HEIS/DataProcessed/Y",year,"HHI.rda"))
# load(paste0("D:/HEIS/DataProcessed/Y",year,"HHBase.rda"))
# H <- merge(HHBase,HHI,by="HHID")
# print(transpose(H[,list(A=sum(Size)),by="Region"]))
# 
# }
