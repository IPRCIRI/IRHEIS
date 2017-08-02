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

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))


for(year in (Settings$startyear:Settings$endyear))
{
  cat(paste0("\n------------------------------\nYear:",year,"\n"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  
  years <- Settings$startyear:Settings$endyear
  if(year<=87){
    EduCodeT <- EduCodesA
  }else if(year %in% 88:92){
    EduCodeT <- EduCodesB
  }else{
    EduCodeT <- EduCodesC
  }

  P1 <- rbind(Tables[[paste0("R",year,"P1")]],Tables[[paste0("U",year,"P1")]])
  
  a <- unlist(P1Cols[P1Cols$Year==year,])
  ind <- which(!is.na(a))[-1]
  setnames(P1,a[ind],names(a[ind]))
  
  P1 <- P1[, lapply(.SD, as.numeric)]
  
# #== income part ======
#   if(year <= 68)  {
#     {
#       tbl <- "P4S1"
#       tbu <- Tables[[paste0("U",year,tbl)]]
#       Tb <- rbind(Tables[[paste0("U",year,tbl)]],
#                   Tables[[paste0("R",year,tbl)]])
#       P4PBW <- data.table(rbind2(sqlQuery(cns,paste0("Select ADDRESS, COL01, COL11 From [U",year,"P4S1]")),
#                                  sqlQuery(cns,paste0("Select ADDRESS, COL01, COL09 From [R",year,"P4S1]"))))
#       setnames(P4PBW,c("HHID","IndivNo","PubWageIncome"))
#       P4PBW <- P4PBW[, lapply(.SD, as.numeric)]
#       
#       P4PVW <- data.table(rbind2(sqlQuery(cns,paste0("Select ADDRESS, COL01, COL11 From [U",year,"P4S2]")),
#                                  sqlQuery(cns,paste0("Select ADDRESS, COL01, COL09 From [R",year,"P4S2]"))))
#       setnames(P4PVW,c("HHID","IndivNo","PrvWageIncome"))
#       P4PVW <- P4PVW[, lapply(.SD, as.numeric)]
#       
#       P4AGR <- data.table(rbind2(sqlQuery(cns,paste0("Select ADDRESS, COL01, COL09 From [U",year,"P4S3]")),
#                                  sqlQuery(cns,paste0("Select ADDRESS, COL01, COL09 From [R",year,"P4S3]"))))
#       setnames(P4AGR,c("HHID","IndivNo","AggriIncome"))
#       P4AGR <- P4AGR[, lapply(.SD, as.numeric)]
#       
#       P4BUS <- data.table(rbind2(sqlQuery(cns,paste0("Select ADDRESS, COL01, COL10 From [U",year,"P4S4]")),
#                                  sqlQuery(cns,paste0("Select ADDRESS, COL01, COL10 From [R",year,"P4S4]"))))
#       setnames(P4BUS,c("HHID","IndivNo","BusinessIncome"))
#       P4BUS <- P4BUS[, lapply(.SD, as.numeric)]
#       
#       P4Other <- data.table(rbind(sqlFetch(cns,paste0("U",year,"P4S5"),stringsAsFactors=FALSE),
#                                   sqlFetch(cns,paste0("R",year,"P4S5"),stringsAsFactors=FALSE)))
#       setnames(P4Other,c("HHID","Code","OtherIncome"))
#       P4Other <- P4Other[, lapply(.SD, as.numeric)]
#       
#       P4Other[is.na(P4Other)] <- 0
#       P4Other[,IndivNo:=1]
#       P4Other <- P4Other[,list(OtherIncome=sum(OtherIncome)),by=list(HHID,IndivNo)]    
#     }  
#     }else{
#     
#     if(year %in% 69:73){
#       PubPrvCol <- "COL04"
#       AgrBusCol <- "COL05"
#       WageCol <- "COL12"
#       BussCol <- "COL12"
#     }else if(year %in% 74:83){
#       PubPrvCol <- "COL05"
#       AgrBusCol <- "COL06"
#       WageCol <- "COL13"
#       BussCol <- "COL13"
#     }else{
#       PubPrvCol <- "DYCOL05"
#       AgrBusCol <- "DYCOL06"
#       WageCol <- "DYCOL15"
#       BussCol <- "DYCOL15"
#     }
#     
#     IDCol <- ifelse(year %in% c(69:83,85),"ADDRESS","Address")
#     IndivNoCol <- ifelse(year %in% 69:83,"COL01","DYCOL01")
#     
#     tbl <- ifelse(year<=83,"P4S1","P4S01")
#     Tb <- rbind(Tables[[paste0("U",year,tbl)]],
#                 Tables[[paste0("R",year,tbl)]])
#     Tb <- Tb[,c(IDCol, IndivNoCol, PubPrvCol,WageCol),with=FALSE]
#     setnames(Tb,c("HHID","IndivNo","PubPrv","Wage"))
#     Tb <- Tb[, lapply(.SD, as.numeric)]
#     P4PBW <- Tb[PubPrv==1,-3,with=FALSE]
#     setnames(P4PBW,c("HHID","IndivNo","PubWageIncome"))
#     P4PVW <- Tb[PubPrv==2,-3,with=FALSE]
#     setnames(P4PVW,c("HHID","IndivNo","PrvWageIncome"))
#     
#     tbl <- ifelse(year<=83,"P4S2","P4S02")
#     Tb <- rbind(Tables[[paste0("U",year,tbl)]],
#                 Tables[[paste0("R",year,tbl)]])
#     Tb <- Tb[,c(IDCol, IndivNoCol, AgrBusCol,BussCol),with=FALSE]
#     setnames(Tb,c("HHID","IndivNo","AgrBus","Wage"))
#     Tb <- Tb[, lapply(.SD, as.numeric)]
#     P4AGR <- Tb[AgrBus==1,-3,with=FALSE]
#     setnames(P4AGR,c("HHID","IndivNo","AggriIncome"))
#     P4BUS <- Tb[AgrBus==2,-3,with=FALSE]
#     setnames(P4BUS,c("HHID","IndivNo","BusinessIncome"))  
#     
#     
#     tbl <- ifelse(year<=83,"P4S3","P4S03")
#     P4Other <- rbind(Tables[[paste0("U",year,tbl)]],
#                      Tables[[paste0("R",year,tbl)]])
#     
#     P4Other <- P4Other[, lapply(.SD, as.numeric)]
#     setnames(P4Other,1:2,c("HHID","IndivNo"))
#     P4Other[is.na(P4Other)] <- 0
#     P4Other$OtherIncome <- rowSums(P4Other[,-1:-2,with=FALSE],na.rm = TRUE)
#     P4Other <- P4Other[,c("HHID","IndivNo","OtherIncome"),with=FALSE]
#     P4Other <- P4Other[,list(OtherIncome=sum(OtherIncome)),by=list(HHID,IndivNo)]
#     
#     rm(Tb,tbl,IndivNoCol,AgrBusCol,BussCol,PubPrvCol,WageCol)
#   }
#   
#   P4PBW <- P4PBW[!is.na(IndivNo)]
#   P4PVW <- P4PVW[!is.na(IndivNo)]
#   P4AGR <- P4AGR[!is.na(IndivNo)]
#   P4BUS <- P4BUS[!is.na(IndivNo)]
#   
#   P4PBW <- P4PBW[PubWageIncome != 0]
#   P4PVW <- P4PVW[PrvWageIncome != 0]
#   P4AGR <- P4AGR[AggriIncome != 0]
#   P4BUS <- P4BUS[BusinessIncome != 0]
#   
# #===end income part =========
  
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
  # P4PBW <- P4PBW[,list(PubWageIncome=sum(PubWageIncome)),by=list(HHID,IndivNo)]
  # P4PVW <- P4PVW[,list(PrvWageIncome=sum(PrvWageIncome)),by=list(HHID,IndivNo)]
  # P4AGR <- P4AGR[,list(AggriIncome=sum(AggriIncome)),by=list(HHID,IndivNo)]
  # P4BUS <- P4BUS[,list(BusinessIncome=sum(BusinessIncome)),by=list(HHID,IndivNo)]
  # 
  # setkey(P4PBW,HHID,IndivNo)
  # setkey(P4PVW,HHID,IndivNo)
  # setkey(P4AGR,HHID,IndivNo)
  # setkey(P4BUS,HHID,IndivNo)
  # setkey(P4Other,HHID,IndivNo)
  setkey(P1,HHID,IndivNo)
  
  # P4 <- merge(P1[,c("HHID","IndivNo"),with=FALSE],P4PBW,all.x=TRUE)
  # P4 <- merge(P4,P4PVW,all.x=TRUE)
  # P4 <- merge(P4,P4AGR,all.x=TRUE)
  # P4 <- merge(P4,P4BUS,all.x=TRUE)
  # P4 <- merge(P4,P4Other,all.x=TRUE)
  
  # P <- merge(unique(P1),unique(P4),all.x=TRUE)
  # 
  # P[is.na(PubWageIncome),PubWageIncome:=0]
  # P[is.na(PrvWageIncome),PrvWageIncome:=0]
  # P[is.na(AggriIncome),AggriIncome:=0]
  # P[is.na(BusinessIncome),BusinessIncome:=0]
  # P[is.na(OtherIncome),OtherIncome:=0]
  # 
  # P <- P[!is.na(P$Age),]
  # 
  # rm(P1,P4PBW,P4PVW,P4AGR,P4BUS,P4,P4Other)
  # 
  # P[,TotalIncome:=PubWageIncome+PrvWageIncome+AggriIncome+BusinessIncome+OtherIncome]
  
  P <- copy(P1)
  
  P <- P[order(P$HHID),]
  
  P[,Size:=1]
  P[,NKids:=ifelse(Age<=17,1,0)]
  
  PSum <- P[,lapply(.SD,sum,na.rm=TRUE),
            .SDcols=c("Size","NKids"),#,"TotalIncome"),
            by="HHID"]
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
  
#  PSum <- PSum[TotalIncome>0]
  
  HHI <- merge(B,PSum,by="HHID")
  HHI[,EqSizeRevOECD := 1 + (Size-NKids-1)*0.5 + NKids*0.3]
  HHI <- HHI[!is.na(HLiterate)]

  rm(P,B,PSum)
  save(HHI,year,file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  
  
  rm(HHI)
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
