# 113-Demo2.R
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
library(Hmisc)
library(haven)
library(sm)
library(gridExtra)
#P1<-P1[,`:=`(Dimension=.N),by=.(HHID)]

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))

EduCodesA <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_A))
EduCodesB <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_B))
EduCodesC <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_C))
EduCodesD <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_EC_D))

years <- Settings$startyear:Settings$endyear

for(year in years){
  #cat(paste0("\n------------------------------\nYear:",year,"\n"))
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
  
  
  B <- P[IndivNo==1]
  setnames(B,2:length(B),sapply(X=names(B)[2:length(B)],function(X){paste("H",X,sep="")}))
  B <- B[order(HHID,HIndivNo)]
  B <- B[!duplicated(B$HHID),]
  
  B[,HEmployed:=HActivityState=="Employed"]
  B[,HUnemployed:=HActivityState=="Unemployed"]
  B[,HIncomeWOWork:=HActivityState=="Income without Work"]
  
  B <- B[!is.na(HActivityState) & !is.na(HLiterate)]
  
  
  P[,Size:=1]
  P[,Kids:=ifelse(Relationship=="Child",1,0)]
  P[,NKids:=ifelse(Age<15,1,0)]
  P[,NInfants:=ifelse(Age<=2,1,0)]
  P[,NSmallKids:=ifelse(Age>=3 & Age<=13, 1, 0)]
  
  P[,NElementary:= ifelse(EduLevel0=="Elementary" & Student==TRUE,1,0)]
  P[,NMiddle:= ifelse(EduLevel0=="Middle" & Student==TRUE,1,0)]
  P[,NHigh:= ifelse(EduLevel0=="High" & Student==TRUE,1,0)]
  P[,NPre:= ifelse(EduLevel0=="Pre" & Student==TRUE,1,0)]
  
  #Age Groups 1
  P[,	NAge0B	:=ifelse(	Age==	0	&Sex==	"Male"	,1,0)]
  P[,	NAge1B	:=ifelse(	Age==	1	&Sex==	"Male"	,1,0)]
  P[,	NAge2B	:=ifelse(	Age==	2	&Sex==	"Male"	,1,0)]
  P[,	NAge3B	:=ifelse(	Age==	3	&Sex==	"Male"	,1,0)]
  P[,	NAge4B	:=ifelse(	Age==	4	&Sex==	"Male"	,1,0)]
  P[,	NAge5B	:=ifelse(	Age==	5	&Sex==	"Male"	,1,0)]
  P[,	NAge6B	:=ifelse(	Age==	6	&Sex==	"Male"	,1,0)]
  P[,	NAge7B	:=ifelse(	Age==	7	&Sex==	"Male"	,1,0)]
  P[,	NAge8B	:=ifelse(	Age==	8	&Sex==	"Male"	,1,0)]
  P[,	NAge9B	:=ifelse(	Age==	9	&Sex==	"Male"	,1,0)]
  P[,	NAge10B	:=ifelse(	Age==	10	&Sex==	"Male"	,1,0)]
  P[,	NAge11B	:=ifelse(	Age==	11	&Sex==	"Male"	,1,0)]
  P[,	NAge12B	:=ifelse(	Age==	12	&Sex==	"Male"	,1,0)]
  P[,	NAge13B	:=ifelse(	Age==	13	&Sex==	"Male"	,1,0)]
  P[,	NAge14B	:=ifelse(	Age==	14	&Sex==	"Male"	,1,0)]
  P[,	NAge15B	:=ifelse(	Age==	15	&Sex==	"Male"	,1,0)]
  P[,	NAge16B	:=ifelse(	Age==	16	&Sex==	"Male"	,1,0)]
  P[,	NAge17B	:=ifelse(	Age==	17	&Sex==	"Male"	,1,0)]
  P[,	NAge18B	:=ifelse(	Age==	18	&Sex==	"Male"	,1,0)]
  P[,	NAge19B	:=ifelse(	Age==	19	&Sex==	"Male"	,1,0)]
  P[,	NAge20B	:=ifelse(	Age==	20	&Sex==	"Male"	,1,0)]
  P[,	NAge21B	:=ifelse(	Age==	21	&Sex==	"Male"	,1,0)]
  P[,	NAge22B	:=ifelse(	Age==	22	&Sex==	"Male"	,1,0)]
  P[,	NAge23B	:=ifelse(	Age==	23	&Sex==	"Male"	,1,0)]
  P[,	NAge24B	:=ifelse(	Age==	24	&Sex==	"Male"	,1,0)]
  P[,	NAge25B	:=ifelse(	Age==	25	&Sex==	"Male"	,1,0)]
  P[,	NAge26B	:=ifelse(	Age==	26	&Sex==	"Male"	,1,0)]
  P[,	NAge27B	:=ifelse(	Age==	27	&Sex==	"Male"	,1,0)]
  P[,	NAge28B	:=ifelse(	Age==	28	&Sex==	"Male"	,1,0)]
  P[,	NAge29B	:=ifelse(	Age==	29	&Sex==	"Male"	,1,0)]
  P[,	NAge30B	:=ifelse(	Age==	30	&Sex==	"Male"	,1,0)]
  P[,	NAge31B	:=ifelse(	Age==	31	&Sex==	"Male"	,1,0)]
  P[,	NAge32B	:=ifelse(	Age==	32	&Sex==	"Male"	,1,0)]
  P[,	NAge33B	:=ifelse(	Age==	33	&Sex==	"Male"	,1,0)]
  P[,	NAge34B	:=ifelse(	Age==	34	&Sex==	"Male"	,1,0)]
  P[,	NAge35B	:=ifelse(	Age==	35	&Sex==	"Male"	,1,0)]
  P[,	NAge36B	:=ifelse(	Age==	36	&Sex==	"Male"	,1,0)]
  P[,	NAge37B	:=ifelse(	Age==	37	&Sex==	"Male"	,1,0)]
  P[,	NAge38B	:=ifelse(	Age==	38	&Sex==	"Male"	,1,0)]
  P[,	NAge39B	:=ifelse(	Age==	39	&Sex==	"Male"	,1,0)]
  P[,	NAge40B	:=ifelse(	Age==	40	&Sex==	"Male"	,1,0)]
  P[,	NAge41B	:=ifelse(	Age==	41	&Sex==	"Male"	,1,0)]
  P[,	NAge42B	:=ifelse(	Age==	42	&Sex==	"Male"	,1,0)]
  P[,	NAge43B	:=ifelse(	Age==	43	&Sex==	"Male"	,1,0)]
  P[,	NAge44B	:=ifelse(	Age==	44	&Sex==	"Male"	,1,0)]
  P[,	NAge45B	:=ifelse(	Age==	45	&Sex==	"Male"	,1,0)]
  P[,	NAge46B	:=ifelse(	Age==	46	&Sex==	"Male"	,1,0)]
  P[,	NAge47B	:=ifelse(	Age==	47	&Sex==	"Male"	,1,0)]
  P[,	NAge48B	:=ifelse(	Age==	48	&Sex==	"Male"	,1,0)]
  P[,	NAge49B	:=ifelse(	Age==	49	&Sex==	"Male"	,1,0)]
  P[,	NAge50B	:=ifelse(	Age==	50	&Sex==	"Male"	,1,0)]
  P[,	NAge51B	:=ifelse(	Age==	51	&Sex==	"Male"	,1,0)]
  P[,	NAge52B	:=ifelse(	Age==	52	&Sex==	"Male"	,1,0)]
  P[,	NAge53B	:=ifelse(	Age==	53	&Sex==	"Male"	,1,0)]
  P[,	NAge54B	:=ifelse(	Age==	54	&Sex==	"Male"	,1,0)]
  P[,	NAge55B	:=ifelse(	Age==	55	&Sex==	"Male"	,1,0)]
  P[,	NAge56B	:=ifelse(	Age==	56	&Sex==	"Male"	,1,0)]
  P[,	NAge57B	:=ifelse(	Age==	57	&Sex==	"Male"	,1,0)]
  P[,	NAge58B	:=ifelse(	Age==	58	&Sex==	"Male"	,1,0)]
  P[,	NAge59B	:=ifelse(	Age==	59	&Sex==	"Male"	,1,0)]
  P[,	NAge60B	:=ifelse(	Age==	60	&Sex==	"Male"	,1,0)]
  P[,	NAge61B	:=ifelse(	Age==	61	&Sex==	"Male"	,1,0)]
  P[,	NAge62B	:=ifelse(	Age==	62	&Sex==	"Male"	,1,0)]
  P[,	NAge63B	:=ifelse(	Age==	63	&Sex==	"Male"	,1,0)]
  P[,	NAge64B	:=ifelse(	Age==	64	&Sex==	"Male"	,1,0)]
  P[,	NAge65B	:=ifelse(	Age==	65	&Sex==	"Male"	,1,0)]
  P[,	NAge66B	:=ifelse(	Age==	66	&Sex==	"Male"	,1,0)]
  P[,	NAge67B	:=ifelse(	Age==	67	&Sex==	"Male"	,1,0)]
  P[,	NAge68B	:=ifelse(	Age==	68	&Sex==	"Male"	,1,0)]
  P[,	NAge69B	:=ifelse(	Age==	69	&Sex==	"Male"	,1,0)]
  P[,	NAge70B	:=ifelse(	Age==	70	&Sex==	"Male"	,1,0)]
  P[,	NAge71B	:=ifelse(	Age==	71	&Sex==	"Male"	,1,0)]
  P[,	NAge72B	:=ifelse(	Age==	72	&Sex==	"Male"	,1,0)]
  P[,	NAge73B	:=ifelse(	Age==	73	&Sex==	"Male"	,1,0)]
  P[,	NAge74B	:=ifelse(	Age==	74	&Sex==	"Male"	,1,0)]
  P[,	NAge75B	:=ifelse(	Age==	75	&Sex==	"Male"	,1,0)]
  P[,	NAge76B	:=ifelse(	Age==	76	&Sex==	"Male"	,1,0)]
  P[,	NAge77B	:=ifelse(	Age==	77	&Sex==	"Male"	,1,0)]
  P[,	NAge78B	:=ifelse(	Age==	78	&Sex==	"Male"	,1,0)]
  P[,	NAge79B	:=ifelse(	Age==	79	&Sex==	"Male"	,1,0)]
  P[,	NAge80B	:=ifelse(	Age==	80	&Sex==	"Male"	,1,0)]
  P[,	NAge81B	:=ifelse(	Age==	81	&Sex==	"Male"	,1,0)]
  P[,	NAge82B	:=ifelse(	Age==	82	&Sex==	"Male"	,1,0)]
  P[,	NAge83B	:=ifelse(	Age==	83	&Sex==	"Male"	,1,0)]
  P[,	NAge84B	:=ifelse(	Age==	84	&Sex==	"Male"	,1,0)]
  P[,	NAge85B	:=ifelse(	Age==	85	&Sex==	"Male"	,1,0)]
  P[,	NAge86B	:=ifelse(	Age==	86	&Sex==	"Male"	,1,0)]
  P[,	NAge87B	:=ifelse(	Age==	87	&Sex==	"Male"	,1,0)]
  P[,	NAge88B	:=ifelse(	Age==	88	&Sex==	"Male"	,1,0)]
  P[,	NAge89B	:=ifelse(	Age==	89	&Sex==	"Male"	,1,0)]
  P[,	NAge90B	:=ifelse(	Age==	90	&Sex==	"Male"	,1,0)]
  P[,	NAge91B	:=ifelse(	Age==	91	&Sex==	"Male"	,1,0)]
  P[,	NAge92B	:=ifelse(	Age==	92	&Sex==	"Male"	,1,0)]
  P[,	NAge93B	:=ifelse(	Age==	93	&Sex==	"Male"	,1,0)]
  P[,	NAge94B	:=ifelse(	Age==	94	&Sex==	"Male"	,1,0)]
  P[,	NAge95B	:=ifelse(	Age==	95	&Sex==	"Male"	,1,0)]
  P[,	NAge96B	:=ifelse(	Age==	96	&Sex==	"Male"	,1,0)]
  P[,	NAge97B	:=ifelse(	Age==	97	&Sex==	"Male"	,1,0)]
  P[,	NAge98B	:=ifelse(	Age==	98	&Sex==	"Male"	,1,0)]
  P[,	NAge99B	:=ifelse(	Age==	99	&Sex==	"Male"	,1,0)]
  P[,	NAge0G	:=ifelse(	Age==	0	&Sex==	"Female"	,1,0)]
  P[,	NAge1G	:=ifelse(	Age==	1	&Sex==	"Female"	,1,0)]
  P[,	NAge2G	:=ifelse(	Age==	2	&Sex==	"Female"	,1,0)]
  P[,	NAge3G	:=ifelse(	Age==	3	&Sex==	"Female"	,1,0)]
  P[,	NAge4G	:=ifelse(	Age==	4	&Sex==	"Female"	,1,0)]
  P[,	NAge5G	:=ifelse(	Age==	5	&Sex==	"Female"	,1,0)]
  P[,	NAge6G	:=ifelse(	Age==	6	&Sex==	"Female"	,1,0)]
  P[,	NAge7G	:=ifelse(	Age==	7	&Sex==	"Female"	,1,0)]
  P[,	NAge8G	:=ifelse(	Age==	8	&Sex==	"Female"	,1,0)]
  P[,	NAge9G	:=ifelse(	Age==	9	&Sex==	"Female"	,1,0)]
  P[,	NAge10G	:=ifelse(	Age==	10	&Sex==	"Female"	,1,0)]
  P[,	NAge11G	:=ifelse(	Age==	11	&Sex==	"Female"	,1,0)]
  P[,	NAge12G	:=ifelse(	Age==	12	&Sex==	"Female"	,1,0)]
  P[,	NAge13G	:=ifelse(	Age==	13	&Sex==	"Female"	,1,0)]
  P[,	NAge14G	:=ifelse(	Age==	14	&Sex==	"Female"	,1,0)]
  P[,	NAge15G	:=ifelse(	Age==	15	&Sex==	"Female"	,1,0)]
  P[,	NAge16G	:=ifelse(	Age==	16	&Sex==	"Female"	,1,0)]
  P[,	NAge17G	:=ifelse(	Age==	17	&Sex==	"Female"	,1,0)]
  P[,	NAge18G	:=ifelse(	Age==	18	&Sex==	"Female"	,1,0)]
  P[,	NAge19G	:=ifelse(	Age==	19	&Sex==	"Female"	,1,0)]
  P[,	NAge20G	:=ifelse(	Age==	20	&Sex==	"Female"	,1,0)]
  P[,	NAge21G	:=ifelse(	Age==	21	&Sex==	"Female"	,1,0)]
  P[,	NAge22G	:=ifelse(	Age==	22	&Sex==	"Female"	,1,0)]
  P[,	NAge23G	:=ifelse(	Age==	23	&Sex==	"Female"	,1,0)]
  P[,	NAge24G	:=ifelse(	Age==	24	&Sex==	"Female"	,1,0)]
  P[,	NAge25G	:=ifelse(	Age==	25	&Sex==	"Female"	,1,0)]
  P[,	NAge26G	:=ifelse(	Age==	26	&Sex==	"Female"	,1,0)]
  P[,	NAge27G	:=ifelse(	Age==	27	&Sex==	"Female"	,1,0)]
  P[,	NAge28G	:=ifelse(	Age==	28	&Sex==	"Female"	,1,0)]
  P[,	NAge29G	:=ifelse(	Age==	29	&Sex==	"Female"	,1,0)]
  P[,	NAge30G	:=ifelse(	Age==	30	&Sex==	"Female"	,1,0)]
  P[,	NAge31G	:=ifelse(	Age==	31	&Sex==	"Female"	,1,0)]
  P[,	NAge32G	:=ifelse(	Age==	32	&Sex==	"Female"	,1,0)]
  P[,	NAge33G	:=ifelse(	Age==	33	&Sex==	"Female"	,1,0)]
  P[,	NAge34G	:=ifelse(	Age==	34	&Sex==	"Female"	,1,0)]
  P[,	NAge35G	:=ifelse(	Age==	35	&Sex==	"Female"	,1,0)]
  P[,	NAge36G	:=ifelse(	Age==	36	&Sex==	"Female"	,1,0)]
  P[,	NAge37G	:=ifelse(	Age==	37	&Sex==	"Female"	,1,0)]
  P[,	NAge38G	:=ifelse(	Age==	38	&Sex==	"Female"	,1,0)]
  P[,	NAge39G	:=ifelse(	Age==	39	&Sex==	"Female"	,1,0)]
  P[,	NAge40G	:=ifelse(	Age==	40	&Sex==	"Female"	,1,0)]
  P[,	NAge41G	:=ifelse(	Age==	41	&Sex==	"Female"	,1,0)]
  P[,	NAge42G	:=ifelse(	Age==	42	&Sex==	"Female"	,1,0)]
  P[,	NAge43G	:=ifelse(	Age==	43	&Sex==	"Female"	,1,0)]
  P[,	NAge44G	:=ifelse(	Age==	44	&Sex==	"Female"	,1,0)]
  P[,	NAge45G	:=ifelse(	Age==	45	&Sex==	"Female"	,1,0)]
  P[,	NAge46G	:=ifelse(	Age==	46	&Sex==	"Female"	,1,0)]
  P[,	NAge47G	:=ifelse(	Age==	47	&Sex==	"Female"	,1,0)]
  P[,	NAge48G	:=ifelse(	Age==	48	&Sex==	"Female"	,1,0)]
  P[,	NAge49G	:=ifelse(	Age==	49	&Sex==	"Female"	,1,0)]
  P[,	NAge50G	:=ifelse(	Age==	50	&Sex==	"Female"	,1,0)]
  P[,	NAge51G	:=ifelse(	Age==	51	&Sex==	"Female"	,1,0)]
  P[,	NAge52G	:=ifelse(	Age==	52	&Sex==	"Female"	,1,0)]
  P[,	NAge53G	:=ifelse(	Age==	53	&Sex==	"Female"	,1,0)]
  P[,	NAge54G	:=ifelse(	Age==	54	&Sex==	"Female"	,1,0)]
  P[,	NAge55G	:=ifelse(	Age==	55	&Sex==	"Female"	,1,0)]
  P[,	NAge56G	:=ifelse(	Age==	56	&Sex==	"Female"	,1,0)]
  P[,	NAge57G	:=ifelse(	Age==	57	&Sex==	"Female"	,1,0)]
  P[,	NAge58G	:=ifelse(	Age==	58	&Sex==	"Female"	,1,0)]
  P[,	NAge59G	:=ifelse(	Age==	59	&Sex==	"Female"	,1,0)]
  P[,	NAge60G	:=ifelse(	Age==	60	&Sex==	"Female"	,1,0)]
  P[,	NAge61G	:=ifelse(	Age==	61	&Sex==	"Female"	,1,0)]
  P[,	NAge62G	:=ifelse(	Age==	62	&Sex==	"Female"	,1,0)]
  P[,	NAge63G	:=ifelse(	Age==	63	&Sex==	"Female"	,1,0)]
  P[,	NAge64G	:=ifelse(	Age==	64	&Sex==	"Female"	,1,0)]
  P[,	NAge65G	:=ifelse(	Age==	65	&Sex==	"Female"	,1,0)]
  P[,	NAge66G	:=ifelse(	Age==	66	&Sex==	"Female"	,1,0)]
  P[,	NAge67G	:=ifelse(	Age==	67	&Sex==	"Female"	,1,0)]
  P[,	NAge68G	:=ifelse(	Age==	68	&Sex==	"Female"	,1,0)]
  P[,	NAge69G	:=ifelse(	Age==	69	&Sex==	"Female"	,1,0)]
  P[,	NAge70G	:=ifelse(	Age==	70	&Sex==	"Female"	,1,0)]
  P[,	NAge71G	:=ifelse(	Age==	71	&Sex==	"Female"	,1,0)]
  P[,	NAge72G	:=ifelse(	Age==	72	&Sex==	"Female"	,1,0)]
  P[,	NAge73G	:=ifelse(	Age==	73	&Sex==	"Female"	,1,0)]
  P[,	NAge74G	:=ifelse(	Age==	74	&Sex==	"Female"	,1,0)]
  P[,	NAge75G	:=ifelse(	Age==	75	&Sex==	"Female"	,1,0)]
  P[,	NAge76G	:=ifelse(	Age==	76	&Sex==	"Female"	,1,0)]
  P[,	NAge77G	:=ifelse(	Age==	77	&Sex==	"Female"	,1,0)]
  P[,	NAge78G	:=ifelse(	Age==	78	&Sex==	"Female"	,1,0)]
  P[,	NAge79G	:=ifelse(	Age==	79	&Sex==	"Female"	,1,0)]
  P[,	NAge80G	:=ifelse(	Age==	80	&Sex==	"Female"	,1,0)]
  P[,	NAge81G	:=ifelse(	Age==	81	&Sex==	"Female"	,1,0)]
  P[,	NAge82G	:=ifelse(	Age==	82	&Sex==	"Female"	,1,0)]
  P[,	NAge83G	:=ifelse(	Age==	83	&Sex==	"Female"	,1,0)]
  P[,	NAge84G	:=ifelse(	Age==	84	&Sex==	"Female"	,1,0)]
  P[,	NAge85G	:=ifelse(	Age==	85	&Sex==	"Female"	,1,0)]
  P[,	NAge86G	:=ifelse(	Age==	86	&Sex==	"Female"	,1,0)]
  P[,	NAge87G	:=ifelse(	Age==	87	&Sex==	"Female"	,1,0)]
  P[,	NAge88G	:=ifelse(	Age==	88	&Sex==	"Female"	,1,0)]
  P[,	NAge89G	:=ifelse(	Age==	89	&Sex==	"Female"	,1,0)]
  P[,	NAge90G	:=ifelse(	Age==	90	&Sex==	"Female"	,1,0)]
  P[,	NAge91G	:=ifelse(	Age==	91	&Sex==	"Female"	,1,0)]
  P[,	NAge92G	:=ifelse(	Age==	92	&Sex==	"Female"	,1,0)]
  P[,	NAge93G	:=ifelse(	Age==	93	&Sex==	"Female"	,1,0)]
  P[,	NAge94G	:=ifelse(	Age==	94	&Sex==	"Female"	,1,0)]
  P[,	NAge95G	:=ifelse(	Age==	95	&Sex==	"Female"	,1,0)]
  P[,	NAge96G	:=ifelse(	Age==	96	&Sex==	"Female"	,1,0)]
  P[,	NAge97G	:=ifelse(	Age==	97	&Sex==	"Female"	,1,0)]
  P[,	NAge98G	:=ifelse(	Age==	98	&Sex==	"Female"	,1,0)]
  P[,	NAge99G	:=ifelse(	Age==	99	&Sex==	"Female"	,1,0)]
  
  
  
  #Age Groups 2
  P[,	NAge0_A_B	:=ifelse(	Age==	0	&Sex==	"Male"	,1,0)]
  P[,	NAge1_A_B	:=ifelse(	Age==	1	&Sex==	"Male"	,1,0)]
  P[,	NAge2_A_B	:=ifelse(	Age==	2	&Sex==	"Male"	,1,0)]
  P[,	NAge3_A_B	:=ifelse(	Age==	3	&Sex==	"Male"	,1,0)]
  P[,	NAge4_A_B	:=ifelse(	Age==	4	&Sex==	"Male"	,1,0)]
  P[,	NAge5_A_B	:=ifelse(	Age==	5	&Sex==	"Male"	,1,0)]
  P[,	NAge6_A_B	:=ifelse(	Age==	6	&Sex==	"Male"	,1,0)]
  P[,	NAge7_A_B	:=ifelse(	Age==	7	&Sex==	"Male"	,1,0)]
  P[,	NAge8_A_B	:=ifelse(	Age==	8	&Sex==	"Male"	,1,0)]
  P[,	NAge9_A_B	:=ifelse(	Age==	9	&Sex==	"Male"	,1,0)]
  P[,	NAge10_A_B	:=ifelse(	Age==	10	&Sex==	"Male"	,1,0)]
  P[,	NAge11_A_B	:=ifelse(	Age==	11	&Sex==	"Male"	,1,0)]
  P[,	NAge12_A_B	:=ifelse(	Age==	12	&Sex==	"Male"	,1,0)]
  P[,	NAge13_A_B	:=ifelse(	Age==	13	&Sex==	"Male"	,1,0)]
  P[,	NAge14_A_B	:=ifelse(	Age==	14	&Sex==	"Male"	,1,0)]
  P[,	NAge15_A_B	:=ifelse(	Age==	15	&Sex==	"Male"	,1,0)]
  P[,	NAge16_A_B	:=ifelse(	Age==	16	&Sex==	"Male"	,1,0)]
  P[,	NAge17_A_B	:=ifelse(	Age==	17	&Sex==	"Male"	,1,0)]
  P[,	NAge18_A_B	:=ifelse(	Age==	18	&Sex==	"Male"	,1,0)]
  P[,	NAge19_A_B	:=ifelse(	Age==	19	&Sex==	"Male"	,1,0)]
  P[,	NAge20_A_B	:=ifelse(	Age==	20	&Sex==	"Male"	,1,0)]
  P[,	NAge21_A_B	:=ifelse(	Age==	21	&Sex==	"Male"	,1,0)]
  P[,	NAge22_A_B	:=ifelse(	Age==	22	&Sex==	"Male"	,1,0)]
  P[,	NAge23_A_B	:=ifelse(	Age==	23	&Sex==	"Male"	,1,0)]
  P[,	NAge24_A_B	:=ifelse(	Age==	24	&Sex==	"Male"	,1,0)]
  P[,	NAge25_A_B	:=ifelse(	Age==	25	&Sex==	"Male"	,1,0)]
  P[,	NAge26_A_B	:=ifelse(	Age==	26	&Sex==	"Male"	,1,0)]
  P[,	NAge27_A_B	:=ifelse(	Age==	27	&Sex==	"Male"	,1,0)]
  P[,	NAge28_A_B	:=ifelse(	Age==	28	&Sex==	"Male"	,1,0)]
  P[,	NAge29_A_B	:=ifelse(	Age==	29	&Sex==	"Male"	,1,0)]
  P[,	NAge30_A_B	:=ifelse(	Age==	30	&Sex==	"Male"	,1,0)]
  P[,	NAge31_A_B	:=ifelse(	Age==	31	&Sex==	"Male"	,1,0)]
  P[,	NAge32_A_B	:=ifelse(	Age==	32	&Sex==	"Male"	,1,0)]
  P[,	NAge33_A_B	:=ifelse(	Age==	33	&Sex==	"Male"	,1,0)]
  P[,	NAge34_A_B	:=ifelse(	Age==	34	&Sex==	"Male"	,1,0)]
  P[,	NAge35_A_B	:=ifelse(	Age==	35	&Sex==	"Male"	,1,0)]
  P[,	NAge36_A_B	:=ifelse(	Age==	36	&Sex==	"Male"	,1,0)]
  P[,	NAge37_A_B	:=ifelse(	Age==	37	&Sex==	"Male"	,1,0)]
  P[,	NAge38_A_B	:=ifelse(	Age==	38	&Sex==	"Male"	,1,0)]
  P[,	NAge39_A_B	:=ifelse(	Age==	39	&Sex==	"Male"	,1,0)]
  P[,	NAge40_A_B	:=ifelse(	Age==	40	&Sex==	"Male"	,1,0)]
  P[,	NAge41_A_B	:=ifelse(	Age==	41	&Sex==	"Male"	,1,0)]
  P[,	NAge42_A_B	:=ifelse(	Age==	42	&Sex==	"Male"	,1,0)]
  P[,	NAge43_A_B	:=ifelse(	Age==	43	&Sex==	"Male"	,1,0)]
  P[,	NAge44_A_B	:=ifelse(	Age==	44	&Sex==	"Male"	,1,0)]
  P[,	NAge45_A_B	:=ifelse(	Age==	45	&Sex==	"Male"	,1,0)]
  P[,	NAge46_A_B	:=ifelse(	Age==	46	&Sex==	"Male"	,1,0)]
  P[,	NAge47_A_B	:=ifelse(	Age==	47	&Sex==	"Male"	,1,0)]
  P[,	NAge48_A_B	:=ifelse(	Age==	48	&Sex==	"Male"	,1,0)]
  P[,	NAge49_A_B	:=ifelse(	Age==	49	&Sex==	"Male"	,1,0)]
  P[,	NAge50_A_B	:=ifelse(	Age==	50	&Sex==	"Male"	,1,0)]
  P[,	NAge51_A_B	:=ifelse(	Age==	51	&Sex==	"Male"	,1,0)]
  P[,	NAge52_A_B	:=ifelse(	Age==	52	&Sex==	"Male"	,1,0)]
  P[,	NAge53_A_B	:=ifelse(	Age==	53	&Sex==	"Male"	,1,0)]
  P[,	NAge54_A_B	:=ifelse(	Age==	54	&Sex==	"Male"	,1,0)]
  P[,	NAge55_A_B	:=ifelse(	Age==	55	&Sex==	"Male"	,1,0)]
  P[,	NAge56_A_B	:=ifelse(	Age==	56	&Sex==	"Male"	,1,0)]
  P[,	NAge57_A_B	:=ifelse(	Age==	57	&Sex==	"Male"	,1,0)]
  P[,	NAge58_A_B	:=ifelse(	Age==	58	&Sex==	"Male"	,1,0)]
  P[,	NAge59_A_B	:=ifelse(	Age==	59	&Sex==	"Male"	,1,0)]
  P[,	NAge60_A_B	:=ifelse(	Age==	60	&Sex==	"Male"	,1,0)]
  P[,	NAge61_A_B	:=ifelse(	Age==	61	&Sex==	"Male"	,1,0)]
  P[,	NAge62_A_B	:=ifelse(	Age==	62	&Sex==	"Male"	,1,0)]
  P[,	NAge63_A_B	:=ifelse(	Age==	63	&Sex==	"Male"	,1,0)]
  P[,	NAge64_A_B	:=ifelse(	Age==	64	&Sex==	"Male"	,1,0)]
  P[,	NAge65_A_B	:=ifelse(	Age==	65	&Sex==	"Male"	,1,0)]
  P[,	NAge66_A_B	:=ifelse(	Age==	66	&Sex==	"Male"	,1,0)]
  P[,	NAge67_A_B	:=ifelse(	Age==	67	&Sex==	"Male"	,1,0)]
  P[,	NAge68_A_B	:=ifelse(	Age==	68	&Sex==	"Male"	,1,0)]
  P[,	NAge69_A_B	:=ifelse(	Age==	69	&Sex==	"Male"	,1,0)]
  P[,	NAge70_A_B	:=ifelse(	Age==	70	&Sex==	"Male"	,1,0)]
  P[,	NAge71_A_B	:=ifelse(	Age==	71	&Sex==	"Male"	,1,0)]
  P[,	NAge72_A_B	:=ifelse(	Age==	72	&Sex==	"Male"	,1,0)]
  P[,	NAge73_A_B	:=ifelse(	Age==	73	&Sex==	"Male"	,1,0)]
  P[,	NAge74_A_B	:=ifelse(	Age==	74	&Sex==	"Male"	,1,0)]
  P[,	NAge75_A_B	:=ifelse(	Age==	75	&Sex==	"Male"	,1,0)]
  P[,	NAge76_A_B	:=ifelse(	Age==	76	&Sex==	"Male"	,1,0)]
  P[,	NAge77_A_B	:=ifelse(	Age==	77	&Sex==	"Male"	,1,0)]
  P[,	NAge78_A_B	:=ifelse(	Age==	78	&Sex==	"Male"	,1,0)]
  P[,	NAge79_A_B	:=ifelse(	Age==	79	&Sex==	"Male"	,1,0)]
  P[,	NAge80_A_B	:=ifelse(	Age==	80	&Sex==	"Male"	,1,0)]
  P[,	NAge81_A_B	:=ifelse(	Age==	81	&Sex==	"Male"	,1,0)]
  P[,	NAge82_A_B	:=ifelse(	Age==	82	&Sex==	"Male"	,1,0)]
  P[,	NAge83_A_B	:=ifelse(	Age==	83	&Sex==	"Male"	,1,0)]
  P[,	NAge84_A_B	:=ifelse(	Age==	84	&Sex==	"Male"	,1,0)]
  P[,	NAge85_A_B	:=ifelse(	Age==	85	&Sex==	"Male"	,1,0)]
  P[,	NAge86_A_B	:=ifelse(	Age==	86	&Sex==	"Male"	,1,0)]
  P[,	NAge87_A_B	:=ifelse(	Age==	87	&Sex==	"Male"	,1,0)]
  P[,	NAge88_A_B	:=ifelse(	Age==	88	&Sex==	"Male"	,1,0)]
  P[,	NAge89_A_B	:=ifelse(	Age==	89	&Sex==	"Male"	,1,0)]
  P[,	NAge90_A_B	:=ifelse(	Age==	90	&Sex==	"Male"	,1,0)]
  P[,	NAge91_A_B	:=ifelse(	Age==	91	&Sex==	"Male"	,1,0)]
  P[,	NAge92_A_B	:=ifelse(	Age==	92	&Sex==	"Male"	,1,0)]
  P[,	NAge93_A_B	:=ifelse(	Age==	93	&Sex==	"Male"	,1,0)]
  P[,	NAge94_A_B	:=ifelse(	Age==	94	&Sex==	"Male"	,1,0)]
  P[,	NAge95_A_B	:=ifelse(	Age==	95	&Sex==	"Male"	,1,0)]
  P[,	NAge96_A_B	:=ifelse(	Age==	96	&Sex==	"Male"	,1,0)]
  P[,	NAge97_A_B	:=ifelse(	Age==	97	&Sex==	"Male"	,1,0)]
  P[,	NAge98_A_B	:=ifelse(	Age==	98	&Sex==	"Male"	,1,0)]
  P[,	NAge99_A_B	:=ifelse(	Age==	99	&Sex==	"Male"	,1,0)]
  P[,	NAge0_A_G	:=ifelse(	Age==	0	&Sex==	"Female"	,1,0)]
  P[,	NAge1_A_G	:=ifelse(	Age==	1	&Sex==	"Female"	,1,0)]
  P[,	NAge2_A_G	:=ifelse(	Age==	2	&Sex==	"Female"	,1,0)]
  P[,	NAge3_A_G	:=ifelse(	Age==	3	&Sex==	"Female"	,1,0)]
  P[,	NAge4_A_G	:=ifelse(	Age==	4	&Sex==	"Female"	,1,0)]
  P[,	NAge5_A_G	:=ifelse(	Age==	5	&Sex==	"Female"	,1,0)]
  P[,	NAge6_A_G	:=ifelse(	Age==	6	&Sex==	"Female"	,1,0)]
  P[,	NAge7_A_G	:=ifelse(	Age==	7	&Sex==	"Female"	,1,0)]
  P[,	NAge8_A_G	:=ifelse(	Age==	8	&Sex==	"Female"	,1,0)]
  P[,	NAge9_A_G	:=ifelse(	Age==	9	&Sex==	"Female"	,1,0)]
  P[,	NAge10_A_G	:=ifelse(	Age==	10	&Sex==	"Female"	,1,0)]
  P[,	NAge11_A_G	:=ifelse(	Age==	11	&Sex==	"Female"	,1,0)]
  P[,	NAge12_A_G	:=ifelse(	Age==	12	&Sex==	"Female"	,1,0)]
  P[,	NAge13_A_G	:=ifelse(	Age==	13	&Sex==	"Female"	,1,0)]
  P[,	NAge14_A_G	:=ifelse(	Age==	14	&Sex==	"Female"	,1,0)]
  P[,	NAge15_A_G	:=ifelse(	Age==	15	&Sex==	"Female"	,1,0)]
  P[,	NAge16_A_G	:=ifelse(	Age==	16	&Sex==	"Female"	,1,0)]
  P[,	NAge17_A_G	:=ifelse(	Age==	17	&Sex==	"Female"	,1,0)]
  P[,	NAge18_A_G	:=ifelse(	Age==	18	&Sex==	"Female"	,1,0)]
  P[,	NAge19_A_G	:=ifelse(	Age==	19	&Sex==	"Female"	,1,0)]
  P[,	NAge20_A_G	:=ifelse(	Age==	20	&Sex==	"Female"	,1,0)]
  P[,	NAge21_A_G	:=ifelse(	Age==	21	&Sex==	"Female"	,1,0)]
  P[,	NAge22_A_G	:=ifelse(	Age==	22	&Sex==	"Female"	,1,0)]
  P[,	NAge23_A_G	:=ifelse(	Age==	23	&Sex==	"Female"	,1,0)]
  P[,	NAge24_A_G	:=ifelse(	Age==	24	&Sex==	"Female"	,1,0)]
  P[,	NAge25_A_G	:=ifelse(	Age==	25	&Sex==	"Female"	,1,0)]
  P[,	NAge26_A_G	:=ifelse(	Age==	26	&Sex==	"Female"	,1,0)]
  P[,	NAge27_A_G	:=ifelse(	Age==	27	&Sex==	"Female"	,1,0)]
  P[,	NAge28_A_G	:=ifelse(	Age==	28	&Sex==	"Female"	,1,0)]
  P[,	NAge29_A_G	:=ifelse(	Age==	29	&Sex==	"Female"	,1,0)]
  P[,	NAge30_A_G	:=ifelse(	Age==	30	&Sex==	"Female"	,1,0)]
  P[,	NAge31_A_G	:=ifelse(	Age==	31	&Sex==	"Female"	,1,0)]
  P[,	NAge32_A_G	:=ifelse(	Age==	32	&Sex==	"Female"	,1,0)]
  P[,	NAge33_A_G	:=ifelse(	Age==	33	&Sex==	"Female"	,1,0)]
  P[,	NAge34_A_G	:=ifelse(	Age==	34	&Sex==	"Female"	,1,0)]
  P[,	NAge35_A_G	:=ifelse(	Age==	35	&Sex==	"Female"	,1,0)]
  P[,	NAge36_A_G	:=ifelse(	Age==	36	&Sex==	"Female"	,1,0)]
  P[,	NAge37_A_G	:=ifelse(	Age==	37	&Sex==	"Female"	,1,0)]
  P[,	NAge38_A_G	:=ifelse(	Age==	38	&Sex==	"Female"	,1,0)]
  P[,	NAge39_A_G	:=ifelse(	Age==	39	&Sex==	"Female"	,1,0)]
  P[,	NAge40_A_G	:=ifelse(	Age==	40	&Sex==	"Female"	,1,0)]
  P[,	NAge41_A_G	:=ifelse(	Age==	41	&Sex==	"Female"	,1,0)]
  P[,	NAge42_A_G	:=ifelse(	Age==	42	&Sex==	"Female"	,1,0)]
  P[,	NAge43_A_G	:=ifelse(	Age==	43	&Sex==	"Female"	,1,0)]
  P[,	NAge44_A_G	:=ifelse(	Age==	44	&Sex==	"Female"	,1,0)]
  P[,	NAge45_A_G	:=ifelse(	Age==	45	&Sex==	"Female"	,1,0)]
  P[,	NAge46_A_G	:=ifelse(	Age==	46	&Sex==	"Female"	,1,0)]
  P[,	NAge47_A_G	:=ifelse(	Age==	47	&Sex==	"Female"	,1,0)]
  P[,	NAge48_A_G	:=ifelse(	Age==	48	&Sex==	"Female"	,1,0)]
  P[,	NAge49_A_G	:=ifelse(	Age==	49	&Sex==	"Female"	,1,0)]
  P[,	NAge50_A_G	:=ifelse(	Age==	50	&Sex==	"Female"	,1,0)]
  P[,	NAge51_A_G	:=ifelse(	Age==	51	&Sex==	"Female"	,1,0)]
  P[,	NAge52_A_G	:=ifelse(	Age==	52	&Sex==	"Female"	,1,0)]
  P[,	NAge53_A_G	:=ifelse(	Age==	53	&Sex==	"Female"	,1,0)]
  P[,	NAge54_A_G	:=ifelse(	Age==	54	&Sex==	"Female"	,1,0)]
  P[,	NAge55_A_G	:=ifelse(	Age==	55	&Sex==	"Female"	,1,0)]
  P[,	NAge56_A_G	:=ifelse(	Age==	56	&Sex==	"Female"	,1,0)]
  P[,	NAge57_A_G	:=ifelse(	Age==	57	&Sex==	"Female"	,1,0)]
  P[,	NAge58_A_G	:=ifelse(	Age==	58	&Sex==	"Female"	,1,0)]
  P[,	NAge59_A_G	:=ifelse(	Age==	59	&Sex==	"Female"	,1,0)]
  P[,	NAge60_A_G	:=ifelse(	Age==	60	&Sex==	"Female"	,1,0)]
  P[,	NAge61_A_G	:=ifelse(	Age==	61	&Sex==	"Female"	,1,0)]
  P[,	NAge62_A_G	:=ifelse(	Age==	62	&Sex==	"Female"	,1,0)]
  P[,	NAge63_A_G	:=ifelse(	Age==	63	&Sex==	"Female"	,1,0)]
  P[,	NAge64_A_G	:=ifelse(	Age==	64	&Sex==	"Female"	,1,0)]
  P[,	NAge65_A_G	:=ifelse(	Age==	65	&Sex==	"Female"	,1,0)]
  P[,	NAge66_A_G	:=ifelse(	Age==	66	&Sex==	"Female"	,1,0)]
  P[,	NAge67_A_G	:=ifelse(	Age==	67	&Sex==	"Female"	,1,0)]
  P[,	NAge68_A_G	:=ifelse(	Age==	68	&Sex==	"Female"	,1,0)]
  P[,	NAge69_A_G	:=ifelse(	Age==	69	&Sex==	"Female"	,1,0)]
  P[,	NAge70_A_G	:=ifelse(	Age==	70	&Sex==	"Female"	,1,0)]
  P[,	NAge71_A_G	:=ifelse(	Age==	71	&Sex==	"Female"	,1,0)]
  P[,	NAge72_A_G	:=ifelse(	Age==	72	&Sex==	"Female"	,1,0)]
  P[,	NAge73_A_G	:=ifelse(	Age==	73	&Sex==	"Female"	,1,0)]
  P[,	NAge74_A_G	:=ifelse(	Age==	74	&Sex==	"Female"	,1,0)]
  P[,	NAge75_A_G	:=ifelse(	Age==	75	&Sex==	"Female"	,1,0)]
  P[,	NAge76_A_G	:=ifelse(	Age==	76	&Sex==	"Female"	,1,0)]
  P[,	NAge77_A_G	:=ifelse(	Age==	77	&Sex==	"Female"	,1,0)]
  P[,	NAge78_A_G	:=ifelse(	Age==	78	&Sex==	"Female"	,1,0)]
  P[,	NAge79_A_G	:=ifelse(	Age==	79	&Sex==	"Female"	,1,0)]
  P[,	NAge80_A_G	:=ifelse(	Age==	80	&Sex==	"Female"	,1,0)]
  P[,	NAge81_A_G	:=ifelse(	Age==	81	&Sex==	"Female"	,1,0)]
  P[,	NAge82_A_G	:=ifelse(	Age==	82	&Sex==	"Female"	,1,0)]
  P[,	NAge83_A_G	:=ifelse(	Age==	83	&Sex==	"Female"	,1,0)]
  P[,	NAge84_A_G	:=ifelse(	Age==	84	&Sex==	"Female"	,1,0)]
  P[,	NAge85_A_G	:=ifelse(	Age==	85	&Sex==	"Female"	,1,0)]
  P[,	NAge86_A_G	:=ifelse(	Age==	86	&Sex==	"Female"	,1,0)]
  P[,	NAge87_A_G	:=ifelse(	Age==	87	&Sex==	"Female"	,1,0)]
  P[,	NAge88_A_G	:=ifelse(	Age==	88	&Sex==	"Female"	,1,0)]
  P[,	NAge89_A_G	:=ifelse(	Age==	89	&Sex==	"Female"	,1,0)]
  P[,	NAge90_A_G	:=ifelse(	Age==	90	&Sex==	"Female"	,1,0)]
  P[,	NAge91_A_G	:=ifelse(	Age==	91	&Sex==	"Female"	,1,0)]
  P[,	NAge92_A_G	:=ifelse(	Age==	92	&Sex==	"Female"	,1,0)]
  P[,	NAge93_A_G	:=ifelse(	Age==	93	&Sex==	"Female"	,1,0)]
  P[,	NAge94_A_G	:=ifelse(	Age==	94	&Sex==	"Female"	,1,0)]
  P[,	NAge95_A_G	:=ifelse(	Age==	95	&Sex==	"Female"	,1,0)]
  P[,	NAge96_A_G	:=ifelse(	Age==	96	&Sex==	"Female"	,1,0)]
  P[,	NAge97_A_G	:=ifelse(	Age==	97	&Sex==	"Female"	,1,0)]
  P[,	NAge98_A_G	:=ifelse(	Age==	98	&Sex==	"Female"	,1,0)]
  P[,	NAge99_A_G	:=ifelse(	Age==	99	&Sex==	"Female"	,1,0)]
  
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
                      "NMiddle","NHigh","NPre","NAge0B",
                      "NAge1B",
                      "NAge2B",
                      "NAge3B",
                      "NAge4B",
                      "NAge5B",
                      "NAge6B",
                      "NAge7B",
                      "NAge8B",
                      "NAge9B",
                      "NAge10B",
                      "NAge11B",
                      "NAge12B",
                      "NAge13B",
                      "NAge14B",
                      "NAge15B",
                      "NAge16B",
                      "NAge17B",
                      "NAge18B",
                      "NAge19B",
                      "NAge20B",
                      "NAge21B",
                      "NAge22B",
                      "NAge23B",
                      "NAge24B",
                      "NAge25B",
                      "NAge26B",
                      "NAge27B",
                      "NAge28B",
                      "NAge29B",
                      "NAge30B",
                      "NAge31B",
                      "NAge32B",
                      "NAge33B",
                      "NAge34B",
                      "NAge35B",
                      "NAge36B",
                      "NAge37B",
                      "NAge38B",
                      "NAge39B",
                      "NAge40B",
                      "NAge41B",
                      "NAge42B",
                      "NAge43B",
                      "NAge44B",
                      "NAge45B",
                      "NAge46B",
                      "NAge47B",
                      "NAge48B",
                      "NAge49B",
                      "NAge50B",
                      "NAge51B",
                      "NAge52B",
                      "NAge53B",
                      "NAge54B",
                      "NAge55B",
                      "NAge56B",
                      "NAge57B",
                      "NAge58B",
                      "NAge59B",
                      "NAge60B",
                      "NAge61B",
                      "NAge62B",
                      "NAge63B",
                      "NAge64B",
                      "NAge65B",
                      "NAge66B",
                      "NAge67B",
                      "NAge68B",
                      "NAge69B",
                      "NAge70B",
                      "NAge71B",
                      "NAge72B",
                      "NAge73B",
                      "NAge74B",
                      "NAge75B",
                      "NAge76B",
                      "NAge77B",
                      "NAge78B",
                      "NAge79B",
                      "NAge80B",
                      "NAge81B",
                      "NAge82B",
                      "NAge83B",
                      "NAge84B",
                      "NAge85B",
                      "NAge86B",
                      "NAge87B",
                      "NAge88B",
                      "NAge89B",
                      "NAge90B",
                      "NAge91B",
                      "NAge92B",
                      "NAge93B",
                      "NAge94B",
                      "NAge95B",
                      "NAge96B",
                      "NAge97B",
                      "NAge98B",
                      "NAge99B",
                      "NAge0G",
                      "NAge1G",
                      "NAge2G",
                      "NAge3G",
                      "NAge4G",
                      "NAge5G",
                      "NAge6G",
                      "NAge7G",
                      "NAge8G",
                      "NAge9G",
                      "NAge10G",
                      "NAge11G",
                      "NAge12G",
                      "NAge13G",
                      "NAge14G",
                      "NAge15G",
                      "NAge16G",
                      "NAge17G",
                      "NAge18G",
                      "NAge19G",
                      "NAge20G",
                      "NAge21G",
                      "NAge22G",
                      "NAge23G",
                      "NAge24G",
                      "NAge25G",
                      "NAge26G",
                      "NAge27G",
                      "NAge28G",
                      "NAge29G",
                      "NAge30G",
                      "NAge31G",
                      "NAge32G",
                      "NAge33G",
                      "NAge34G",
                      "NAge35G",
                      "NAge36G",
                      "NAge37G",
                      "NAge38G",
                      "NAge39G",
                      "NAge40G",
                      "NAge41G",
                      "NAge42G",
                      "NAge43G",
                      "NAge44G",
                      "NAge45G",
                      "NAge46G",
                      "NAge47G",
                      "NAge48G",
                      "NAge49G",
                      "NAge50G",
                      "NAge51G",
                      "NAge52G",
                      "NAge53G",
                      "NAge54G",
                      "NAge55G",
                      "NAge56G",
                      "NAge57G",
                      "NAge58G",
                      "NAge59G",
                      "NAge60G",
                      "NAge61G",
                      "NAge62G",
                      "NAge63G",
                      "NAge64G",
                      "NAge65G",
                      "NAge66G",
                      "NAge67G",
                      "NAge68G",
                      "NAge69G",
                      "NAge70G",
                      "NAge71G",
                      "NAge72G",
                      "NAge73G",
                      "NAge74G",
                      "NAge75G",
                      "NAge76G",
                      "NAge77G",
                      "NAge78G",
                      "NAge79G",
                      "NAge80G",
                      "NAge81G",
                      "NAge82G",
                      "NAge83G",
                      "NAge84G",
                      "NAge85G",
                      "NAge86G",
                      "NAge87G",
                      "NAge88G",
                      "NAge89G",
                      "NAge90G",
                      "NAge91G",
                      "NAge92G",
                      "NAge93G",
                      "NAge94G",
                      "NAge95G",
                      "NAge96G",
                      "NAge97G",
                      "NAge98G",
                      "NAge99G",
                      "NAge0_A_B",
                      "NAge1_A_B",
                      "NAge2_A_B",
                      "NAge3_A_B",
                      "NAge4_A_B",
                      "NAge5_A_B",
                      "NAge6_A_B",
                      "NAge7_A_B",
                      "NAge8_A_B",
                      "NAge9_A_B",
                      "NAge10_A_B",
                      "NAge11_A_B",
                      "NAge12_A_B",
                      "NAge13_A_B",
                      "NAge14_A_B",
                      "NAge15_A_B",
                      "NAge16_A_B",
                      "NAge17_A_B",
                      "NAge18_A_B",
                      "NAge19_A_B",
                      "NAge20_A_B",
                      "NAge21_A_B",
                      "NAge22_A_B",
                      "NAge23_A_B",
                      "NAge24_A_B",
                      "NAge25_A_B",
                      "NAge26_A_B",
                      "NAge27_A_B",
                      "NAge28_A_B",
                      "NAge29_A_B",
                      "NAge30_A_B",
                      "NAge31_A_B",
                      "NAge32_A_B",
                      "NAge33_A_B",
                      "NAge34_A_B",
                      "NAge35_A_B",
                      "NAge36_A_B",
                      "NAge37_A_B",
                      "NAge38_A_B",
                      "NAge39_A_B",
                      "NAge40_A_B",
                      "NAge41_A_B",
                      "NAge42_A_B",
                      "NAge43_A_B",
                      "NAge44_A_B",
                      "NAge45_A_B",
                      "NAge46_A_B",
                      "NAge47_A_B",
                      "NAge48_A_B",
                      "NAge49_A_B",
                      "NAge50_A_B",
                      "NAge51_A_B",
                      "NAge52_A_B",
                      "NAge53_A_B",
                      "NAge54_A_B",
                      "NAge55_A_B",
                      "NAge56_A_B",
                      "NAge57_A_B",
                      "NAge58_A_B",
                      "NAge59_A_B",
                      "NAge60_A_B",
                      "NAge61_A_B",
                      "NAge62_A_B",
                      "NAge63_A_B",
                      "NAge64_A_B",
                      "NAge65_A_B",
                      "NAge66_A_B",
                      "NAge67_A_B",
                      "NAge68_A_B",
                      "NAge69_A_B",
                      "NAge70_A_B",
                      "NAge71_A_B",
                      "NAge72_A_B",
                      "NAge73_A_B",
                      "NAge74_A_B",
                      "NAge75_A_B",
                      "NAge76_A_B",
                      "NAge77_A_B",
                      "NAge78_A_B",
                      "NAge79_A_B",
                      "NAge80_A_B",
                      "NAge81_A_B",
                      "NAge82_A_B",
                      "NAge83_A_B",
                      "NAge84_A_B",
                      "NAge85_A_B",
                      "NAge86_A_B",
                      "NAge87_A_B",
                      "NAge88_A_B",
                      "NAge89_A_B",
                      "NAge90_A_B",
                      "NAge91_A_B",
                      "NAge92_A_B",
                      "NAge93_A_B",
                      "NAge94_A_B",
                      "NAge95_A_B",
                      "NAge96_A_B",
                      "NAge97_A_B",
                      "NAge98_A_B",
                      "NAge99_A_B",
                      "NAge0_A_G",
                      "NAge1_A_G",
                      "NAge2_A_G",
                      "NAge3_A_G",
                      "NAge4_A_G",
                      "NAge5_A_G",
                      "NAge6_A_G",
                      "NAge7_A_G",
                      "NAge8_A_G",
                      "NAge9_A_G",
                      "NAge10_A_G",
                      "NAge11_A_G",
                      "NAge12_A_G",
                      "NAge13_A_G",
                      "NAge14_A_G",
                      "NAge15_A_G",
                      "NAge16_A_G",
                      "NAge17_A_G",
                      "NAge18_A_G",
                      "NAge19_A_G",
                      "NAge20_A_G",
                      "NAge21_A_G",
                      "NAge22_A_G",
                      "NAge23_A_G",
                      "NAge24_A_G",
                      "NAge25_A_G",
                      "NAge26_A_G",
                      "NAge27_A_G",
                      "NAge28_A_G",
                      "NAge29_A_G",
                      "NAge30_A_G",
                      "NAge31_A_G",
                      "NAge32_A_G",
                      "NAge33_A_G",
                      "NAge34_A_G",
                      "NAge35_A_G",
                      "NAge36_A_G",
                      "NAge37_A_G",
                      "NAge38_A_G",
                      "NAge39_A_G",
                      "NAge40_A_G",
                      "NAge41_A_G",
                      "NAge42_A_G",
                      "NAge43_A_G",
                      "NAge44_A_G",
                      "NAge45_A_G",
                      "NAge46_A_G",
                      "NAge47_A_G",
                      "NAge48_A_G",
                      "NAge49_A_G",
                      "NAge50_A_G",
                      "NAge51_A_G",
                      "NAge52_A_G",
                      "NAge53_A_G",
                      "NAge54_A_G",
                      "NAge55_A_G",
                      "NAge56_A_G",
                      "NAge57_A_G",
                      "NAge58_A_G",
                      "NAge59_A_G",
                      "NAge60_A_G",
                      "NAge61_A_G",
                      "NAge62_A_G",
                      "NAge63_A_G",
                      "NAge64_A_G",
                      "NAge65_A_G",
                      "NAge66_A_G",
                      "NAge67_A_G",
                      "NAge68_A_G",
                      "NAge69_A_G",
                      "NAge70_A_G",
                      "NAge71_A_G",
                      "NAge72_A_G",
                      "NAge73_A_G",
                      "NAge74_A_G",
                      "NAge75_A_G",
                      "NAge76_A_G",
                      "NAge77_A_G",
                      "NAge78_A_G",
                      "NAge79_A_G",
                      "NAge80_A_G",
                      "NAge81_A_G",
                      "NAge82_A_G",
                      "NAge83_A_G",
                      "NAge84_A_G",
                      "NAge85_A_G",
                      "NAge86_A_G",
                      "NAge87_A_G",
                      "NAge88_A_G",
                      "NAge89_A_G",
                      "NAge90_A_G",
                      "NAge91_A_G",
                      "NAge92_A_G",
                      "NAge93_A_G",
                      "NAge94_A_G",
                      "NAge95_A_G",
                      "NAge96_A_G",
                      "NAge97_A_G",
                      "NAge98_A_G",
                      "NAge99_A_G"),#,"TotalIncome"),
            by="HHID"]
  
  #  PSum <- PSum[TotalIncome>0]
  
  HHI <- merge(B,PSum,by="HHID")
  HHI[,EqSizeOECD := ifelse(Size==NKids,1+(NKids-1)*0.5,
                               1 + (Size-NKids-1)*0.7 + (NKids)*0.5)]
  HHI <- HHI[!is.na(HLiterate)]
  
  rm(P,B,PSum)
  save(HHI,year,file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  
  
  rm(HHI)
  
 
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
  

  load(file = "Cal_Edition.rda")
  Poverty<-as.data.table(Poverty)
  Edited<-Poverty[,.(Age,Sex,Cal_B_Edited,Cal_W_Edited)]
  P1<-merge(P1,Edited,by=c("Sex", "Age"))
  

  P1[,	B0	:=ifelse(	Age==	0	&	Sex=="Male"	,1	,0)]
  P1[,	B1	:=ifelse(	Age==	1	&	Sex=="Male"	,1	,0)]
  P1[,	B2	:=ifelse(	Age==	2	&	Sex=="Male"	,1	,0)]
  P1[,	B3	:=ifelse(	Age==	3	&	Sex=="Male"	,1	,0)]
  P1[,	B4	:=ifelse(	Age==	4	&	Sex=="Male"	,1	,0)]
  P1[,	B5	:=ifelse(	Age==	5	&	Sex=="Male"	,1	,0)]
  P1[,	B6	:=ifelse(	Age==	6	&	Sex=="Male"	,1	,0)]
  P1[,	B7	:=ifelse(	Age==	7	&	Sex=="Male"	,1	,0)]
  P1[,	B8	:=ifelse(	Age==	8	&	Sex=="Male"	,1	,0)]
  P1[,	B9	:=ifelse(	Age==	9	&	Sex=="Male"	,1	,0)]
  P1[,	B10	:=ifelse(	Age==	10	&	Sex=="Male"	,1	,0)]
  P1[,	B11	:=ifelse(	Age==	11	&	Sex=="Male"	,1	,0)]
  P1[,	B12	:=ifelse(	Age==	12	&	Sex=="Male"	,1	,0)]
  P1[,	B13	:=ifelse(	Age==	13	&	Sex=="Male"	,1	,0)]
  P1[,	B14	:=ifelse(	Age==	14	&	Sex=="Male"	,1	,0)]
  P1[,	B15	:=ifelse(	Age==	15	&	Sex=="Male"	,1	,0)]
  P1[,	B16	:=ifelse(	Age==	16	&	Sex=="Male"	,1	,0)]
  P1[,	B17	:=ifelse(	Age==	17	&	Sex=="Male"	,1	,0)]
  P1[,	B18	:=ifelse(	Age==	18	&	Sex=="Male"	,1	,0)]
  P1[,	B19	:=ifelse(	Age==	19	&	Sex=="Male"	,1	,0)]
  P1[,	B20	:=ifelse(	Age==	20	&	Sex=="Male"	,1	,0)]
  P1[,	B21	:=ifelse(	Age==	21	&	Sex=="Male"	,1	,0)]
  P1[,	B22	:=ifelse(	Age==	22	&	Sex=="Male"	,1	,0)]
  P1[,	B23	:=ifelse(	Age==	23	&	Sex=="Male"	,1	,0)]
  P1[,	B24	:=ifelse(	Age==	24	&	Sex=="Male"	,1	,0)]
  P1[,	B25	:=ifelse(	Age==	25	&	Sex=="Male"	,1	,0)]
  P1[,	B26	:=ifelse(	Age==	26	&	Sex=="Male"	,1	,0)]
  P1[,	B27	:=ifelse(	Age==	27	&	Sex=="Male"	,1	,0)]
  P1[,	B28	:=ifelse(	Age==	28	&	Sex=="Male"	,1	,0)]
  P1[,	B29	:=ifelse(	Age==	29	&	Sex=="Male"	,1	,0)]
  P1[,	B30	:=ifelse(	Age==	30	&	Sex=="Male"	,1	,0)]
  P1[,	B31	:=ifelse(	Age==	31	&	Sex=="Male"	,1	,0)]
  P1[,	B32	:=ifelse(	Age==	32	&	Sex=="Male"	,1	,0)]
  P1[,	B33	:=ifelse(	Age==	33	&	Sex=="Male"	,1	,0)]
  P1[,	B34	:=ifelse(	Age==	34	&	Sex=="Male"	,1	,0)]
  P1[,	B35	:=ifelse(	Age==	35	&	Sex=="Male"	,1	,0)]
  P1[,	B36	:=ifelse(	Age==	36	&	Sex=="Male"	,1	,0)]
  P1[,	B37	:=ifelse(	Age==	37	&	Sex=="Male"	,1	,0)]
  P1[,	B38	:=ifelse(	Age==	38	&	Sex=="Male"	,1	,0)]
  P1[,	B39	:=ifelse(	Age==	39	&	Sex=="Male"	,1	,0)]
  P1[,	B40	:=ifelse(	Age==	40	&	Sex=="Male"	,1	,0)]
  P1[,	B41	:=ifelse(	Age==	41	&	Sex=="Male"	,1	,0)]
  P1[,	B42	:=ifelse(	Age==	42	&	Sex=="Male"	,1	,0)]
  P1[,	B43	:=ifelse(	Age==	43	&	Sex=="Male"	,1	,0)]
  P1[,	B44	:=ifelse(	Age==	44	&	Sex=="Male"	,1	,0)]
  P1[,	B45	:=ifelse(	Age==	45	&	Sex=="Male"	,1	,0)]
  P1[,	B46	:=ifelse(	Age==	46	&	Sex=="Male"	,1	,0)]
  P1[,	B47	:=ifelse(	Age==	47	&	Sex=="Male"	,1	,0)]
  P1[,	B48	:=ifelse(	Age==	48	&	Sex=="Male"	,1	,0)]
  P1[,	B49	:=ifelse(	Age==	49	&	Sex=="Male"	,1	,0)]
  P1[,	B50	:=ifelse(	Age==	50	&	Sex=="Male"	,1	,0)]
  P1[,	B51	:=ifelse(	Age==	51	&	Sex=="Male"	,1	,0)]
  P1[,	B52	:=ifelse(	Age==	52	&	Sex=="Male"	,1	,0)]
  P1[,	B53	:=ifelse(	Age==	53	&	Sex=="Male"	,1	,0)]
  P1[,	B54	:=ifelse(	Age==	54	&	Sex=="Male"	,1	,0)]
  P1[,	B55	:=ifelse(	Age==	55	&	Sex=="Male"	,1	,0)]
  P1[,	B56	:=ifelse(	Age==	56	&	Sex=="Male"	,1	,0)]
  P1[,	B57	:=ifelse(	Age==	57	&	Sex=="Male"	,1	,0)]
  P1[,	B58	:=ifelse(	Age==	58	&	Sex=="Male"	,1	,0)]
  P1[,	B59	:=ifelse(	Age==	59	&	Sex=="Male"	,1	,0)]
  P1[,	B60	:=ifelse(	Age==	60	&	Sex=="Male"	,1	,0)]
  P1[,	B61	:=ifelse(	Age==	61	&	Sex=="Male"	,1	,0)]
  P1[,	B62	:=ifelse(	Age==	62	&	Sex=="Male"	,1	,0)]
  P1[,	B63	:=ifelse(	Age==	63	&	Sex=="Male"	,1	,0)]
  P1[,	B64	:=ifelse(	Age==	64	&	Sex=="Male"	,1	,0)]
  P1[,	B65	:=ifelse(	Age==	65	&	Sex=="Male"	,1	,0)]
  P1[,	B66	:=ifelse(	Age==	66	&	Sex=="Male"	,1	,0)]
  P1[,	B67	:=ifelse(	Age==	67	&	Sex=="Male"	,1	,0)]
  P1[,	B68	:=ifelse(	Age==	68	&	Sex=="Male"	,1	,0)]
  P1[,	B69	:=ifelse(	Age==	69	&	Sex=="Male"	,1	,0)]
  P1[,	B70	:=ifelse(	Age==	70	&	Sex=="Male"	,1	,0)]
  P1[,	B71	:=ifelse(	Age==	71	&	Sex=="Male"	,1	,0)]
  P1[,	B72	:=ifelse(	Age==	72	&	Sex=="Male"	,1	,0)]
  P1[,	B73	:=ifelse(	Age==	73	&	Sex=="Male"	,1	,0)]
  P1[,	B74	:=ifelse(	Age==	74	&	Sex=="Male"	,1	,0)]
  P1[,	B75	:=ifelse(	Age==	75	&	Sex=="Male"	,1	,0)]
  P1[,	B76	:=ifelse(	Age==	76	&	Sex=="Male"	,1	,0)]
  P1[,	B77	:=ifelse(	Age==	77	&	Sex=="Male"	,1	,0)]
  P1[,	B78	:=ifelse(	Age==	78	&	Sex=="Male"	,1	,0)]
  P1[,	B79	:=ifelse(	Age==	79	&	Sex=="Male"	,1	,0)]
  P1[,	B80	:=ifelse(	Age==	80	&	Sex=="Male"	,1	,0)]
  P1[,	B81	:=ifelse(	Age==	81	&	Sex=="Male"	,1	,0)]
  P1[,	B82	:=ifelse(	Age==	82	&	Sex=="Male"	,1	,0)]
  P1[,	B83	:=ifelse(	Age==	83	&	Sex=="Male"	,1	,0)]
  P1[,	B84	:=ifelse(	Age==	84	&	Sex=="Male"	,1	,0)]
  P1[,	B85	:=ifelse(	Age==	85	&	Sex=="Male"	,1	,0)]
  P1[,	B86	:=ifelse(	Age==	86	&	Sex=="Male"	,1	,0)]
  P1[,	B87	:=ifelse(	Age==	87	&	Sex=="Male"	,1	,0)]
  P1[,	B88	:=ifelse(	Age==	88	&	Sex=="Male"	,1	,0)]
  P1[,	B89	:=ifelse(	Age==	89	&	Sex=="Male"	,1	,0)]
  P1[,	B90	:=ifelse(	Age==	90	&	Sex=="Male"	,1	,0)]
  P1[,	B91	:=ifelse(	Age==	91	&	Sex=="Male"	,1	,0)]
  P1[,	B92	:=ifelse(	Age==	92	&	Sex=="Male"	,1	,0)]
  P1[,	B93	:=ifelse(	Age==	93	&	Sex=="Male"	,1	,0)]
  P1[,	B94	:=ifelse(	Age==	94	&	Sex=="Male"	,1	,0)]
  P1[,	B95	:=ifelse(	Age==	95	&	Sex=="Male"	,1	,0)]
  P1[,	B96	:=ifelse(	Age==	96	&	Sex=="Male"	,1	,0)]
  P1[,	B97	:=ifelse(	Age==	97	&	Sex=="Male"	,1	,0)]
  P1[,	B98	:=ifelse(	Age==	98	&	Sex=="Male"	,1	,0)]
  P1[,	B99	:=ifelse(	Age==	99	&	Sex=="Male"	,1	,0)]
  P1[,	G0	:=ifelse(	Age==	0	&	Sex=="Female"	,1	,0)]
  P1[,	G1	:=ifelse(	Age==	1	&	Sex=="Female"	,1	,0)]
  P1[,	G2	:=ifelse(	Age==	2	&	Sex=="Female"	,1	,0)]
  P1[,	G3	:=ifelse(	Age==	3	&	Sex=="Female"	,1	,0)]
  P1[,	G4	:=ifelse(	Age==	4	&	Sex=="Female"	,1	,0)]
  P1[,	G5	:=ifelse(	Age==	5	&	Sex=="Female"	,1	,0)]
  P1[,	G6	:=ifelse(	Age==	6	&	Sex=="Female"	,1	,0)]
  P1[,	G7	:=ifelse(	Age==	7	&	Sex=="Female"	,1	,0)]
  P1[,	G8	:=ifelse(	Age==	8	&	Sex=="Female"	,1	,0)]
  P1[,	G9	:=ifelse(	Age==	9	&	Sex=="Female"	,1	,0)]
  P1[,	G10	:=ifelse(	Age==	10	&	Sex=="Female"	,1	,0)]
  P1[,	G11	:=ifelse(	Age==	11	&	Sex=="Female"	,1	,0)]
  P1[,	G12	:=ifelse(	Age==	12	&	Sex=="Female"	,1	,0)]
  P1[,	G13	:=ifelse(	Age==	13	&	Sex=="Female"	,1	,0)]
  P1[,	G14	:=ifelse(	Age==	14	&	Sex=="Female"	,1	,0)]
  P1[,	G15	:=ifelse(	Age==	15	&	Sex=="Female"	,1	,0)]
  P1[,	G16	:=ifelse(	Age==	16	&	Sex=="Female"	,1	,0)]
  P1[,	G17	:=ifelse(	Age==	17	&	Sex=="Female"	,1	,0)]
  P1[,	G18	:=ifelse(	Age==	18	&	Sex=="Female"	,1	,0)]
  P1[,	G19	:=ifelse(	Age==	19	&	Sex=="Female"	,1	,0)]
  P1[,	G20	:=ifelse(	Age==	20	&	Sex=="Female"	,1	,0)]
  P1[,	G21	:=ifelse(	Age==	21	&	Sex=="Female"	,1	,0)]
  P1[,	G22	:=ifelse(	Age==	22	&	Sex=="Female"	,1	,0)]
  P1[,	G23	:=ifelse(	Age==	23	&	Sex=="Female"	,1	,0)]
  P1[,	G24	:=ifelse(	Age==	24	&	Sex=="Female"	,1	,0)]
  P1[,	G25	:=ifelse(	Age==	25	&	Sex=="Female"	,1	,0)]
  P1[,	G26	:=ifelse(	Age==	26	&	Sex=="Female"	,1	,0)]
  P1[,	G27	:=ifelse(	Age==	27	&	Sex=="Female"	,1	,0)]
  P1[,	G28	:=ifelse(	Age==	28	&	Sex=="Female"	,1	,0)]
  P1[,	G29	:=ifelse(	Age==	29	&	Sex=="Female"	,1	,0)]
  P1[,	G30	:=ifelse(	Age==	30	&	Sex=="Female"	,1	,0)]
  P1[,	G31	:=ifelse(	Age==	31	&	Sex=="Female"	,1	,0)]
  P1[,	G32	:=ifelse(	Age==	32	&	Sex=="Female"	,1	,0)]
  P1[,	G33	:=ifelse(	Age==	33	&	Sex=="Female"	,1	,0)]
  P1[,	G34	:=ifelse(	Age==	34	&	Sex=="Female"	,1	,0)]
  P1[,	G35	:=ifelse(	Age==	35	&	Sex=="Female"	,1	,0)]
  P1[,	G36	:=ifelse(	Age==	36	&	Sex=="Female"	,1	,0)]
  P1[,	G37	:=ifelse(	Age==	37	&	Sex=="Female"	,1	,0)]
  P1[,	G38	:=ifelse(	Age==	38	&	Sex=="Female"	,1	,0)]
  P1[,	G39	:=ifelse(	Age==	39	&	Sex=="Female"	,1	,0)]
  P1[,	G40	:=ifelse(	Age==	40	&	Sex=="Female"	,1	,0)]
  P1[,	G41	:=ifelse(	Age==	41	&	Sex=="Female"	,1	,0)]
  P1[,	G42	:=ifelse(	Age==	42	&	Sex=="Female"	,1	,0)]
  P1[,	G43	:=ifelse(	Age==	43	&	Sex=="Female"	,1	,0)]
  P1[,	G44	:=ifelse(	Age==	44	&	Sex=="Female"	,1	,0)]
  P1[,	G45	:=ifelse(	Age==	45	&	Sex=="Female"	,1	,0)]
  P1[,	G46	:=ifelse(	Age==	46	&	Sex=="Female"	,1	,0)]
  P1[,	G47	:=ifelse(	Age==	47	&	Sex=="Female"	,1	,0)]
  P1[,	G48	:=ifelse(	Age==	48	&	Sex=="Female"	,1	,0)]
  P1[,	G49	:=ifelse(	Age==	49	&	Sex=="Female"	,1	,0)]
  P1[,	G50	:=ifelse(	Age==	50	&	Sex=="Female"	,1	,0)]
  P1[,	G51	:=ifelse(	Age==	51	&	Sex=="Female"	,1	,0)]
  P1[,	G52	:=ifelse(	Age==	52	&	Sex=="Female"	,1	,0)]
  P1[,	G53	:=ifelse(	Age==	53	&	Sex=="Female"	,1	,0)]
  P1[,	G54	:=ifelse(	Age==	54	&	Sex=="Female"	,1	,0)]
  P1[,	G55	:=ifelse(	Age==	55	&	Sex=="Female"	,1	,0)]
  P1[,	G56	:=ifelse(	Age==	56	&	Sex=="Female"	,1	,0)]
  P1[,	G57	:=ifelse(	Age==	57	&	Sex=="Female"	,1	,0)]
  P1[,	G58	:=ifelse(	Age==	58	&	Sex=="Female"	,1	,0)]
  P1[,	G59	:=ifelse(	Age==	59	&	Sex=="Female"	,1	,0)]
  P1[,	G60	:=ifelse(	Age==	60	&	Sex=="Female"	,1	,0)]
  P1[,	G61	:=ifelse(	Age==	61	&	Sex=="Female"	,1	,0)]
  P1[,	G62	:=ifelse(	Age==	62	&	Sex=="Female"	,1	,0)]
  P1[,	G63	:=ifelse(	Age==	63	&	Sex=="Female"	,1	,0)]
  P1[,	G64	:=ifelse(	Age==	64	&	Sex=="Female"	,1	,0)]
  P1[,	G65	:=ifelse(	Age==	65	&	Sex=="Female"	,1	,0)]
  P1[,	G66	:=ifelse(	Age==	66	&	Sex=="Female"	,1	,0)]
  P1[,	G67	:=ifelse(	Age==	67	&	Sex=="Female"	,1	,0)]
  P1[,	G68	:=ifelse(	Age==	68	&	Sex=="Female"	,1	,0)]
  P1[,	G69	:=ifelse(	Age==	69	&	Sex=="Female"	,1	,0)]
  P1[,	G70	:=ifelse(	Age==	70	&	Sex=="Female"	,1	,0)]
  P1[,	G71	:=ifelse(	Age==	71	&	Sex=="Female"	,1	,0)]
  P1[,	G72	:=ifelse(	Age==	72	&	Sex=="Female"	,1	,0)]
  P1[,	G73	:=ifelse(	Age==	73	&	Sex=="Female"	,1	,0)]
  P1[,	G74	:=ifelse(	Age==	74	&	Sex=="Female"	,1	,0)]
  P1[,	G75	:=ifelse(	Age==	75	&	Sex=="Female"	,1	,0)]
  P1[,	G76	:=ifelse(	Age==	76	&	Sex=="Female"	,1	,0)]
  P1[,	G77	:=ifelse(	Age==	77	&	Sex=="Female"	,1	,0)]
  P1[,	G78	:=ifelse(	Age==	78	&	Sex=="Female"	,1	,0)]
  P1[,	G79	:=ifelse(	Age==	79	&	Sex=="Female"	,1	,0)]
  P1[,	G80	:=ifelse(	Age==	80	&	Sex=="Female"	,1	,0)]
  P1[,	G81	:=ifelse(	Age==	81	&	Sex=="Female"	,1	,0)]
  P1[,	G82	:=ifelse(	Age==	82	&	Sex=="Female"	,1	,0)]
  P1[,	G83	:=ifelse(	Age==	83	&	Sex=="Female"	,1	,0)]
  P1[,	G84	:=ifelse(	Age==	84	&	Sex=="Female"	,1	,0)]
  P1[,	G85	:=ifelse(	Age==	85	&	Sex=="Female"	,1	,0)]
  P1[,	G86	:=ifelse(	Age==	86	&	Sex=="Female"	,1	,0)]
  P1[,	G87	:=ifelse(	Age==	87	&	Sex=="Female"	,1	,0)]
  P1[,	G88	:=ifelse(	Age==	88	&	Sex=="Female"	,1	,0)]
  P1[,	G89	:=ifelse(	Age==	89	&	Sex=="Female"	,1	,0)]
  P1[,	G90	:=ifelse(	Age==	90	&	Sex=="Female"	,1	,0)]
  P1[,	G91	:=ifelse(	Age==	91	&	Sex=="Female"	,1	,0)]
  P1[,	G92	:=ifelse(	Age==	92	&	Sex=="Female"	,1	,0)]
  P1[,	G93	:=ifelse(	Age==	93	&	Sex=="Female"	,1	,0)]
  P1[,	G94	:=ifelse(	Age==	94	&	Sex=="Female"	,1	,0)]
  P1[,	G95	:=ifelse(	Age==	95	&	Sex=="Female"	,1	,0)]
  P1[,	G96	:=ifelse(	Age==	96	&	Sex=="Female"	,1	,0)]
  P1[,	G97	:=ifelse(	Age==	97	&	Sex=="Female"	,1	,0)]
  P1[,	G98	:=ifelse(	Age==	98	&	Sex=="Female"	,1	,0)]
  P1[,	G99	:=ifelse(	Age==	99	&	Sex=="Female"	,1	,0)]
  
  
  
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
           weighted.mean(	B0	,Weight)*Settings$	KCaloryNeed_B0	+
           weighted.mean(	B1	,Weight)*Settings$	KCaloryNeed_B1	+
           weighted.mean(	B2	,Weight)*Settings$	KCaloryNeed_B2	+
           weighted.mean(	B3	,Weight)*Settings$	KCaloryNeed_B3	+
           weighted.mean(	B4	,Weight)*Settings$	KCaloryNeed_B4	+
           weighted.mean(	B5	,Weight)*Settings$	KCaloryNeed_B5	+
           weighted.mean(	B6	,Weight)*Settings$	KCaloryNeed_B6	+
           weighted.mean(	B7	,Weight)*Settings$	KCaloryNeed_B7	+
           weighted.mean(	B8	,Weight)*Settings$	KCaloryNeed_B8	+
           weighted.mean(	B9	,Weight)*Settings$	KCaloryNeed_B9	+
           weighted.mean(	B10	,Weight)*Settings$	KCaloryNeed_B10	+
           weighted.mean(	B11	,Weight)*Settings$	KCaloryNeed_B11	+
           weighted.mean(	B12	,Weight)*Settings$	KCaloryNeed_B12	+
           weighted.mean(	B13	,Weight)*Settings$	KCaloryNeed_B13	+
           weighted.mean(	B14	,Weight)*Settings$	KCaloryNeed_B14	+
           weighted.mean(	B15	,Weight)*Settings$	KCaloryNeed_B15	+
           weighted.mean(	B16	,Weight)*Settings$	KCaloryNeed_B16	+
           weighted.mean(	B17	,Weight)*Settings$	KCaloryNeed_B17	+
           weighted.mean(	B18	,Weight)*Settings$	KCaloryNeed_B18	+
           weighted.mean(	B19	,Weight)*Settings$	KCaloryNeed_B19	+
           weighted.mean(	B20	,Weight)*Settings$	KCaloryNeed_B20	+
           weighted.mean(	B21	,Weight)*Settings$	KCaloryNeed_B21	+
           weighted.mean(	B22	,Weight)*Settings$	KCaloryNeed_B22	+
           weighted.mean(	B23	,Weight)*Settings$	KCaloryNeed_B23	+
           weighted.mean(	B24	,Weight)*Settings$	KCaloryNeed_B24	+
           weighted.mean(	B25	,Weight)*Settings$	KCaloryNeed_B25	+
           weighted.mean(	B26	,Weight)*Settings$	KCaloryNeed_B26	+
           weighted.mean(	B27	,Weight)*Settings$	KCaloryNeed_B27	+
           weighted.mean(	B28	,Weight)*Settings$	KCaloryNeed_B28	+
           weighted.mean(	B29	,Weight)*Settings$	KCaloryNeed_B29	+
           weighted.mean(	B30	,Weight)*Settings$	KCaloryNeed_B30	+
           weighted.mean(	B31	,Weight)*Settings$	KCaloryNeed_B31	+
           weighted.mean(	B32	,Weight)*Settings$	KCaloryNeed_B32	+
           weighted.mean(	B33	,Weight)*Settings$	KCaloryNeed_B33	+
           weighted.mean(	B34	,Weight)*Settings$	KCaloryNeed_B34	+
           weighted.mean(	B35	,Weight)*Settings$	KCaloryNeed_B35	+
           weighted.mean(	B36	,Weight)*Settings$	KCaloryNeed_B36	+
           weighted.mean(	B37	,Weight)*Settings$	KCaloryNeed_B37	+
           weighted.mean(	B38	,Weight)*Settings$	KCaloryNeed_B38	+
           weighted.mean(	B39	,Weight)*Settings$	KCaloryNeed_B39	+
           weighted.mean(	B40	,Weight)*Settings$	KCaloryNeed_B40	+
           weighted.mean(	B41	,Weight)*Settings$	KCaloryNeed_B41	+
           weighted.mean(	B42	,Weight)*Settings$	KCaloryNeed_B42	+
           weighted.mean(	B43	,Weight)*Settings$	KCaloryNeed_B43	+
           weighted.mean(	B44	,Weight)*Settings$	KCaloryNeed_B44	+
           weighted.mean(	B45	,Weight)*Settings$	KCaloryNeed_B45	+
           weighted.mean(	B46	,Weight)*Settings$	KCaloryNeed_B46	+
           weighted.mean(	B47	,Weight)*Settings$	KCaloryNeed_B47	+
           weighted.mean(	B48	,Weight)*Settings$	KCaloryNeed_B48	+
           weighted.mean(	B49	,Weight)*Settings$	KCaloryNeed_B49	+
           weighted.mean(	B50	,Weight)*Settings$	KCaloryNeed_B50	+
           weighted.mean(	B51	,Weight)*Settings$	KCaloryNeed_B51	+
           weighted.mean(	B52	,Weight)*Settings$	KCaloryNeed_B52	+
           weighted.mean(	B53	,Weight)*Settings$	KCaloryNeed_B53	+
           weighted.mean(	B54	,Weight)*Settings$	KCaloryNeed_B54	+
           weighted.mean(	B55	,Weight)*Settings$	KCaloryNeed_B55	+
           weighted.mean(	B56	,Weight)*Settings$	KCaloryNeed_B56	+
           weighted.mean(	B57	,Weight)*Settings$	KCaloryNeed_B57	+
           weighted.mean(	B58	,Weight)*Settings$	KCaloryNeed_B58	+
           weighted.mean(	B59	,Weight)*Settings$	KCaloryNeed_B59	+
           weighted.mean(	B60	,Weight)*Settings$	KCaloryNeed_B60	+
           weighted.mean(	B61	,Weight)*Settings$	KCaloryNeed_B61	+
           weighted.mean(	B62	,Weight)*Settings$	KCaloryNeed_B62	+
           weighted.mean(	B63	,Weight)*Settings$	KCaloryNeed_B63	+
           weighted.mean(	B64	,Weight)*Settings$	KCaloryNeed_B64	+
           weighted.mean(	B65	,Weight)*Settings$	KCaloryNeed_B65	+
           weighted.mean(	B66	,Weight)*Settings$	KCaloryNeed_B66	+
           weighted.mean(	B67	,Weight)*Settings$	KCaloryNeed_B67	+
           weighted.mean(	B68	,Weight)*Settings$	KCaloryNeed_B68	+
           weighted.mean(	B69	,Weight)*Settings$	KCaloryNeed_B69	+
           weighted.mean(	B70	,Weight)*Settings$	KCaloryNeed_B70	+
           weighted.mean(	B71	,Weight)*Settings$	KCaloryNeed_B71	+
           weighted.mean(	B72	,Weight)*Settings$	KCaloryNeed_B72	+
           weighted.mean(	B73	,Weight)*Settings$	KCaloryNeed_B73	+
           weighted.mean(	B74	,Weight)*Settings$	KCaloryNeed_B74	+
           weighted.mean(	B75	,Weight)*Settings$	KCaloryNeed_B75	+
           weighted.mean(	B76	,Weight)*Settings$	KCaloryNeed_B76	+
           weighted.mean(	B77	,Weight)*Settings$	KCaloryNeed_B77	+
           weighted.mean(	B78	,Weight)*Settings$	KCaloryNeed_B78	+
           weighted.mean(	B79	,Weight)*Settings$	KCaloryNeed_B79	+
           weighted.mean(	B80	,Weight)*Settings$	KCaloryNeed_B80	+
           weighted.mean(	B81	,Weight)*Settings$	KCaloryNeed_B81	+
           weighted.mean(	B82	,Weight)*Settings$	KCaloryNeed_B82	+
           weighted.mean(	B83	,Weight)*Settings$	KCaloryNeed_B83	+
           weighted.mean(	B84	,Weight)*Settings$	KCaloryNeed_B84	+
           weighted.mean(	B85	,Weight)*Settings$	KCaloryNeed_B85	+
           weighted.mean(	B86	,Weight)*Settings$	KCaloryNeed_B86	+
           weighted.mean(	B87	,Weight)*Settings$	KCaloryNeed_B87	+
           weighted.mean(	B88	,Weight)*Settings$	KCaloryNeed_B88	+
           weighted.mean(	B89	,Weight)*Settings$	KCaloryNeed_B89	+
           weighted.mean(	B90	,Weight)*Settings$	KCaloryNeed_B90	+
           weighted.mean(	B91	,Weight)*Settings$	KCaloryNeed_B91	+
           weighted.mean(	B92	,Weight)*Settings$	KCaloryNeed_B92	+
           weighted.mean(	B93	,Weight)*Settings$	KCaloryNeed_B93	+
           weighted.mean(	B94	,Weight)*Settings$	KCaloryNeed_B94	+
           weighted.mean(	B95	,Weight)*Settings$	KCaloryNeed_B95	+
           weighted.mean(	B96	,Weight)*Settings$	KCaloryNeed_B96	+
           weighted.mean(	B97	,Weight)*Settings$	KCaloryNeed_B97	+
           weighted.mean(	B98	,Weight)*Settings$	KCaloryNeed_B98	+
           weighted.mean(	B99	,Weight)*Settings$	KCaloryNeed_B99	+
           weighted.mean(	G0	,Weight)*Settings$	KCaloryNeed_G0	+
           weighted.mean(	G1	,Weight)*Settings$	KCaloryNeed_G1	+
           weighted.mean(	G2	,Weight)*Settings$	KCaloryNeed_G2	+
           weighted.mean(	G3	,Weight)*Settings$	KCaloryNeed_G3	+
           weighted.mean(	G4	,Weight)*Settings$	KCaloryNeed_G4	+
           weighted.mean(	G5	,Weight)*Settings$	KCaloryNeed_G5	+
           weighted.mean(	G6	,Weight)*Settings$	KCaloryNeed_G6	+
           weighted.mean(	G7	,Weight)*Settings$	KCaloryNeed_G7	+
           weighted.mean(	G8	,Weight)*Settings$	KCaloryNeed_G8	+
           weighted.mean(	G9	,Weight)*Settings$	KCaloryNeed_G9	+
           weighted.mean(	G10	,Weight)*Settings$	KCaloryNeed_G10	+
           weighted.mean(	G11	,Weight)*Settings$	KCaloryNeed_G11	+
           weighted.mean(	G12	,Weight)*Settings$	KCaloryNeed_G12	+
           weighted.mean(	G13	,Weight)*Settings$	KCaloryNeed_G13	+
           weighted.mean(	G14	,Weight)*Settings$	KCaloryNeed_G14	+
           weighted.mean(	G15	,Weight)*Settings$	KCaloryNeed_G15	+
           weighted.mean(	G16	,Weight)*Settings$	KCaloryNeed_G16	+
           weighted.mean(	G17	,Weight)*Settings$	KCaloryNeed_G17	+
           weighted.mean(	G18	,Weight)*Settings$	KCaloryNeed_G18	+
           weighted.mean(	G19	,Weight)*Settings$	KCaloryNeed_G19	+
           weighted.mean(	G20	,Weight)*Settings$	KCaloryNeed_G20	+
           weighted.mean(	G21	,Weight)*Settings$	KCaloryNeed_G21	+
           weighted.mean(	G22	,Weight)*Settings$	KCaloryNeed_G22	+
           weighted.mean(	G23	,Weight)*Settings$	KCaloryNeed_G23	+
           weighted.mean(	G24	,Weight)*Settings$	KCaloryNeed_G24	+
           weighted.mean(	G25	,Weight)*Settings$	KCaloryNeed_G25	+
           weighted.mean(	G26	,Weight)*Settings$	KCaloryNeed_G26	+
           weighted.mean(	G27	,Weight)*Settings$	KCaloryNeed_G27	+
           weighted.mean(	G28	,Weight)*Settings$	KCaloryNeed_G28	+
           weighted.mean(	G29	,Weight)*Settings$	KCaloryNeed_G29	+
           weighted.mean(	G30	,Weight)*Settings$	KCaloryNeed_G30	+
           weighted.mean(	G31	,Weight)*Settings$	KCaloryNeed_G31	+
           weighted.mean(	G32	,Weight)*Settings$	KCaloryNeed_G32	+
           weighted.mean(	G33	,Weight)*Settings$	KCaloryNeed_G33	+
           weighted.mean(	G34	,Weight)*Settings$	KCaloryNeed_G34	+
           weighted.mean(	G35	,Weight)*Settings$	KCaloryNeed_G35	+
           weighted.mean(	G36	,Weight)*Settings$	KCaloryNeed_G36	+
           weighted.mean(	G37	,Weight)*Settings$	KCaloryNeed_G37	+
           weighted.mean(	G38	,Weight)*Settings$	KCaloryNeed_G38	+
           weighted.mean(	G39	,Weight)*Settings$	KCaloryNeed_G39	+
           weighted.mean(	G40	,Weight)*Settings$	KCaloryNeed_G40	+
           weighted.mean(	G41	,Weight)*Settings$	KCaloryNeed_G41	+
           weighted.mean(	G42	,Weight)*Settings$	KCaloryNeed_G42	+
           weighted.mean(	G43	,Weight)*Settings$	KCaloryNeed_G43	+
           weighted.mean(	G44	,Weight)*Settings$	KCaloryNeed_G44	+
           weighted.mean(	G45	,Weight)*Settings$	KCaloryNeed_G45	+
           weighted.mean(	G46	,Weight)*Settings$	KCaloryNeed_G46	+
           weighted.mean(	G47	,Weight)*Settings$	KCaloryNeed_G47	+
           weighted.mean(	G48	,Weight)*Settings$	KCaloryNeed_G48	+
           weighted.mean(	G49	,Weight)*Settings$	KCaloryNeed_G49	+
           weighted.mean(	G50	,Weight)*Settings$	KCaloryNeed_G50	+
           weighted.mean(	G51	,Weight)*Settings$	KCaloryNeed_G51	+
           weighted.mean(	G52	,Weight)*Settings$	KCaloryNeed_G52	+
           weighted.mean(	G53	,Weight)*Settings$	KCaloryNeed_G53	+
           weighted.mean(	G54	,Weight)*Settings$	KCaloryNeed_G54	+
           weighted.mean(	G55	,Weight)*Settings$	KCaloryNeed_G55	+
           weighted.mean(	G56	,Weight)*Settings$	KCaloryNeed_G56	+
           weighted.mean(	G57	,Weight)*Settings$	KCaloryNeed_G57	+
           weighted.mean(	G58	,Weight)*Settings$	KCaloryNeed_G58	+
           weighted.mean(	G59	,Weight)*Settings$	KCaloryNeed_G59	+
           weighted.mean(	G60	,Weight)*Settings$	KCaloryNeed_G60	+
           weighted.mean(	G61	,Weight)*Settings$	KCaloryNeed_G61	+
           weighted.mean(	G62	,Weight)*Settings$	KCaloryNeed_G62	+
           weighted.mean(	G63	,Weight)*Settings$	KCaloryNeed_G63	+
           weighted.mean(	G64	,Weight)*Settings$	KCaloryNeed_G64	+
           weighted.mean(	G65	,Weight)*Settings$	KCaloryNeed_G65	+
           weighted.mean(	G66	,Weight)*Settings$	KCaloryNeed_G66	+
           weighted.mean(	G67	,Weight)*Settings$	KCaloryNeed_G67	+
           weighted.mean(	G68	,Weight)*Settings$	KCaloryNeed_G68	+
           weighted.mean(	G69	,Weight)*Settings$	KCaloryNeed_G69	+
           weighted.mean(	G70	,Weight)*Settings$	KCaloryNeed_G70	+
           weighted.mean(	G71	,Weight)*Settings$	KCaloryNeed_G71	+
           weighted.mean(	G72	,Weight)*Settings$	KCaloryNeed_G72	+
           weighted.mean(	G73	,Weight)*Settings$	KCaloryNeed_G73	+
           weighted.mean(	G74	,Weight)*Settings$	KCaloryNeed_G74	+
           weighted.mean(	G75	,Weight)*Settings$	KCaloryNeed_G75	+
           weighted.mean(	G76	,Weight)*Settings$	KCaloryNeed_G76	+
           weighted.mean(	G77	,Weight)*Settings$	KCaloryNeed_G77	+
           weighted.mean(	G78	,Weight)*Settings$	KCaloryNeed_G78	+
           weighted.mean(	G79	,Weight)*Settings$	KCaloryNeed_G79	+
           weighted.mean(	G80	,Weight)*Settings$	KCaloryNeed_G80	+
           weighted.mean(	G81	,Weight)*Settings$	KCaloryNeed_G81	+
           weighted.mean(	G82	,Weight)*Settings$	KCaloryNeed_G82	+
           weighted.mean(	G83	,Weight)*Settings$	KCaloryNeed_G83	+
           weighted.mean(	G84	,Weight)*Settings$	KCaloryNeed_G84	+
           weighted.mean(	G85	,Weight)*Settings$	KCaloryNeed_G85	+
           weighted.mean(	G86	,Weight)*Settings$	KCaloryNeed_G86	+
           weighted.mean(	G87	,Weight)*Settings$	KCaloryNeed_G87	+
           weighted.mean(	G88	,Weight)*Settings$	KCaloryNeed_G88	+
           weighted.mean(	G89	,Weight)*Settings$	KCaloryNeed_G89	+
           weighted.mean(	G90	,Weight)*Settings$	KCaloryNeed_G90	+
           weighted.mean(	G91	,Weight)*Settings$	KCaloryNeed_G91	+
           weighted.mean(	G92	,Weight)*Settings$	KCaloryNeed_G92	+
           weighted.mean(	G93	,Weight)*Settings$	KCaloryNeed_G93	+
           weighted.mean(	G94	,Weight)*Settings$	KCaloryNeed_G94	+
           weighted.mean(	G95	,Weight)*Settings$	KCaloryNeed_G95	+
           weighted.mean(	G96	,Weight)*Settings$	KCaloryNeed_G96	+
           weighted.mean(	G97	,Weight)*Settings$	KCaloryNeed_G97	+
           weighted.mean(	G98	,Weight)*Settings$	KCaloryNeed_G98	+
           weighted.mean(	G99	,Weight)*Settings$	KCaloryNeed_G99	+
           weighted.mean(lactating,Weight)*(Settings$KCaloryNeed_lactating)]
  
  
  
  P1[,	BA0	:=ifelse(	Age==	0	&	Sex=="Male"	,1	,0)]
  P1[,	BA1	:=ifelse(	Age==	1	&	Sex=="Male"	,1	,0)]
  P1[,	BA2	:=ifelse(	Age==	2	&	Sex=="Male"	,1	,0)]
  P1[,	BA3	:=ifelse(	Age==	3	&	Sex=="Male"	,1	,0)]
  P1[,	BA4	:=ifelse(	Age==	4	&	Sex=="Male"	,1	,0)]
  P1[,	BA5	:=ifelse(	Age==	5	&	Sex=="Male"	,1	,0)]
  P1[,	BA6	:=ifelse(	Age==	6	&	Sex=="Male"	,1	,0)]
  P1[,	BA7	:=ifelse(	Age==	7	&	Sex=="Male"	,1	,0)]
  P1[,	BA8	:=ifelse(	Age==	8	&	Sex=="Male"	,1	,0)]
  P1[,	BA9	:=ifelse(	Age==	9	&	Sex=="Male"	,1	,0)]
  P1[,	BA10	:=ifelse(	Age==	10	&	Sex=="Male"	,1	,0)]
  P1[,	BA11	:=ifelse(	Age==	11	&	Sex=="Male"	,1	,0)]
  P1[,	BA12	:=ifelse(	Age==	12	&	Sex=="Male"	,1	,0)]
  P1[,	BA13	:=ifelse(	Age==	13	&	Sex=="Male"	,1	,0)]
  P1[,	BA14	:=ifelse(	Age==	14	&	Sex=="Male"	,1	,0)]
  P1[,	BA15	:=ifelse(	Age==	15	&	Sex=="Male"	,1	,0)]
  P1[,	BA16	:=ifelse(	Age==	16	&	Sex=="Male"	,1	,0)]
  P1[,	BA17	:=ifelse(	Age==	17	&	Sex=="Male"	,1	,0)]
  P1[,	BA18	:=ifelse(	Age==	18	&	Sex=="Male"	,1	,0)]
  P1[,	BA19	:=ifelse(	Age==	19	&	Sex=="Male"	,1	,0)]
  P1[,	BA20	:=ifelse(	Age==	20	&	Sex=="Male"	,1	,0)]
  P1[,	BA21	:=ifelse(	Age==	21	&	Sex=="Male"	,1	,0)]
  P1[,	BA22	:=ifelse(	Age==	22	&	Sex=="Male"	,1	,0)]
  P1[,	BA23	:=ifelse(	Age==	23	&	Sex=="Male"	,1	,0)]
  P1[,	BA24	:=ifelse(	Age==	24	&	Sex=="Male"	,1	,0)]
  P1[,	BA25	:=ifelse(	Age==	25	&	Sex=="Male"	,1	,0)]
  P1[,	BA26	:=ifelse(	Age==	26	&	Sex=="Male"	,1	,0)]
  P1[,	BA27	:=ifelse(	Age==	27	&	Sex=="Male"	,1	,0)]
  P1[,	BA28	:=ifelse(	Age==	28	&	Sex=="Male"	,1	,0)]
  P1[,	BA29	:=ifelse(	Age==	29	&	Sex=="Male"	,1	,0)]
  P1[,	BA30	:=ifelse(	Age==	30	&	Sex=="Male"	,1	,0)]
  P1[,	BA31	:=ifelse(	Age==	31	&	Sex=="Male"	,1	,0)]
  P1[,	BA32	:=ifelse(	Age==	32	&	Sex=="Male"	,1	,0)]
  P1[,	BA33	:=ifelse(	Age==	33	&	Sex=="Male"	,1	,0)]
  P1[,	BA34	:=ifelse(	Age==	34	&	Sex=="Male"	,1	,0)]
  P1[,	BA35	:=ifelse(	Age==	35	&	Sex=="Male"	,1	,0)]
  P1[,	BA36	:=ifelse(	Age==	36	&	Sex=="Male"	,1	,0)]
  P1[,	BA37	:=ifelse(	Age==	37	&	Sex=="Male"	,1	,0)]
  P1[,	BA38	:=ifelse(	Age==	38	&	Sex=="Male"	,1	,0)]
  P1[,	BA39	:=ifelse(	Age==	39	&	Sex=="Male"	,1	,0)]
  P1[,	BA40	:=ifelse(	Age==	40	&	Sex=="Male"	,1	,0)]
  P1[,	BA41	:=ifelse(	Age==	41	&	Sex=="Male"	,1	,0)]
  P1[,	BA42	:=ifelse(	Age==	42	&	Sex=="Male"	,1	,0)]
  P1[,	BA43	:=ifelse(	Age==	43	&	Sex=="Male"	,1	,0)]
  P1[,	BA44	:=ifelse(	Age==	44	&	Sex=="Male"	,1	,0)]
  P1[,	BA45	:=ifelse(	Age==	45	&	Sex=="Male"	,1	,0)]
  P1[,	BA46	:=ifelse(	Age==	46	&	Sex=="Male"	,1	,0)]
  P1[,	BA47	:=ifelse(	Age==	47	&	Sex=="Male"	,1	,0)]
  P1[,	BA48	:=ifelse(	Age==	48	&	Sex=="Male"	,1	,0)]
  P1[,	BA49	:=ifelse(	Age==	49	&	Sex=="Male"	,1	,0)]
  P1[,	BA50	:=ifelse(	Age==	50	&	Sex=="Male"	,1	,0)]
  P1[,	BA51	:=ifelse(	Age==	51	&	Sex=="Male"	,1	,0)]
  P1[,	BA52	:=ifelse(	Age==	52	&	Sex=="Male"	,1	,0)]
  P1[,	BA53	:=ifelse(	Age==	53	&	Sex=="Male"	,1	,0)]
  P1[,	BA54	:=ifelse(	Age==	54	&	Sex=="Male"	,1	,0)]
  P1[,	BA55	:=ifelse(	Age==	55	&	Sex=="Male"	,1	,0)]
  P1[,	BA56	:=ifelse(	Age==	56	&	Sex=="Male"	,1	,0)]
  P1[,	BA57	:=ifelse(	Age==	57	&	Sex=="Male"	,1	,0)]
  P1[,	BA58	:=ifelse(	Age==	58	&	Sex=="Male"	,1	,0)]
  P1[,	BA59	:=ifelse(	Age==	59	&	Sex=="Male"	,1	,0)]
  P1[,	BA60	:=ifelse(	Age==	60	&	Sex=="Male"	,1	,0)]
  P1[,	BA61	:=ifelse(	Age==	61	&	Sex=="Male"	,1	,0)]
  P1[,	BA62	:=ifelse(	Age==	62	&	Sex=="Male"	,1	,0)]
  P1[,	BA63	:=ifelse(	Age==	63	&	Sex=="Male"	,1	,0)]
  P1[,	BA64	:=ifelse(	Age==	64	&	Sex=="Male"	,1	,0)]
  P1[,	BA65	:=ifelse(	Age==	65	&	Sex=="Male"	,1	,0)]
  P1[,	BA66	:=ifelse(	Age==	66	&	Sex=="Male"	,1	,0)]
  P1[,	BA67	:=ifelse(	Age==	67	&	Sex=="Male"	,1	,0)]
  P1[,	BA68	:=ifelse(	Age==	68	&	Sex=="Male"	,1	,0)]
  P1[,	BA69	:=ifelse(	Age==	69	&	Sex=="Male"	,1	,0)]
  P1[,	BA70	:=ifelse(	Age==	70	&	Sex=="Male"	,1	,0)]
  P1[,	BA71	:=ifelse(	Age==	71	&	Sex=="Male"	,1	,0)]
  P1[,	BA72	:=ifelse(	Age==	72	&	Sex=="Male"	,1	,0)]
  P1[,	BA73	:=ifelse(	Age==	73	&	Sex=="Male"	,1	,0)]
  P1[,	BA74	:=ifelse(	Age==	74	&	Sex=="Male"	,1	,0)]
  P1[,	BA75	:=ifelse(	Age==	75	&	Sex=="Male"	,1	,0)]
  P1[,	BA76	:=ifelse(	Age==	76	&	Sex=="Male"	,1	,0)]
  P1[,	BA77	:=ifelse(	Age==	77	&	Sex=="Male"	,1	,0)]
  P1[,	BA78	:=ifelse(	Age==	78	&	Sex=="Male"	,1	,0)]
  P1[,	BA79	:=ifelse(	Age==	79	&	Sex=="Male"	,1	,0)]
  P1[,	BA80	:=ifelse(	Age==	80	&	Sex=="Male"	,1	,0)]
  P1[,	BA81	:=ifelse(	Age==	81	&	Sex=="Male"	,1	,0)]
  P1[,	BA82	:=ifelse(	Age==	82	&	Sex=="Male"	,1	,0)]
  P1[,	BA83	:=ifelse(	Age==	83	&	Sex=="Male"	,1	,0)]
  P1[,	BA84	:=ifelse(	Age==	84	&	Sex=="Male"	,1	,0)]
  P1[,	BA85	:=ifelse(	Age==	85	&	Sex=="Male"	,1	,0)]
  P1[,	BA86	:=ifelse(	Age==	86	&	Sex=="Male"	,1	,0)]
  P1[,	BA87	:=ifelse(	Age==	87	&	Sex=="Male"	,1	,0)]
  P1[,	BA88	:=ifelse(	Age==	88	&	Sex=="Male"	,1	,0)]
  P1[,	BA89	:=ifelse(	Age==	89	&	Sex=="Male"	,1	,0)]
  P1[,	BA90	:=ifelse(	Age==	90	&	Sex=="Male"	,1	,0)]
  P1[,	BA91	:=ifelse(	Age==	91	&	Sex=="Male"	,1	,0)]
  P1[,	BA92	:=ifelse(	Age==	92	&	Sex=="Male"	,1	,0)]
  P1[,	BA93	:=ifelse(	Age==	93	&	Sex=="Male"	,1	,0)]
  P1[,	BA94	:=ifelse(	Age==	94	&	Sex=="Male"	,1	,0)]
  P1[,	BA95	:=ifelse(	Age==	95	&	Sex=="Male"	,1	,0)]
  P1[,	BA96	:=ifelse(	Age==	96	&	Sex=="Male"	,1	,0)]
  P1[,	BA97	:=ifelse(	Age==	97	&	Sex=="Male"	,1	,0)]
  P1[,	BA98	:=ifelse(	Age==	98	&	Sex=="Male"	,1	,0)]
  P1[,	BA99	:=ifelse(	Age==	99	&	Sex=="Male"	,1	,0)]
  P1[,	GA0	:=ifelse(	Age==	0	&	Sex=="Female"	,1	,0)]
  P1[,	GA1	:=ifelse(	Age==	1	&	Sex=="Female"	,1	,0)]
  P1[,	GA2	:=ifelse(	Age==	2	&	Sex=="Female"	,1	,0)]
  P1[,	GA3	:=ifelse(	Age==	3	&	Sex=="Female"	,1	,0)]
  P1[,	GA4	:=ifelse(	Age==	4	&	Sex=="Female"	,1	,0)]
  P1[,	GA5	:=ifelse(	Age==	5	&	Sex=="Female"	,1	,0)]
  P1[,	GA6	:=ifelse(	Age==	6	&	Sex=="Female"	,1	,0)]
  P1[,	GA7	:=ifelse(	Age==	7	&	Sex=="Female"	,1	,0)]
  P1[,	GA8	:=ifelse(	Age==	8	&	Sex=="Female"	,1	,0)]
  P1[,	GA9	:=ifelse(	Age==	9	&	Sex=="Female"	,1	,0)]
  P1[,	GA10	:=ifelse(	Age==	10	&	Sex=="Female"	,1	,0)]
  P1[,	GA11	:=ifelse(	Age==	11	&	Sex=="Female"	,1	,0)]
  P1[,	GA12	:=ifelse(	Age==	12	&	Sex=="Female"	,1	,0)]
  P1[,	GA13	:=ifelse(	Age==	13	&	Sex=="Female"	,1	,0)]
  P1[,	GA14	:=ifelse(	Age==	14	&	Sex=="Female"	,1	,0)]
  P1[,	GA15	:=ifelse(	Age==	15	&	Sex=="Female"	,1	,0)]
  P1[,	GA16	:=ifelse(	Age==	16	&	Sex=="Female"	,1	,0)]
  P1[,	GA17	:=ifelse(	Age==	17	&	Sex=="Female"	,1	,0)]
  P1[,	GA18	:=ifelse(	Age==	18	&	Sex=="Female"	,1	,0)]
  P1[,	GA19	:=ifelse(	Age==	19	&	Sex=="Female"	,1	,0)]
  P1[,	GA20	:=ifelse(	Age==	20	&	Sex=="Female"	,1	,0)]
  P1[,	GA21	:=ifelse(	Age==	21	&	Sex=="Female"	,1	,0)]
  P1[,	GA22	:=ifelse(	Age==	22	&	Sex=="Female"	,1	,0)]
  P1[,	GA23	:=ifelse(	Age==	23	&	Sex=="Female"	,1	,0)]
  P1[,	GA24	:=ifelse(	Age==	24	&	Sex=="Female"	,1	,0)]
  P1[,	GA25	:=ifelse(	Age==	25	&	Sex=="Female"	,1	,0)]
  P1[,	GA26	:=ifelse(	Age==	26	&	Sex=="Female"	,1	,0)]
  P1[,	GA27	:=ifelse(	Age==	27	&	Sex=="Female"	,1	,0)]
  P1[,	GA28	:=ifelse(	Age==	28	&	Sex=="Female"	,1	,0)]
  P1[,	GA29	:=ifelse(	Age==	29	&	Sex=="Female"	,1	,0)]
  P1[,	GA30	:=ifelse(	Age==	30	&	Sex=="Female"	,1	,0)]
  P1[,	GA31	:=ifelse(	Age==	31	&	Sex=="Female"	,1	,0)]
  P1[,	GA32	:=ifelse(	Age==	32	&	Sex=="Female"	,1	,0)]
  P1[,	GA33	:=ifelse(	Age==	33	&	Sex=="Female"	,1	,0)]
  P1[,	GA34	:=ifelse(	Age==	34	&	Sex=="Female"	,1	,0)]
  P1[,	GA35	:=ifelse(	Age==	35	&	Sex=="Female"	,1	,0)]
  P1[,	GA36	:=ifelse(	Age==	36	&	Sex=="Female"	,1	,0)]
  P1[,	GA37	:=ifelse(	Age==	37	&	Sex=="Female"	,1	,0)]
  P1[,	GA38	:=ifelse(	Age==	38	&	Sex=="Female"	,1	,0)]
  P1[,	GA39	:=ifelse(	Age==	39	&	Sex=="Female"	,1	,0)]
  P1[,	GA40	:=ifelse(	Age==	40	&	Sex=="Female"	,1	,0)]
  P1[,	GA41	:=ifelse(	Age==	41	&	Sex=="Female"	,1	,0)]
  P1[,	GA42	:=ifelse(	Age==	42	&	Sex=="Female"	,1	,0)]
  P1[,	GA43	:=ifelse(	Age==	43	&	Sex=="Female"	,1	,0)]
  P1[,	GA44	:=ifelse(	Age==	44	&	Sex=="Female"	,1	,0)]
  P1[,	GA45	:=ifelse(	Age==	45	&	Sex=="Female"	,1	,0)]
  P1[,	GA46	:=ifelse(	Age==	46	&	Sex=="Female"	,1	,0)]
  P1[,	GA47	:=ifelse(	Age==	47	&	Sex=="Female"	,1	,0)]
  P1[,	GA48	:=ifelse(	Age==	48	&	Sex=="Female"	,1	,0)]
  P1[,	GA49	:=ifelse(	Age==	49	&	Sex=="Female"	,1	,0)]
  P1[,	GA50	:=ifelse(	Age==	50	&	Sex=="Female"	,1	,0)]
  P1[,	GA51	:=ifelse(	Age==	51	&	Sex=="Female"	,1	,0)]
  P1[,	GA52	:=ifelse(	Age==	52	&	Sex=="Female"	,1	,0)]
  P1[,	GA53	:=ifelse(	Age==	53	&	Sex=="Female"	,1	,0)]
  P1[,	GA54	:=ifelse(	Age==	54	&	Sex=="Female"	,1	,0)]
  P1[,	GA55	:=ifelse(	Age==	55	&	Sex=="Female"	,1	,0)]
  P1[,	GA56	:=ifelse(	Age==	56	&	Sex=="Female"	,1	,0)]
  P1[,	GA57	:=ifelse(	Age==	57	&	Sex=="Female"	,1	,0)]
  P1[,	GA58	:=ifelse(	Age==	58	&	Sex=="Female"	,1	,0)]
  P1[,	GA59	:=ifelse(	Age==	59	&	Sex=="Female"	,1	,0)]
  P1[,	GA60	:=ifelse(	Age==	60	&	Sex=="Female"	,1	,0)]
  P1[,	GA61	:=ifelse(	Age==	61	&	Sex=="Female"	,1	,0)]
  P1[,	GA62	:=ifelse(	Age==	62	&	Sex=="Female"	,1	,0)]
  P1[,	GA63	:=ifelse(	Age==	63	&	Sex=="Female"	,1	,0)]
  P1[,	GA64	:=ifelse(	Age==	64	&	Sex=="Female"	,1	,0)]
  P1[,	GA65	:=ifelse(	Age==	65	&	Sex=="Female"	,1	,0)]
  P1[,	GA66	:=ifelse(	Age==	66	&	Sex=="Female"	,1	,0)]
  P1[,	GA67	:=ifelse(	Age==	67	&	Sex=="Female"	,1	,0)]
  P1[,	GA68	:=ifelse(	Age==	68	&	Sex=="Female"	,1	,0)]
  P1[,	GA69	:=ifelse(	Age==	69	&	Sex=="Female"	,1	,0)]
  P1[,	GA70	:=ifelse(	Age==	70	&	Sex=="Female"	,1	,0)]
  P1[,	GA71	:=ifelse(	Age==	71	&	Sex=="Female"	,1	,0)]
  P1[,	GA72	:=ifelse(	Age==	72	&	Sex=="Female"	,1	,0)]
  P1[,	GA73	:=ifelse(	Age==	73	&	Sex=="Female"	,1	,0)]
  P1[,	GA74	:=ifelse(	Age==	74	&	Sex=="Female"	,1	,0)]
  P1[,	GA75	:=ifelse(	Age==	75	&	Sex=="Female"	,1	,0)]
  P1[,	GA76	:=ifelse(	Age==	76	&	Sex=="Female"	,1	,0)]
  P1[,	GA77	:=ifelse(	Age==	77	&	Sex=="Female"	,1	,0)]
  P1[,	GA78	:=ifelse(	Age==	78	&	Sex=="Female"	,1	,0)]
  P1[,	GA79	:=ifelse(	Age==	79	&	Sex=="Female"	,1	,0)]
  P1[,	GA80	:=ifelse(	Age==	80	&	Sex=="Female"	,1	,0)]
  P1[,	GA81	:=ifelse(	Age==	81	&	Sex=="Female"	,1	,0)]
  P1[,	GA82	:=ifelse(	Age==	82	&	Sex=="Female"	,1	,0)]
  P1[,	GA83	:=ifelse(	Age==	83	&	Sex=="Female"	,1	,0)]
  P1[,	GA84	:=ifelse(	Age==	84	&	Sex=="Female"	,1	,0)]
  P1[,	GA85	:=ifelse(	Age==	85	&	Sex=="Female"	,1	,0)]
  P1[,	GA86	:=ifelse(	Age==	86	&	Sex=="Female"	,1	,0)]
  P1[,	GA87	:=ifelse(	Age==	87	&	Sex=="Female"	,1	,0)]
  P1[,	GA88	:=ifelse(	Age==	88	&	Sex=="Female"	,1	,0)]
  P1[,	GA89	:=ifelse(	Age==	89	&	Sex=="Female"	,1	,0)]
  P1[,	GA90	:=ifelse(	Age==	90	&	Sex=="Female"	,1	,0)]
  P1[,	GA91	:=ifelse(	Age==	91	&	Sex=="Female"	,1	,0)]
  P1[,	GA92	:=ifelse(	Age==	92	&	Sex=="Female"	,1	,0)]
  P1[,	GA93	:=ifelse(	Age==	93	&	Sex=="Female"	,1	,0)]
  P1[,	GA94	:=ifelse(	Age==	94	&	Sex=="Female"	,1	,0)]
  P1[,	GA95	:=ifelse(	Age==	95	&	Sex=="Female"	,1	,0)]
  P1[,	GA96	:=ifelse(	Age==	96	&	Sex=="Female"	,1	,0)]
  P1[,	GA97	:=ifelse(	Age==	97	&	Sex=="Female"	,1	,0)]
  P1[,	GA98	:=ifelse(	Age==	98	&	Sex=="Female"	,1	,0)]
  P1[,	GA99	:=ifelse(	Age==	99	&	Sex=="Female"	,1	,0)]
  
  
  
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
           weighted.mean(	BA0	,Weight)*Settings$	KCaloryNeed_A_B0	+
           weighted.mean(	BA1	,Weight)*Settings$	KCaloryNeed_A_B1	+
           weighted.mean(	BA2	,Weight)*Settings$	KCaloryNeed_A_B2	+
           weighted.mean(	BA3	,Weight)*Settings$	KCaloryNeed_A_B3	+
           weighted.mean(	BA4	,Weight)*Settings$	KCaloryNeed_A_B4	+
           weighted.mean(	BA5	,Weight)*Settings$	KCaloryNeed_A_B5	+
           weighted.mean(	BA6	,Weight)*Settings$	KCaloryNeed_A_B6	+
           weighted.mean(	BA7	,Weight)*Settings$	KCaloryNeed_A_B7	+
           weighted.mean(	BA8	,Weight)*Settings$	KCaloryNeed_A_B8	+
           weighted.mean(	BA9	,Weight)*Settings$	KCaloryNeed_A_B9	+
           weighted.mean(	BA10	,Weight)*Settings$	KCaloryNeed_A_B10	+
           weighted.mean(	BA11	,Weight)*Settings$	KCaloryNeed_A_B11	+
           weighted.mean(	BA12	,Weight)*Settings$	KCaloryNeed_A_B12	+
           weighted.mean(	BA13	,Weight)*Settings$	KCaloryNeed_A_B13	+
           weighted.mean(	BA14	,Weight)*Settings$	KCaloryNeed_A_B14	+
           weighted.mean(	BA15	,Weight)*Settings$	KCaloryNeed_A_B15	+
           weighted.mean(	BA16	,Weight)*Settings$	KCaloryNeed_A_B16	+
           weighted.mean(	BA17	,Weight)*Settings$	KCaloryNeed_A_B17	+
           weighted.mean(	BA18	,Weight)*Settings$	KCaloryNeed_A_B18	+
           weighted.mean(	BA19	,Weight)*Settings$	KCaloryNeed_A_B19	+
           weighted.mean(	BA20	,Weight)*Settings$	KCaloryNeed_A_B20	+
           weighted.mean(	BA21	,Weight)*Settings$	KCaloryNeed_A_B21	+
           weighted.mean(	BA22	,Weight)*Settings$	KCaloryNeed_A_B22	+
           weighted.mean(	BA23	,Weight)*Settings$	KCaloryNeed_A_B23	+
           weighted.mean(	BA24	,Weight)*Settings$	KCaloryNeed_A_B24	+
           weighted.mean(	BA25	,Weight)*Settings$	KCaloryNeed_A_B25	+
           weighted.mean(	BA26	,Weight)*Settings$	KCaloryNeed_A_B26	+
           weighted.mean(	BA27	,Weight)*Settings$	KCaloryNeed_A_B27	+
           weighted.mean(	BA28	,Weight)*Settings$	KCaloryNeed_A_B28	+
           weighted.mean(	BA29	,Weight)*Settings$	KCaloryNeed_A_B29	+
           weighted.mean(	BA30	,Weight)*Settings$	KCaloryNeed_A_B30	+
           weighted.mean(	BA31	,Weight)*Settings$	KCaloryNeed_A_B31	+
           weighted.mean(	BA32	,Weight)*Settings$	KCaloryNeed_A_B32	+
           weighted.mean(	BA33	,Weight)*Settings$	KCaloryNeed_A_B33	+
           weighted.mean(	BA34	,Weight)*Settings$	KCaloryNeed_A_B34	+
           weighted.mean(	BA35	,Weight)*Settings$	KCaloryNeed_A_B35	+
           weighted.mean(	BA36	,Weight)*Settings$	KCaloryNeed_A_B36	+
           weighted.mean(	BA37	,Weight)*Settings$	KCaloryNeed_A_B37	+
           weighted.mean(	BA38	,Weight)*Settings$	KCaloryNeed_A_B38	+
           weighted.mean(	BA39	,Weight)*Settings$	KCaloryNeed_A_B39	+
           weighted.mean(	BA40	,Weight)*Settings$	KCaloryNeed_A_B40	+
           weighted.mean(	BA41	,Weight)*Settings$	KCaloryNeed_A_B41	+
           weighted.mean(	BA42	,Weight)*Settings$	KCaloryNeed_A_B42	+
           weighted.mean(	BA43	,Weight)*Settings$	KCaloryNeed_A_B43	+
           weighted.mean(	BA44	,Weight)*Settings$	KCaloryNeed_A_B44	+
           weighted.mean(	BA45	,Weight)*Settings$	KCaloryNeed_A_B45	+
           weighted.mean(	BA46	,Weight)*Settings$	KCaloryNeed_A_B46	+
           weighted.mean(	BA47	,Weight)*Settings$	KCaloryNeed_A_B47	+
           weighted.mean(	BA48	,Weight)*Settings$	KCaloryNeed_A_B48	+
           weighted.mean(	BA49	,Weight)*Settings$	KCaloryNeed_A_B49	+
           weighted.mean(	BA50	,Weight)*Settings$	KCaloryNeed_A_B50	+
           weighted.mean(	BA51	,Weight)*Settings$	KCaloryNeed_A_B51	+
           weighted.mean(	BA52	,Weight)*Settings$	KCaloryNeed_A_B52	+
           weighted.mean(	BA53	,Weight)*Settings$	KCaloryNeed_A_B53	+
           weighted.mean(	BA54	,Weight)*Settings$	KCaloryNeed_A_B54	+
           weighted.mean(	BA55	,Weight)*Settings$	KCaloryNeed_A_B55	+
           weighted.mean(	BA56	,Weight)*Settings$	KCaloryNeed_A_B56	+
           weighted.mean(	BA57	,Weight)*Settings$	KCaloryNeed_A_B57	+
           weighted.mean(	BA58	,Weight)*Settings$	KCaloryNeed_A_B58	+
           weighted.mean(	BA59	,Weight)*Settings$	KCaloryNeed_A_B59	+
           weighted.mean(	BA60	,Weight)*Settings$	KCaloryNeed_A_B60	+
           weighted.mean(	BA61	,Weight)*Settings$	KCaloryNeed_A_B61	+
           weighted.mean(	BA62	,Weight)*Settings$	KCaloryNeed_A_B62	+
           weighted.mean(	BA63	,Weight)*Settings$	KCaloryNeed_A_B63	+
           weighted.mean(	BA64	,Weight)*Settings$	KCaloryNeed_A_B64	+
           weighted.mean(	BA65	,Weight)*Settings$	KCaloryNeed_A_B65	+
           weighted.mean(	BA66	,Weight)*Settings$	KCaloryNeed_A_B66	+
           weighted.mean(	BA67	,Weight)*Settings$	KCaloryNeed_A_B67	+
           weighted.mean(	BA68	,Weight)*Settings$	KCaloryNeed_A_B68	+
           weighted.mean(	BA69	,Weight)*Settings$	KCaloryNeed_A_B69	+
           weighted.mean(	BA70	,Weight)*Settings$	KCaloryNeed_A_B70	+
           weighted.mean(	BA71	,Weight)*Settings$	KCaloryNeed_A_B71	+
           weighted.mean(	BA72	,Weight)*Settings$	KCaloryNeed_A_B72	+
           weighted.mean(	BA73	,Weight)*Settings$	KCaloryNeed_A_B73	+
           weighted.mean(	BA74	,Weight)*Settings$	KCaloryNeed_A_B74	+
           weighted.mean(	BA75	,Weight)*Settings$	KCaloryNeed_A_B75	+
           weighted.mean(	BA76	,Weight)*Settings$	KCaloryNeed_A_B76	+
           weighted.mean(	BA77	,Weight)*Settings$	KCaloryNeed_A_B77	+
           weighted.mean(	BA78	,Weight)*Settings$	KCaloryNeed_A_B78	+
           weighted.mean(	BA79	,Weight)*Settings$	KCaloryNeed_A_B79	+
           weighted.mean(	BA80	,Weight)*Settings$	KCaloryNeed_A_B80	+
           weighted.mean(	BA81	,Weight)*Settings$	KCaloryNeed_A_B81	+
           weighted.mean(	BA82	,Weight)*Settings$	KCaloryNeed_A_B82	+
           weighted.mean(	BA83	,Weight)*Settings$	KCaloryNeed_A_B83	+
           weighted.mean(	BA84	,Weight)*Settings$	KCaloryNeed_A_B84	+
           weighted.mean(	BA85	,Weight)*Settings$	KCaloryNeed_A_B85	+
           weighted.mean(	BA86	,Weight)*Settings$	KCaloryNeed_A_B86	+
           weighted.mean(	BA87	,Weight)*Settings$	KCaloryNeed_A_B87	+
           weighted.mean(	BA88	,Weight)*Settings$	KCaloryNeed_A_B88	+
           weighted.mean(	BA89	,Weight)*Settings$	KCaloryNeed_A_B89	+
           weighted.mean(	BA90	,Weight)*Settings$	KCaloryNeed_A_B90	+
           weighted.mean(	BA91	,Weight)*Settings$	KCaloryNeed_A_B91	+
           weighted.mean(	BA92	,Weight)*Settings$	KCaloryNeed_A_B92	+
           weighted.mean(	BA93	,Weight)*Settings$	KCaloryNeed_A_B93	+
           weighted.mean(	BA94	,Weight)*Settings$	KCaloryNeed_A_B94	+
           weighted.mean(	BA95	,Weight)*Settings$	KCaloryNeed_A_B95	+
           weighted.mean(	BA96	,Weight)*Settings$	KCaloryNeed_A_B96	+
           weighted.mean(	BA97	,Weight)*Settings$	KCaloryNeed_A_B97	+
           weighted.mean(	BA98	,Weight)*Settings$	KCaloryNeed_A_B98	+
           weighted.mean(	BA99	,Weight)*Settings$	KCaloryNeed_A_B99	+
           weighted.mean(	GA0	,Weight)*Settings$	KCaloryNeed_A_G0	+
           weighted.mean(	GA1	,Weight)*Settings$	KCaloryNeed_A_G1	+
           weighted.mean(	GA2	,Weight)*Settings$	KCaloryNeed_A_G2	+
           weighted.mean(	GA3	,Weight)*Settings$	KCaloryNeed_A_G3	+
           weighted.mean(	GA4	,Weight)*Settings$	KCaloryNeed_A_G4	+
           weighted.mean(	GA5	,Weight)*Settings$	KCaloryNeed_A_G5	+
           weighted.mean(	GA6	,Weight)*Settings$	KCaloryNeed_A_G6	+
           weighted.mean(	GA7	,Weight)*Settings$	KCaloryNeed_A_G7	+
           weighted.mean(	GA8	,Weight)*Settings$	KCaloryNeed_A_G8	+
           weighted.mean(	GA9	,Weight)*Settings$	KCaloryNeed_A_G9	+
           weighted.mean(	GA10	,Weight)*Settings$	KCaloryNeed_A_G10	+
           weighted.mean(	GA11	,Weight)*Settings$	KCaloryNeed_A_G11	+
           weighted.mean(	GA12	,Weight)*Settings$	KCaloryNeed_A_G12	+
           weighted.mean(	GA13	,Weight)*Settings$	KCaloryNeed_A_G13	+
           weighted.mean(	GA14	,Weight)*Settings$	KCaloryNeed_A_G14	+
           weighted.mean(	GA15	,Weight)*Settings$	KCaloryNeed_A_G15	+
           weighted.mean(	GA16	,Weight)*Settings$	KCaloryNeed_A_G16	+
           weighted.mean(	GA17	,Weight)*Settings$	KCaloryNeed_A_G17	+
           weighted.mean(	GA18	,Weight)*Settings$	KCaloryNeed_A_G18	+
           weighted.mean(	GA19	,Weight)*Settings$	KCaloryNeed_A_G19	+
           weighted.mean(	GA20	,Weight)*Settings$	KCaloryNeed_A_G20	+
           weighted.mean(	GA21	,Weight)*Settings$	KCaloryNeed_A_G21	+
           weighted.mean(	GA22	,Weight)*Settings$	KCaloryNeed_A_G22	+
           weighted.mean(	GA23	,Weight)*Settings$	KCaloryNeed_A_G23	+
           weighted.mean(	GA24	,Weight)*Settings$	KCaloryNeed_A_G24	+
           weighted.mean(	GA25	,Weight)*Settings$	KCaloryNeed_A_G25	+
           weighted.mean(	GA26	,Weight)*Settings$	KCaloryNeed_A_G26	+
           weighted.mean(	GA27	,Weight)*Settings$	KCaloryNeed_A_G27	+
           weighted.mean(	GA28	,Weight)*Settings$	KCaloryNeed_A_G28	+
           weighted.mean(	GA29	,Weight)*Settings$	KCaloryNeed_A_G29	+
           weighted.mean(	GA30	,Weight)*Settings$	KCaloryNeed_A_G30	+
           weighted.mean(	GA31	,Weight)*Settings$	KCaloryNeed_A_G31	+
           weighted.mean(	GA32	,Weight)*Settings$	KCaloryNeed_A_G32	+
           weighted.mean(	GA33	,Weight)*Settings$	KCaloryNeed_A_G33	+
           weighted.mean(	GA34	,Weight)*Settings$	KCaloryNeed_A_G34	+
           weighted.mean(	GA35	,Weight)*Settings$	KCaloryNeed_A_G35	+
           weighted.mean(	GA36	,Weight)*Settings$	KCaloryNeed_A_G36	+
           weighted.mean(	GA37	,Weight)*Settings$	KCaloryNeed_A_G37	+
           weighted.mean(	GA38	,Weight)*Settings$	KCaloryNeed_A_G38	+
           weighted.mean(	GA39	,Weight)*Settings$	KCaloryNeed_A_G39	+
           weighted.mean(	GA40	,Weight)*Settings$	KCaloryNeed_A_G40	+
           weighted.mean(	GA41	,Weight)*Settings$	KCaloryNeed_A_G41	+
           weighted.mean(	GA42	,Weight)*Settings$	KCaloryNeed_A_G42	+
           weighted.mean(	GA43	,Weight)*Settings$	KCaloryNeed_A_G43	+
           weighted.mean(	GA44	,Weight)*Settings$	KCaloryNeed_A_G44	+
           weighted.mean(	GA45	,Weight)*Settings$	KCaloryNeed_A_G45	+
           weighted.mean(	GA46	,Weight)*Settings$	KCaloryNeed_A_G46	+
           weighted.mean(	GA47	,Weight)*Settings$	KCaloryNeed_A_G47	+
           weighted.mean(	GA48	,Weight)*Settings$	KCaloryNeed_A_G48	+
           weighted.mean(	GA49	,Weight)*Settings$	KCaloryNeed_A_G49	+
           weighted.mean(	GA50	,Weight)*Settings$	KCaloryNeed_A_G50	+
           weighted.mean(	GA51	,Weight)*Settings$	KCaloryNeed_A_G51	+
           weighted.mean(	GA52	,Weight)*Settings$	KCaloryNeed_A_G52	+
           weighted.mean(	GA53	,Weight)*Settings$	KCaloryNeed_A_G53	+
           weighted.mean(	GA54	,Weight)*Settings$	KCaloryNeed_A_G54	+
           weighted.mean(	GA55	,Weight)*Settings$	KCaloryNeed_A_G55	+
           weighted.mean(	GA56	,Weight)*Settings$	KCaloryNeed_A_G56	+
           weighted.mean(	GA57	,Weight)*Settings$	KCaloryNeed_A_G57	+
           weighted.mean(	GA58	,Weight)*Settings$	KCaloryNeed_A_G58	+
           weighted.mean(	GA59	,Weight)*Settings$	KCaloryNeed_A_G59	+
           weighted.mean(	GA60	,Weight)*Settings$	KCaloryNeed_A_G60	+
           weighted.mean(	GA61	,Weight)*Settings$	KCaloryNeed_A_G61	+
           weighted.mean(	GA62	,Weight)*Settings$	KCaloryNeed_A_G62	+
           weighted.mean(	GA63	,Weight)*Settings$	KCaloryNeed_A_G63	+
           weighted.mean(	GA64	,Weight)*Settings$	KCaloryNeed_A_G64	+
           weighted.mean(	GA65	,Weight)*Settings$	KCaloryNeed_A_G65	+
           weighted.mean(	GA66	,Weight)*Settings$	KCaloryNeed_A_G66	+
           weighted.mean(	GA67	,Weight)*Settings$	KCaloryNeed_A_G67	+
           weighted.mean(	GA68	,Weight)*Settings$	KCaloryNeed_A_G68	+
           weighted.mean(	GA69	,Weight)*Settings$	KCaloryNeed_A_G69	+
           weighted.mean(	GA70	,Weight)*Settings$	KCaloryNeed_A_G70	+
           weighted.mean(	GA71	,Weight)*Settings$	KCaloryNeed_A_G71	+
           weighted.mean(	GA72	,Weight)*Settings$	KCaloryNeed_A_G72	+
           weighted.mean(	GA73	,Weight)*Settings$	KCaloryNeed_A_G73	+
           weighted.mean(	GA74	,Weight)*Settings$	KCaloryNeed_A_G74	+
           weighted.mean(	GA75	,Weight)*Settings$	KCaloryNeed_A_G75	+
           weighted.mean(	GA76	,Weight)*Settings$	KCaloryNeed_A_G76	+
           weighted.mean(	GA77	,Weight)*Settings$	KCaloryNeed_A_G77	+
           weighted.mean(	GA78	,Weight)*Settings$	KCaloryNeed_A_G78	+
           weighted.mean(	GA79	,Weight)*Settings$	KCaloryNeed_A_G79	+
           weighted.mean(	GA80	,Weight)*Settings$	KCaloryNeed_A_G80	+
           weighted.mean(	GA81	,Weight)*Settings$	KCaloryNeed_A_G81	+
           weighted.mean(	GA82	,Weight)*Settings$	KCaloryNeed_A_G82	+
           weighted.mean(	GA83	,Weight)*Settings$	KCaloryNeed_A_G83	+
           weighted.mean(	GA84	,Weight)*Settings$	KCaloryNeed_A_G84	+
           weighted.mean(	GA85	,Weight)*Settings$	KCaloryNeed_A_G85	+
           weighted.mean(	GA86	,Weight)*Settings$	KCaloryNeed_A_G86	+
           weighted.mean(	GA87	,Weight)*Settings$	KCaloryNeed_A_G87	+
           weighted.mean(	GA88	,Weight)*Settings$	KCaloryNeed_A_G88	+
           weighted.mean(	GA89	,Weight)*Settings$	KCaloryNeed_A_G89	+
           weighted.mean(	GA90	,Weight)*Settings$	KCaloryNeed_A_G90	+
           weighted.mean(	GA91	,Weight)*Settings$	KCaloryNeed_A_G91	+
           weighted.mean(	GA92	,Weight)*Settings$	KCaloryNeed_A_G92	+
           weighted.mean(	GA93	,Weight)*Settings$	KCaloryNeed_A_G93	+
           weighted.mean(	GA94	,Weight)*Settings$	KCaloryNeed_A_G94	+
           weighted.mean(	GA95	,Weight)*Settings$	KCaloryNeed_A_G95	+
           weighted.mean(	GA96	,Weight)*Settings$	KCaloryNeed_A_G96	+
           weighted.mean(	GA97	,Weight)*Settings$	KCaloryNeed_A_G97	+
           weighted.mean(	GA98	,Weight)*Settings$	KCaloryNeed_A_G98	+
           weighted.mean(	GA99	,Weight)*Settings$	KCaloryNeed_A_G99	+
           weighted.mean(lactating,Weight)*Settings$KCaloryNeed_lactating]
  
  cat(P1[,mean(Calorie_Need_WorldBank)],"\n")
  cat(P1[,mean(Calorie_Need_Anstitoo)],"\n")
  
  Calorie_Need<-P1[,.(Calorie_Need_WorldBank=mean(Calorie_Need_WorldBank),
                      Calorie_Need_Anstitoo=mean(Calorie_Need_Anstitoo)),by="HHID"]
  
  save(P1,file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
  save(Calorie_Need,file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)