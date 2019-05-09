# 110-Education Expenditures.R
# 2019: Arin Shahbazian

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Education Expenditures =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)

EducationTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Education))

#Introduce Groups
PrimaryschoolReg<-101111:101115
HighschoolReg<-102111:102215
PreUniversityReg<-103111:103116
UniversityReg<-104111:104115
OtherClassReg<-105111:105115

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,","))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- EducationTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTEducation <- Tables[[paste0("U",year,tab)]]
  RTEducation <- Tables[[paste0("R",year,tab)]]
  TEducations1 <- rbind(UTEducation,RTEducation)
  for(n in names(TEducations1)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TEducations1,n,names(ft)[x])
  }
  
  
 # cat(" PrimaryschoolReg, ")
  
  TEducations <- TEducations1[Code %in% PrimaryschoolReg]
  TEducations[,a:=NULL]
  TEducations[,b:=NULL]
  TEducations[,c:=NULL]
  TEducations[,d:=NULL]
  TEducations[,Code:=NULL]
  TEducations[is.na(TEducations)] <- 0
  names(TEducations) <- c("HHID", "PrimaryschoolRegExp")
  TEducations[,PrimaryschoolRegExp:=as.numeric(PrimaryschoolRegExp)/12]
  PrimaryschoolRegExp <- TEducations[,lapply(.SD,sum),by=HHID]
  save(PrimaryschoolRegExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"PrimaryschoolRegExp.rda"))
  
  #cat(" HighschoolReg, ")
  
  TEducations <- TEducations1[Code %in% HighschoolReg]
  TEducations[,a:=NULL]
  TEducations[,b:=NULL]
  TEducations[,c:=NULL]
  TEducations[,d:=NULL]
  TEducations[,Code:=NULL]
  TEducations[is.na(TEducations)] <- 0
  names(TEducations) <- c("HHID", "HighschoolRegExp")
  TEducations[,HighschoolRegExp:=as.numeric(HighschoolRegExp)/12]
  HighschoolRegExp <- TEducations[,lapply(.SD,sum),by=HHID]
  save(HighschoolRegExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"HighschoolRegExp.rda"))
  
  
  
  #cat(" PreUniversityReg, ")
  
  TEducations <- TEducations1[Code %in% PreUniversityReg]
  TEducations[,a:=NULL]
  TEducations[,b:=NULL]
  TEducations[,c:=NULL]
  TEducations[,d:=NULL]
  TEducations[,Code:=NULL]
  TEducations[is.na(TEducations)] <- 0
  names(TEducations) <- c("HHID", "PreUniversityRegExp")
  TEducations[,PreUniversityRegExp:=as.numeric(PreUniversityRegExp)/12]
  PreUniversityRegExp <- TEducations[,lapply(.SD,sum),by=HHID]
  save(PreUniversityRegExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"PreUniversityRegExp.rda"))
  
  #cat(" UniversityReg, ")
  
  TEducations <- TEducations1[Code %in% UniversityReg]
  TEducations[,a:=NULL]
  TEducations[,b:=NULL]
  TEducations[,c:=NULL]
  TEducations[,d:=NULL]
  TEducations[,Code:=NULL]
  TEducations[is.na(TEducations)] <- 0
  names(TEducations) <- c("HHID", "UniversityRegExp")
  TEducations[,UniversityRegExp:=as.numeric(UniversityRegExp)/12]
  UniversityRegExp <- TEducations[,lapply(.SD,sum),by=HHID]
  save(UniversityRegExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"UniversityRegExp.rda"))
  
  #cat(" OtherClassReg, ")
  
  TEducations <- TEducations1[Code %in% OtherClassReg]
  TEducations[,a:=NULL]
  TEducations[,b:=NULL]
  TEducations[,c:=NULL]
  TEducations[,d:=NULL]
  TEducations[,Code:=NULL]
  TEducations[is.na(TEducations)] <- 0
  names(TEducations) <- c("HHID", "OtherClassRegExp")
  TEducations[,OtherClassRegExp:=as.numeric(OtherClassRegExp)/12]
  OtherClassRegExp <- TEducations[,lapply(.SD,sum),by=HHID]
  save(OtherClassRegExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"OtherClassRegExp.rda"))
  
  
  #merge Education Consumption
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Education.rda"))
  EducationExp<-merge(EducData,PrimaryschoolRegExp,all = TRUE)
  EducationExp<-merge(EducationExp,HighschoolRegExp,all = TRUE)
  EducationExp<-merge(EducationExp,PreUniversityRegExp,all = TRUE)
  EducationExp<-merge(EducationExp,UniversityRegExp,all = TRUE)
  EducationExp<-merge(EducationExp,OtherClassRegExp,all = TRUE)
  
  EducationExp[is.na(EducationExp)] <- 0
  
  cat(EducationExp[,mean(EducExpenditure,na.rm=TRUE)])
  
  save(EducationExp, file = paste0(Settings$HEISProcessedPath,"Y",year,"EducationExp.rda"))
}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
