# 152-Subsidy.R
# 2019: Arin Shahbazian

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ HHSubsidy =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)


SubsidyTable <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_SubsidyWage))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  Subsidywt <- SubsidyTable[Year==year]
  tab <- Subsidywt$Table
  if(is.na(tab))
    next
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  UTSubsidyW <- Tables[[paste0("U",year,tab)]]
  RTSubsidyW <- Tables[[paste0("R",year,tab)]]
  TSubsidyW <- rbind(UTSubsidyW,RTSubsidyW,fill=TRUE)
  for(n in names(TSubsidyW)){
    x <- which(Subsidywt==n)
    if(length(x)>0)
      setnames(TSubsidyW,n,names(Subsidywt)[x])
  }
  pcols <- intersect(names(TSubsidyW),c("HHID","dimension","Subsidy"))
  TSubsidyW <- TSubsidyW[,pcols,with=FALSE]
  
  
  
  TSubsidyW[is.na(TSubsidyW)] <- 0
  SubsidyWageData <- TSubsidyW[,lapply(.SD,sum),by=HHID]
  save(SubsidyWageData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Subsidy.rda"))
  

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  UTSubsidyW[,check1:=Dycol05/(Dycol04*Dycol03)]
  UMD<-MD[Region=="Urban"]
  UMD<-UMD[,.(HHID,FinalPoor,Decile,Decile_Nominal,Month)]
  names(UMD) <- c("Address", "FinalPoor","Decile","Decile_Nominal","Month")
  UTSubsidyW<-merge(UTSubsidyW,UMD)
  
  RTSubsidyW[,check1:=Dycol05/(Dycol04*Dycol03)]
  RMD<-MD[Region=="Rural"]
  RMD<-RMD[,.(HHID,FinalPoor,Decile,Decile_Nominal,Month)]
  names(RMD) <- c("Address", "FinalPoor","Decile","Decile_Nominal","Month")
  RTSubsidyW<-merge(RTSubsidyW,RMD)
  
  xU<-UTSubsidyW[check1>1000000]
  xR<-RTSubsidyW[check1>1000000]
  
  save(xU,file = "xU.rda")
  save(xR,file = "xR.rda")
}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
