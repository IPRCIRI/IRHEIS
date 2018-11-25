rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in 77:86){
  cat("\nYear",year)
  load(file=paste0(Settings$HEISProcessedPath,
                   "Y",year,"HHBase.rda"))
  GeoInfo <- copy(HHBase[,.(HHID,
                            Geo2=substr(sprintf("%09d",HHID),2,3),
                            Geo4=substr(sprintf("%09d",HHID),2,5))])
  cat(":\t",length(unique(GeoInfo$Geo2)))
  cat(":\t",GeoInfo[order(Geo2),Geo2][c(1,nrow(GeoInfo))])
  cat(":\t",length(unique(GeoInfo$Geo4)))
  cat(":\t",GeoInfo[order(Geo4),Geo4][c(1,nrow(GeoInfo))])
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
}

for(year in 87:91){
  cat("\nYear",year)
  DR <- data.table(read_excel(paste0("D:/Dropbox/HEIS/DataSummary/13",
                                     year,"/SumR",year,".xlsx")))
  DU <- data.table(read_excel(paste0("D:/Dropbox/HEIS/DataSummary/13",
                                     year,"/SumU",year,".xlsx")))
  s <- "ADDRESS"
  if("shr" %in% names(DR))
    s <- c(s,"shr")
  if("Shr" %in% names(DR))
    s <- c(s,"Shr")
  if("Adr85_19" %in% names(DR))
    s <- c(s,"Adr85_19")
  if("adr85_19" %in% names(DR))
    s <- c(s,"adr85_19")
  
  D <- rbind(DR[,..s],DU[,..s])
  setnames(D,"ADDRESS","HHID")
  rm(DR,DU)
  D[,Geo2:=substr(HHID,2,3)]
  
  if("shr" %in% names(D))
    setnames(D,"shr","Shr")
   if("adr85_19" %in% names(D))
    setnames(D,"adr85_19","Adr85_19")
  if("Shr" %in% names(D)){
    D[,Shr:=sprintf("%02d",as.integer(Shr))]
    D[,Geo4:=paste0(Geo2,Shr)]
  }else{
    D[,Geo4:=substr(Adr85_19,1,4)]
  }
  D[,HHID:=as.numeric(HHID)]

  D[Geo2=="30" & Geo4=="2305", Geo4:="3001"]
  D[Geo2=="30" & Geo4=="2308", Geo4:="3002"]
  D[Geo2=="30" & Geo4=="2315", Geo4:="3003"]
  
  GeoInfo <- D[,.(HHID,Geo2,Geo4)]
  cat(":\t",length(unique(GeoInfo$Geo2)))
  cat(":\t",GeoInfo[order(Geo2),Geo2][c(1,nrow(GeoInfo))])
  cat(":\t",length(unique(GeoInfo$Geo4)))
  cat(":\t",GeoInfo[order(Geo4),Geo4][c(1,nrow(GeoInfo))])
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
}

for(year in 92:Settings$endyear){
  cat("\nYear",year)
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  GeoInfo <- HHBase[,.(HHID,
                       Geo2=substr(HHID,2,3),
                       Geo4=substr(HHID,2,5))]
  cat(":\t",length(unique(GeoInfo$Geo2)))
  cat(":\t",GeoInfo[order(Geo2),Geo2][c(1,nrow(GeoInfo))])
  cat(":\t",length(unique(GeoInfo$Geo4)))
  cat(":\t",GeoInfo[order(Geo4),Geo4][c(1,nrow(GeoInfo))])
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
}