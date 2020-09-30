rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

for(year in max(77,Settings$startyear):86){   # 77 i the first year with county data
  cat("\nYear",year)
  load(file=paste0(Settings$HEISProcessedPath,
                   "Y",year,"HHBase.rda"))
  GeoInfo <- copy(HHBase[,.(HHID,
                            Geo2=substr(sprintf("%09d",HHID),2,3),
                            Geo4=substr(sprintf("%09d",HHID),2,5))])
  if(year %in% 84:86){
    GeoInfo[Geo4=="2824", Geo4:="2803"] # Jaram
    GeoInfo[Geo4=="2809", Geo4:="2804"] # Shirvan
    GeoInfo[Geo4=="2813", Geo4:="2805"] # Faruj     # Guess!
    GeoInfo[Geo4=="2825", Geo4:="2806"] # Mane & Semelqan

    GeoInfo[Geo4=="2903", Geo4:="2901"] # Birjand
    GeoInfo[Geo4=="2912", Geo4:="2906"] # Sarbishe  # Just a guess
    GeoInfo[Geo4=="2921", Geo4:="2905"] # Nehbandan
    GeoInfo[Geo4=="2911", Geo4:="2903"] # Serayan   # Just a guess
    #GeoInfo[Geo4=="0911", Geo4:="2907"] # Ferdows
  }
  
  cat(":\t",length(unique(GeoInfo$Geo2)))
  cat(":\t",GeoInfo[order(Geo2),Geo2][c(1,nrow(GeoInfo))])
  cat(":\t",length(unique(GeoInfo$Geo4)))
  cat(":\t",GeoInfo[order(Geo4),Geo4][c(1,nrow(GeoInfo))])
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
  
}

for(year in 87:91){
  cat("\nYear",year)
  DR <- data.table(read_excel(paste0("C:/Users/Majid/Dropbox/HEIS/HEIS-Share/DataSummary/13",
                                     year,"/SumR",year,".xlsx")))
  DU <- data.table(read_excel(paste0("C:/Users/Majid/Dropbox/HEIS/HEIS-Share/DataSummary/13",
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

for(year in 92:max(92,Settings$endyear)){
  cat("\nYear",year)
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  GeoInfo <- HHBase[,.(HHID,
                       Geo2=substr(HHID,2,3),
                       Geo4=substr(HHID,2,5))]
  # if(year %in% 95:96) GeoInfo[Geo4=="2909", Geo4:="2911"] # Tabas (3) # This is just a guess!
  if(year >=97) cat("Check this line for, What is Tabas code for >=97?")
  cat(":\t",length(unique(GeoInfo$Geo2)))
  cat(":\t",GeoInfo[order(Geo2),Geo2][c(1,nrow(GeoInfo))])
  cat(":\t",length(unique(GeoInfo$Geo4)))
  cat(":\t",GeoInfo[order(Geo4),Geo4][c(1,nrow(GeoInfo))])
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
}


for(year in  max(77,Settings$startyear):Settings$endyear){
  cat("\nYear",year)
  load(file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
  GeoInfo[,FGeo4:=Geo4]
  
  GeoInfo[Geo4=="0901", FGeo4:="2801"] # Esfarayen
  GeoInfo[Geo4=="0902", FGeo4:="2802"] # Bojnourd
  GeoInfo[Geo4=="0903", FGeo4:="2901"] # Birjand
  GeoInfo[Geo4=="0909", FGeo4:="2804"] # Shirvan
  GeoInfo[Geo4=="0910", FGeo4:="2911"] # Tabas (1)
  GeoInfo[Geo4=="0911", FGeo4:="2907"] # Ferdows
  GeoInfo[Geo4=="0912", FGeo4:="2904"] # Qaenat
  GeoInfo[Geo4=="0921", FGeo4:="2905"] # Nehbandan
  GeoInfo[Geo4=="0924", FGeo4:="2803"] # Jajarm
  GeoInfo[Geo4=="0925", FGeo4:="2806"] # Mane & Semelqan
  GeoInfo[Geo4=="2110", FGeo4:="2911"] # Tabas (2)
  GeoInfo[Geo4=="2305", FGeo4:="3001"] # Karaj
  GeoInfo[Geo4=="2308", FGeo4:="3002"] # Savojbolaq
  GeoInfo[Geo4=="2315", FGeo4:="3003"] # Nazarabad
  
  GeoInfo[,FGeo2:=substr(FGeo4,1,2)]
  
  cat(":\t",length(unique(GeoInfo$FGeo2)))
  cat(":\t",length(unique(GeoInfo$FGeo4)))
  
  save(GeoInfo, file=paste0(Settings$HEISProcessedPath,
                            "Y",year,"GeoInfo.rda"))
}
  
