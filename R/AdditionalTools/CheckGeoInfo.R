rm(list=ls())

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(readxl)

shs <- excel_sheets(Settings$GeoInfoFilePath)
D <- data.table()
for(sh in shs[-1:-6]){
  D0 <- data.table(read_excel(path = Settings$GeoInfoFilePath, sheet = sh))
  D0 <- D0[,.(Geo4,CountyFa)]
  setnames(D0,"CountyFa",paste0("CountyFa",sh))
  if(nrow(D)==0){
    D <- copy(D0)
  }else{
    D <- merge(D,D0,by="Geo4",all = TRUE)
  }
}

E <- data.table()
for(year in 77:Settings$endyear){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"GeoInfo.rda"))
  E0 <- GeoInfo[,.N,by=Geo4][order(Geo4)]
  setnames(E0,"N",paste0("N",year))
  if(nrow(E)==0){
    E <- copy(E0)
  }else{
    E <- merge(E,E0,by="Geo4",all = TRUE)
  }
}

rm(D0,E0,sh,shs,year,GeoInfo)

G <- merge(D[,.(Geo4,CountyFa81,CountyFa96)],E)
#View(G)
ucc <- G[is.na(CountyFa96),Geo4]

Old <- D[Geo4 %in% ucc]
Old <- Old[order(Geo4)]
for(i in 1:nrow(Old)){
  oc <- Old[i,Geo4]
  s <- Old[i,CountyFa76]
  if(is.na(s))
    s <- Old[i,CountyFa81]
  if(is.na(s))
    s <- Old[i,CountyFa82]
  nc <- G[CountyFa96==s,Geo4]
  cat("\n",oc," : ",nc," # ", s)
}


FE <- data.table()
for(year in 77:Settings$endyear){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"GeoInfo.rda"))
  FE0 <- GeoInfo[,.N,by=FGeo4]
  setnames(FE0,"N",paste0("N",year))
  if(nrow(FE)==0){
    FE <- copy(FE0)
  }else{
    FE <- merge(FE,FE0,by="FGeo4",all = TRUE)
  }
}
FE <- FE[FGeo4!="2201"]
sapply(FE[,-1],function(X){length(X[!is.na(X)])})

FE[is.na(N81) & !is.na(N80)]
FE[is.na(N86) & !is.na(N85)]

View(FE[FGeo4 %in% c("0910","2110","2911")])
View(E[Geo4 %in% c("0910","2110","2911")])


for(year in 77:Settings$endyear){
  print(year)
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"GeoInfo.rda"))
  A <- GeoInfo[Geo2=="29",.N,by=FGeo4]
  print(A)
}
