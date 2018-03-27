# 22-Food Groups.R
# Builds the Food Groups data.table for households
#
# Copyright © 2018: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)
cat("\n\n================ HHBerenjs =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
BerenjTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Berenj))
for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- BerenjTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$BerenjGram<-TF$Kilos*1000+TF$Grams
    TF$BerenjGram<- TF$BerenjGram/30
    BerenjData <- TF[,lapply(.SD,sum),by=HHID]
    save(BerenjData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Berenjs.rda"))
  }
}
cat("\n\n================ HHGhands =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  
  GhandTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Ghand))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- GhandTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$GhandGram<-TF$Kilos*1000+TF$Grams
    TF$GhandGram<- TF$GhandGram/30
    GhandData <- TF[,lapply(.SD,sum),by=HHID]
    save(GhandData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Ghands.rda"))
  }
}

cat("\n\n================ HHGooshts =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
GooshtTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Goosht))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ft <- GooshtTables[Year==year]
  tab <- ft$Table
  if(is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
  TF <- TF[,pcols,with=FALSE]
  TF <- TF[Code %in% ft$StartCode:ft$EndCode]
  if(year %in% 84:94){
    TF[,Kilos:=as.numeric(Kilos)]
    TF[,Grams:=as.numeric(Grams)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF$GooshtGram<-TF$Kilos*1000+TF$Grams
  TF$GooshtGram<- TF$GooshtGram/30
  GooshtData <- TF[,lapply(.SD,sum),by=HHID]
  save(GooshtData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Gooshts.rda"))
}
}
cat("\n\n================ HHHoboobats =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  
  HoboobatTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Hoboobat))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- HoboobatTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Kilos:=as.numeric(Kilos)]
      TF[,Grams:=as.numeric(Grams)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$HoboobatGram<-TF$Kilos*1000+TF$Grams
    TF$HoboobatGram<- TF$HoboobatGram/30
    HoboobatData <- TF[,lapply(.SD,sum),by=HHID]
    save(HoboobatData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Hoboobats.rda"))
  }
} 
cat("\n\n================ HHMahis =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  MahiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mahi))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- MahiTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$MahiGram<-TF$Kilos*1000+TF$Grams
    TF$MahiGram<- TF$MahiGram/30
    MahiData <- TF[,lapply(.SD,sum),by=HHID]
    save(MahiData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Mahis.rda"))
  }
}
cat("\n\n================ HHMakaroonis =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  MakarooniTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Makarooni))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- MakarooniTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$MakarooniGram<-TF$Kilos*1000+TF$Grams
    TF$MakarooniGram<- TF$MakarooniGram/30
    MakarooniData <- TF[,lapply(.SD,sum),by=HHID]
    save(MakarooniData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Makaroonis.rda"))
  } 
}   
cat("\n\n================ HHMasts =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  MastTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mast))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- MastTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$MastGram<-TF$Kilos*1000+TF$Grams
    TF$MastGram<- TF$MastGram/30
    MastData <- TF[,lapply(.SD,sum),by=HHID]
    save(MastData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Masts.rda"))
  }  
}
cat("\n\n================ HHMives =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  MiveTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Mive))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- MiveTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$MiveGram<-TF$Kilos*1000+TF$Grams
    TF$MiveGram<- TF$MiveGram/30
    MiveData <- TF[,lapply(.SD,sum),by=HHID]
    save(MiveData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Mives.rda"))
  }
}   
cat("\n\n================ HHMorghs =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  MorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Morgh))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- MorghTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$MorghGram<-TF$Kilos*1000+TF$Grams
    TF$MorghGram<- TF$MorghGram/30
    MorghData <- TF[,lapply(.SD,sum),by=HHID]
    save(MorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Morghs.rda"))
  }
}     
cat("\n\n================ HHNans =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  NanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Nan))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- NanTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$NanGram<-TF$Kilos*1000+TF$Grams
    TF$NanGram<- TF$NanGram/30
    NanData <- TF[,lapply(.SD,sum),by=HHID]
    save(NanData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Nans.rda"))
  }       
}        
cat("\n\n================ HHPanirs =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  PanirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Panir))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- PanirTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$PanirGram<-TF$Kilos*1000+TF$Grams
    TF$PanirGram<- TF$PanirGram/30
    PanirData <- TF[,lapply(.SD,sum),by=HHID]
    save(PanirData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Panirs.rda"))
  }        
}         
cat("\n\n================ Roghans =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  RoghanTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Roghan))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- RoghanTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$RoghanGram<-TF$Kilos*1000+TF$Grams
    TF$RoghanGram<- TF$RoghanGram/30
    RoghanData <- TF[,lapply(.SD,sum),by=HHID]
    save(RoghanData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Roghans.rda"))
  }
}  
cat("\n\n================ HHSabzis =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  SabziTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sabzi))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- SabziTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$SabziGram<-TF$Kilos*1000+TF$Grams
    TF$SabziGram<- TF$SabziGram/30
    SabziData <- TF[,lapply(.SD,sum),by=HHID]
    save(SabziData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sabzis.rda"))
  }
}    
cat("\n\n================ HHShirs =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  ShirTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Shir))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- ShirTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$ShirGram<-TF$Kilos*1000+TF$Grams
    TF$ShirGram<- TF$ShirGram/30
    ShirData <- TF[,lapply(.SD,sum),by=HHID]
    save(ShirData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Shirs.rda"))
  }   
}       
cat("\n\n================ HHSibzaminis =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  SibzaminiTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Sibzamini))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- SibzaminiTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$SibzaminiGram<-TF$Kilos*1000+TF$Grams
    TF$SibzaminiGram<- TF$SibzaminiGram/30
    SibzaminiData <- TF[,lapply(.SD,sum),by=HHID]
    save(SibzaminiData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Sibzaminis.rda"))
  }       
}         
cat("\n\n================ HHTokhmemorghs =====================================\n")
for(year in (Settings$startyear:Settings$endyear)){
  TokhmemorghTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Tokhmemorgh))
  
  
  
  for(year in (Settings$startyear:Settings$endyear)){
    cat(paste0("\n------------------------------\nYear:",year,"\n"))
    load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
    ft <- TokhmemorghTables[Year==year]
    tab <- ft$Table
    if(is.na(tab))
      next
    UTF <- Tables[[paste0("U",year,tab)]]
    RTF <- Tables[[paste0("R",year,tab)]]
    TF <- rbind(UTF,RTF)
    for(n in names(TF)){
      x <- which(ft==n)
      if(length(x)>0)
        setnames(TF,n,names(ft)[x])
    }
    pcols <- intersect(names(TF),c("HHID","Code","Grams","Kilos"))
    TF <- TF[,pcols,with=FALSE]
    TF <- TF[Code %in% ft$StartCode:ft$EndCode]
    if(year %in% 84:94){
      TF[,Grams:=as.numeric(Grams)]
      TF[,Kilos:=as.numeric(Kilos)]
    }
    
    TF[,Code:=NULL]
    TF[is.na(TF)] <- 0
    TF$TokhmemorghGram<-TF$Kilos*1000+TF$Grams
    TF$TokhmemorghGram<- TF$TokhmemorghGram/30
    TokhmemorghData <- TF[,lapply(.SD,sum),by=HHID]
    save(TokhmemorghData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Tokhmemorghs.rda"))
  }     

}
cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat(endtime-starttime)