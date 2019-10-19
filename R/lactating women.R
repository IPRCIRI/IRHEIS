#lactating women.R
#build a column that identifies lactating women

#zahra shahidi
#2019

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ lactating women =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(stringr)

P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))



for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  FoodTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Food))
  
  
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
  P1 <- P1[, lapply(.SD, f)] 
  
  
  P1[is.na(Age)]
  P1[,Relationship :=factor(Relationship, levels=1:9, 
                            labels=c("Head","Spouse","Child","Child-in-Law",
                                     "Grand-Child","Parent","Sister/Brother",
                                     "Other Family","Non-Family"))]
  
  P1[,Sex := factor(Sex, levels=1:2,
                    labels=c("Male","Female"))]
  
  
  
  P <- copy(P1)
  
  P <- P[order(P$HHID),]
  
  B <- P[IndivNo==1]
  setnames(B,2:length(B),sapply(X=names(B)[2:length(B)],function(X){paste("H",X,sep="")}))
  B <- B[order(HHID,HIndivNo)]
  B <- B[!duplicated(B$HHID),]
  
  
  P[,Size:=1]
  P[,NKids:=ifelse(Age<18,1,0)]
  
  P[,NInfants:=ifelse(Age<=2,1,0)]
  P[,NInfants0:=ifelse(Age==0,1,0)]
  P[,NSmallKids:=ifelse(Age>=3 & Age<=13, 1, 0)]
  
  
  PSum <- P[,lapply(.SD,sum,na.rm=TRUE),
            .SDcols=c("Size","NInfants0"),
            by="HHID"]
  
  
  HHI <- merge(B,PSum,by="HHID")
  
  
  
  ft <- FoodTables[Year == year]
  tab <- ft$Table
  if (is.na(tab))
    next
  UTF <- Tables[[paste0("U",year,tab)]]
  RTF <- Tables[[paste0("R",year,tab)]]
  TF <- rbind(UTF,RTF)
  for(n in names(TF)){
    x <- which(ft==n)
    if(length(x)>0)
      setnames(TF,n,names(ft)[x])
  }
  pcols <- intersect(names(TF),c("HHID","Code","FoodExpenditure"))
  TF <- TF[,pcols,with=FALSE]
  if(year %in% 83:97){
    TF <- TF[Code ==11413]
    TF[,FoodExpenditure:=as.numeric(FoodExpenditure)]
  }else if(year %in% 69:82){
    TF <- TF[Code ==13137]
    TF[,FoodExpenditure:=as.numeric(FoodExpenditure)]
  }
  
  TF[,Code:=NULL]
  TF[is.na(TF)] <- 0
  TF <- TF[,lapply(.SD,sum),by=HHID]
  TFL<-merge(HHI,TF,by="HHID", all.x = T)
  for (col in c("FoodExpenditure")) 
    TFL[is.na(get(col)), (col) := 0]
  TFL<-TFL[,lactating:=ifelse(NInfants0>0&FoodExpenditure==0,1,0)]
  lactating<-TFL[,.(HHID,lactating)]
  
  save(lactating,file = paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
} 
x<-lactating[lactating==1]
lactating[,mean(lactating)]

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
