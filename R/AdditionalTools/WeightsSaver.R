rm(list = ls())

cat("\n\n================ WeightsSaver =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(RODBC)
library(data.table)
library(stringr)
library(tcltk)
#library(XLConnect)
library(readxl)
library(tools)
flist <- tk_choose.files()
starttime <- proc.time()

HHWeights <- data.table(HHID=NA_integer_,Year=NA_integer_,Weight=NA_real_)[0]

for(f in flist){
  year <- as.numeric(str_extract_all(f, "[0-9]+")[[1]])
  year <- year[length(year)]
  s <- file_path_sans_ext(f)
  RU <- substr(s,nchar(s)-2,nchar(s)-2)
  sh <- excel_sheets(f)
  
  if(year==76){
    Query <-"Select R_U, OSTAN, BLOCK, KHANEVAR, weight from ["
  }else if(year %in% 77:82){
    Query <-  "Select R_U, OSTAN, SHAHRESTAN, BLOCK, KHANEVAR, weight from ["
  }else if(year %in% 88:89){
    Query <- "Select ADDRESS, Weight from ["
  }else{
    Query <- "Select ADDRESS, weight from ["
  }
  
  Query <- paste0(Query,sh,"$]")
  cnx <- odbcConnectExcel2007(f)
  mt <- data.table(sqlQuery(cnx,query=Query))
  close(cnx)

  rm(cnx)
  
  if(year %in% 63:75){
    mt[,HHID:=as.integer(paste(ZONE,OSTAN,HOUSEHOLD,sep=""))]
  }else if(year==76){
    mt[,HHID:=as.integer(paste(R_U,sprintf("%02d", OSTAN), KHANEVAR %/% 10000,
                                      sprintf("%03d",KHANEVAR %% 1000),sep=""))]
  }else if(year %in% 77:82){
    mt[,HHID:=as.integer(paste(R_U,sprintf("%02d", OSTAN), sprintf("%02d", SHAHRESTAN),KHANEVAR %/% 10000,
                                      sprintf("%03d",KHANEVAR %% 1000),sep=""))]
  }else{
    setnames(mt,"ADDRESS","HHID")
  }

  mt[,Year:=year]
  if("weight" %in% names(mt))
    setnames(mt,"weight","Weight")
  setkey(mt,HHID,Year)

  mt <- mt[,.(HHID,Year,Weight)]
  HHWeights <- rbind(HHWeights,mt)
  if(min(HHWeights$Year)!=year || max(HHWeights$Year)!=year)
    warning("More than one year in data!")
}

save(HHWeights,file = paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])
cat(" seconds. ")