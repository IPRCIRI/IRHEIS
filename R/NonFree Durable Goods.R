#NonFree Durable Goods

rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)



cat("\n\n================ NonFree Durable Goods=====================================\n")
DurableTables <- data.table(read_excel(Settings$MetaDataFilePath,sheet=Settings$MDS_Durable))


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))
  ct <- DurableTables[Year==year]
  tab <- ct$Table
  if(is.na(tab))
    next
  UTC <- Tables[[paste0("U",year,tab)]]
  RTC <- Tables[[paste0("R",year,tab)]]
  TC <- rbind(UTC,RTC)
  for(n in names(TC)){
    x <- which(ct==n)
    if(length(x)>0)
      setnames(TC,n,names(ct)[x])
  }
  pcols <- intersect(names(TC),c("HHID","Code","BuyingMethod","Durable_Exp","Durable_Sale"))
  TC <- TC[,pcols,with=FALSE]
  TC<-TC[BuyingMethod!=8]
  if(year %in% 84:97){
    TC[,NonFreeDurable_Exp:=as.numeric(Durable_Exp)]
    TC[,NonFreeDurable_Sale:=as.numeric(Durable_Sale)]
  }
  TC$NonFreeDurable_Exp<-TC$Durable_Exp/12
  TC$NonFreeDurable_Sale<-TC$Durable_Sale/12
  TC[,Code:=NULL]
  TC[,BuyingMethod:=NULL]
  TC[is.na(TC)] <- 0
  TC[,NonFreeDurable_Pure_Exp:=NonFreeDurable_Exp-NonFreeDurable_Sale]
  NonFreeDurableData <- TC[,lapply(.SD,sum),by=HHID]
  save(NonFreeDurableData, file = paste0(Settings$HEISProcessedPath,"Y",year,"NonFreeDurableData.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  MD[ProvinceCode==0,ProvinceName:="Markazi"]
  MD[ProvinceCode==1,ProvinceName:="Gilan"]
  MD[ProvinceCode==2,ProvinceName:="Mazandaran"]
  MD[ProvinceCode==3,ProvinceName:="Az_Sharghi"]
  MD[ProvinceCode==4,ProvinceName:="Az_Gharbi"]
  MD[ProvinceCode==5,ProvinceName:="Kermanshah"]
  MD[ProvinceCode==6,ProvinceName:="Khoozestan"]
  MD[ProvinceCode==7,ProvinceName:="Fars"]
  MD[ProvinceCode==8,ProvinceName:="Kerman"]
  MD[ProvinceCode==9,ProvinceName:="Khorasan_Razavi"]
  MD[ProvinceCode==10,ProvinceName:="Esfahan"]
  MD[ProvinceCode==11,ProvinceName:="Sistan"]
  MD[ProvinceCode==12,ProvinceName:="Kordestan"]
  MD[ProvinceCode==13,ProvinceName:="Hamedan"]
  MD[ProvinceCode==14,ProvinceName:="Chaharmahal"]
  MD[ProvinceCode==15,ProvinceName:="Lorestan"]
  MD[ProvinceCode==16,ProvinceName:="Ilam"]
  MD[ProvinceCode==17,ProvinceName:="Kohkilooye"]
  MD[ProvinceCode==18,ProvinceName:="Booshehr"]
  MD[ProvinceCode==19,ProvinceName:="Zanjan"]
  MD[ProvinceCode==20,ProvinceName:="Semnan"]
  MD[ProvinceCode==21,ProvinceName:="Yazd"]
  MD[ProvinceCode==22,ProvinceName:="Hormozgan"]
  MD[ProvinceCode==23,ProvinceName:="Tehran"]
  MD[ProvinceCode==24,ProvinceName:="Ardebil"]
  MD[ProvinceCode==25,ProvinceName:="Ghom"]
  MD[ProvinceCode==26,ProvinceName:="Ghazvin"]
  MD[ProvinceCode==27,ProvinceName:="Golestan"]
  MD[ProvinceCode==28,ProvinceName:="Khorasan_Shomali"]
  MD[ProvinceCode==29,ProvinceName:="Khorasan_Jonoobi"]
  MD[ProvinceCode==30,ProvinceName:="Alborz"]
  

  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"FreeDurableData.rda"))
  FreeDurableData<-FreeDurableData[,.(HHID,FreeDurable_Exp)]
  NonFreeDurableData<-NonFreeDurableData[,.(HHID,NonFreeDurable_Exp)]
  
  NonFreeDurableData<-merge(NonFreeDurableData,FreeDurableData,all = TRUE)
  NonFreeDurableData<-merge(NonFreeDurableData,MD[,.(HHID,Region,ProvinceName,Weight)])
  NonFreeDurableData[is.na(NonFreeDurableData)] <- 0 
  
  A<-NonFreeDurableData[FreeDurable_Exp>0,.(.N),by=ProvinceName]

  D<-NonFreeDurableData[,.(FreeDurable_Exp=weighted.mean(FreeDurable_Exp,Weight)),by=ProvinceName]
  E<-NonFreeDurableData[,.(NonFreeDurable_Exp=weighted.mean(NonFreeDurable_Exp,Weight)),by=ProvinceName]
  
  A<-E
  
  A$ProvinceName <- factor(A$ProvinceName, levels = A$ProvinceName[order(A$NonFreeDurable_Exp)])
  ggplot(A, aes(x = A$ProvinceName, y = A$NonFreeDurable_Exp)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
}




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
