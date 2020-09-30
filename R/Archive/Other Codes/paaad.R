rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Admin Area Analysis Data =================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(data.table)

load(file = "D:/HEIS/DataProcessed/ProvincialPriceData.rda")
PriceData[,Year:=Year-1300]
PriceData[is.na(P01),P01:=CPI]
PriceData[is.na(P02),P02:=CPI]
PriceData[is.na(P03),P03:=CPI]
PriceData[is.na(P04),P04:=CPI]
PriceData[is.na(P05),P05:=CPI]
PriceData[is.na(P06),P06:=CPI]
PriceData[is.na(P07),P07:=CPI]
PriceData[is.na(P08),P08:=CPI]
PriceData[is.na(P09),P09:=CPI]
PriceData[is.na(P10),P10:=CPI]
PriceData[is.na(P11),P11:=CPI]
PriceData[is.na(P12),P12:=CPI]
year <- 77

BigD <- data.table()
for(year in 77:96){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"GeoInfo.rda"))
  D <- merge(MD,GeoInfo)
  D <- merge(D,PriceData,by=c("Year", "Geo2"))
  
  D[,RX01:=FoodExpenditure/P01*100]  
  D[,RX02:=Cigar_Exp/P02*100]  
  D[,RX03:=Cloth_Exp/P03*100]  
  D[,RX04:=(ServiceExp+Energy_Exp)/P04*100]  
  D[,RX05:=Furniture_Exp/P05*100]  
  D[,RX06:=Behdasht_Exp/P06*100]   
  D[,RX06x:=Medical_Exp/P06*100]  
  D[,RX07:=Transportation_Exp/P07*100]  
  D[,RX08:=Communication_Exp/P08*100]  
  D[,RX09:=Amusement_Exp/P09*100]  
  D[,RX10:=EducExpenditure/P10*100]  
  D[,RX11:=Hotel_Exp/P11*100]  
  D[,RX12:=Other_Exp/P12*100]  
  D[,RXD:=Durable_Exp/CPI*100]
  D[,RXND:=RX01+RX02+RX03+RX04+RX05+RX06+
      RX07+RX08+RX09+RX10+RX11+RX12]
  D[,RXT:=RXD+RXND+RX06x]
  D[,RXND2:=Total_Exp_Month_nondurable/CPI*100]
  D[,RXT2:=Total_Exp_Month/CPI*100]
  
  D0 <- D[,.(Year,HHID,Region,Quarter,Geo2,Geo4,FGeo2,FGeo4,
             HAge,Size,EqSizeOECD,Weight,
             RXND,RXD,RXT)]
  D0[,YoB := 1300+Year-HAge]
  
  BigD <- rbind(BigD,D0)
}

library(readxl)

rm(D,D0,MD,GeoInfo,PriceData,year)


BigD[,Cohort1:=cut(YoB,breaks=seq(from=1277,to=1390,by=1),
                labels=paste(seq(from=1278,to=1390,by=1)))]
BigD[,Cohort2:=cut(YoB,breaks=seq(from=1277,to=1389,by=2),
                labels=paste(seq(from=1278,to=1388,by=2),"-",
                             sprintf("%02d",seq(from=1279,to=1389,by=2) %% 100),sep=""))]
BigD[,Cohort5:=cut(YoB,breaks=seq(from=1275,to=1390,by=5),
                labels=paste(seq(from=1276,to=1386,by=5),"-",
                             sprintf("%02d",seq(from=1280,to=1390,by=5) %% 100),sep=""))]
BigD[,Cohort10:=cut(YoB,breaks=seq(from=1275,to=1385,by=10),
                 labels=paste(seq(from=1276,to=1376,by=10),"-",
                              sprintf("%02d",seq(from=1285,to=1385,by=10) %% 100),sep=""))]
BigD[,Cohort20:=cut(YoB,breaks=seq(from=1275,to=1395,by=20),
                    labels=paste(seq(from=1276,to=1376,by=20),"-",
                                 sprintf("%02d",seq(from=1295,to=1395,by=20) %% 100),sep=""))]



BigD[,RXNDpc:=RXND/EqSizeOECD]
BigD[,RXDpc:=RXD/EqSizeOECD]
BigD[,RXTpc:=RXT/EqSizeOECD]

BigD[,Geo4:=factor(Geo4)]
Geo4D <- data.table(read_excel("../Data/Geo4.xlsx","Changed"))

BigD <- merge(BigD,Geo4D[,.(Geo4,IGeo4)],by = "Geo4",all.x = TRUE)
BigD[is.na(IGeo4),IGeo4:=Geo4]

save(BigD,file = "D:/HEIS/DataProcessed/BigD.rda")
