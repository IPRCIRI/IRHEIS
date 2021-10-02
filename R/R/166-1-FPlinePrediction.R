# 166-Step6-FoodPoor.R: Calculate base year basket cost in current year prices 
#                  for each cluster. and find FoodPoor
#
# Copyright © 2019-2020: Arin Shahbazian & Majid Einian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(spatstat)
library(data.table)
library(dplyr)
library(tidyr)
library(writexl)

BigsdTable <- data.table()
year<-Settings$baseBundleyear

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
PriceCodes<-as.data.table(read_excel(Settings$PriceCodesFilePath,Settings$PCS))

MD[,Selected_Group:=ifelse((Region=="Urban" & Decile==4) |
                             (Region=="Rural" & Decile==3),1,0)]

Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
Bfd2 <- merge(Bfd2,MD[,.(HHID,Region,Weight,Size,
                         EqSizeCalory,Selected_Group)],by="HHID")
Bfd2[is.na(Bfd2)]<-0
Bfd2[Price<0.1,Price:=NA]

BaseYearBasket <- Bfd2[Selected_Group==1,
                       .(FGramspc=weighted.mean(FGrams/EqSizeCalory,Weight*Size),
                         FKCalspc=weighted.mean(FoodKCalories/EqSizeCalory,Weight*Size)),
                       by=.(FoodType,Region)]
BaseYearBasket[,BasketCals:=sum(FKCalspc),by=Region]
BaseYearBasket[,StandardFGramspc:=FGramspc*Settings$KCaloryNeed_Adult_WorldBank/BasketCals]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  for (month in (Settings$startmonth:Settings$endmonth)) {
    
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  
  MD[,Selected_Group:=ifelse((Decile==5) | (Decile==2) |
                               (Decile==3) | (Decile==4),1,0)]
  ######################
      year<-year+1
    cat(paste0("\nmonth:",month,"\t"))
    
    #l <- dir(path=Settings$CPIPath,pattern=glob2rx(paste0("Y",year,"M",month,".*")),ignore.case = TRUE)
    #Price_month <- as.data.table(read_excel(path=paste0(Settings$CPIPath, l)))
    
    c <- dir(path=Settings$CPIPath,pattern=glob2rx(paste0("CPIY",year,"M",month,".*")),ignore.case = TRUE)
    CPI_month <- as.data.table(read_excel(path=paste0(Settings$CPIPath, c)))
  
    year<-year-1
    c <- dir(path=Settings$CPIPath,pattern=glob2rx(paste0("CPIY",year,".*")),ignore.case = TRUE)
    CPI_month_lag <- as.data.table(read_excel(path=paste0(Settings$CPIPath, c)))
    #Price_month <- Price_month %>% gather(ProvinceCode,Price, 2:32)
    
    #Price_month <- as.data.table(Price_month)
    #Price_month <- Price_month[,ProvinceCode:=as.numeric(ProvinceCode)]
    #Price_month <- Price_month[Price==0,Price:=NA]
    
    BigFData<-BigFData[,code:=FoodCode]
    BigFData<-merge(BigFData,PriceCodes,by=c("code"),all.x = TRUE)
    Province<-MD[,.(HHID,ProvinceCode)]
    BigFData<-merge(BigFData,Province,by=c("HHID"),all.x = TRUE)
    BigFData<-BigFData[,Price_O:=Price]
    BigFData<-BigFData[,Price:=NULL]
    
    #BigFData<-merge(BigFData,Price_month,by=c("Pcode1","ProvinceCode"),all.x = TRUE)
    #BigFData<-BigFData[,Price1:=Price]
    #BigFData<-BigFData[,Price:=NULL]
    
    #Price_month <- Price_month[,Pcode2:=Pcode1]
    #BigFData<-merge(BigFData,Price_month[,.(Pcode2,ProvinceCode,Price)],by=c("Pcode2","ProvinceCode"),all.x = TRUE)
  #  BigFData<-BigFData[,Price2:=Price]
   # BigFData<-BigFData[,Price:=NULL]
    
    #Price_month <- Price_month[,Pcode3:=Pcode1]
    #BigFData<-merge(BigFData,Price_month[,.(Pcode3,ProvinceCode,Price)],by=c("Pcode3","ProvinceCode"),all.x = TRUE)
    #BigFData<-BigFData[,Price3:=Price]
    #BigFData<-BigFData[,Price:=NULL]
    
    #for (col in c("Pcode1","Pcode2","Pcode3"))
     # BigFData[is.na(get(col)), (col) := 0]
    #BigFData$mean <- rowMeans(subset(BigFData, select = c("Price1","Price2","Price3")), na.rm = TRUE)
    #BigFData<-BigFData[(Pcode1>0 | Pcode2>0 | Pcode3>0) ,Price := mean]  
    
    
    CPI_month1<-CPI_month/CPI_month_lag  
    CPI_month1<-CPI_month1[,CPIcode:=NULL]
    CPI_month1<-cbind(CPI_month[,.(CPIcode)],CPI_month1)
    CPI_month1 <- CPI_month1 %>% gather(ProvinceCode,CPI, 2:32)
    CPI_month1 <- as.data.table(CPI_month1)
    CPI_month1 <- CPI_month1[,ProvinceCode:=as.numeric(ProvinceCode)]
    CPI_month1 <- CPI_month1[CPI==0,CPI:=NA]
    BigFData<-merge(BigFData,CPI_month1,by=c("CPIcode","ProvinceCode"),all.x = TRUE)
    BigFData<-BigFData[,Price:=Price_O*CPI]
    ######################
    #Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
    #Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
    Bfd2 <- merge(BigFData,MD[,.(HHID,Region,Weight,Size,cluster3,
                                 EqSizeCalory,Selected_Group)],by="HHID")
    Bfd2[is.na(Bfd2)]<-0
    Bfd2[Price<0.1,Price:=NA]
    Bfd3 <- Bfd2[Selected_Group==1 & !is.na(Price),
                 .(MedPrice=weighted.median(Price,Weight*Size*FGrams,na.rm = TRUE),
                   MeanPrice=weighted.mean(Price,Weight*Size*FGrams,na.rm = TRUE)),
                 by=.(FoodType,Region,cluster3)]
    BasketPrice <- Bfd3[!is.na(MeanPrice),
                        .(Price=min(MeanPrice)),
                        by=.(FoodType,Region,cluster3)]
    
    BasketCost <- merge(BaseYearBasket,BasketPrice,by=c("FoodType","Region"))
    BasketCost[,Cost:=(StandardFGramspc/1000)*Price]
    FPLineBasket <- BasketCost[,.(FPLine=sum(Cost)),by=cluster3]
    FPLineBasket<-FPLineBasket[,Year:=year]
    FPLineBasket<-FPLineBasket[,Month:=month]
    MD <- merge(MD,FPLineBasket,all.x=TRUE,by="cluster3")
    sd <- MD
    sd <- sd[,FPLineBasketyear:=weighted.mean(FPLine,Weight)]
    sd <-sd[,Year:=year+1]
    sd <-sd[,Month:=month]
    sd <- unique(sd[,.(Year,Month,FPLineBasketyear)])
    BigsdTable <- rbind(BigsdTable,sd)
    
    
    MD[,FoodPoor:=ifelse(TOriginalFoodExpenditure_Per < FPLine,1,0)]
    
    cat(unlist(sd[Year==year+1 & Month==month]))
    
 if (year==Settings$startyear & month==Settings$startmonth){
   ClusterFPline<-FPLineBasket
   CountryFPLine<-sd
   
   }else{
   ClusterFPline<-rbind(ClusterFPline,FPLineBasket)
   CountryFPLine<-rbind(CountryFPLine,sd)
   }
    save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"M",month,"FoodPoor.rda"))
    
   }
  
  



}

write_xlsx(CountryFPLine,"E:/FPLinePrediction.xlsx")

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")
