rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(spatstat)
library(data.table)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  #inflation <- read_excel("~/GitHub/IRHEIS/Data/inflation.xlsx")
  inflation <- read_excel("inflation.xlsx")

  
  Bfd2 <- data.table(expand.grid(HHID=MD$HHID,FoodType=unique(BigFData$FoodType)))
  Bfd2 <- merge(Bfd2,BigFData,all.x = TRUE)
  Bfd2 <- merge(Bfd2,MD[,.(HHID,Region,Weight,Size,
                           EqSizeCalory)],by="HHID")
  Bfd2[is.na(Bfd2)]<-0
  Bfd2[Price<0.1,Price:=NA]
  
  MD<-MD[order(TFoodKCaloriesHH_Per)]
  C<-as.data.table(MD[,weighted.median(TFoodKCaloriesHH_Per,Weight*Size)])
  C<-C[,Year:=year]
  
  Bfd2<-Bfd2[FoodType=="Meat",.(HHID,FoodType,FGrams,EqSizeCalory,Weight,Size)]
  Bfd2 <- Bfd2[,lapply(.SD,sum),by=HHID,.SDcols=c("FGrams")]
  Bfd2<-merge(MD[,.(HHID,Size,EqSizeCalory,Weight)],Bfd2,all.x=TRUE,by="HHID")
  Bfd2<-Bfd2[order(FGrams)]
  Meat<-as.data.table(Bfd2[,weighted.median(FGrams/EqSizeCalory,Weight*Size)])
  Meat<-Meat[,Year:=year]
  
  Bfd2 <- Bfd2[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  Bfd2 <- Bfd2[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  BaseYearBasket <-as.data.table(Bfd2[,weighted.mean(FGrams/EqSizeCalory,Weight*Size)])
  #BaseYearBasket <-as.data.table(Bfd2[,weighted.mean(FGrams/EqSizeCalory,Weight*Size)])
  BaseYearBasket<-BaseYearBasket[,Year:=year]
  Calory<-MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("Decile")]
  Calory<-Calory[,Year:=year]
  
  MD<-MD[order(Total_Exp_Month_Per_nondurable)]
  MD <- MD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  MD <- MD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  EXP<-as.data.table(MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight)])
  #EXP<-as.data.table(MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight)])
  EXP<-EXP[,Year:=year]
  
  
  MD[,Decile:=NULL]
  
  MD<-MD[order(TFoodKCaloriesHH_Per)]
  MD <- MD[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  
  MD <- MD[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  Calory<-as.data.table(MD[,weighted.mean(TFoodKCaloriesHH_Per)])
  #Calory<-as.data.table(MD[,weighted.mean(FoodKCaloriesHH_Per)])
  Calory<-Calory[,Year:=year]
  
  MD<-MD[order(Total_Exp_Month_Per_nondurable)]
  MedianEXP<-as.data.table(MD[,weighted.median(Total_Exp_Month_Per_nondurable,Weight*Size)])
  MedianEXP<-MedianEXP[,Year:=year]
 # if (year==90){
 #   BASKET<-BaseYearBasket
 #   CALORY<-Calory
 #   Cal<-C
 #   EXPenditure<-EXP
 ##   MEAT<-Meat
 #   medEXP<-MedianEXP
#  }else{
    #medEXP<-rbind(medEXP,MedianEXP)
    #BASKET<-rbind(BASKET,BaseYearBasket)
    #CALORY<-rbind(CALORY,Calory)
    #Cal<-rbind(Cal,C)
    #EXPenditure<-rbind(EXPenditure,EXP)
    #MEAT<-rbind(MEAT,Meat)
 # }
  
  cat(Bfd2[,weighted.mean(FGrams>0,Weight)],"\t")
  Bfd2[,Decile:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  if (year==98){
    Deciles[,Decile:=Decile1]
  }
  Bfd2<-merge(Bfd2,Deciles)
  Bfd2[,weighted.mean(FGrams/EqSizeCalory,Weight*Size),by="Decile"][order(Decile)]
  
  MD<-merge(MD,Deciles)
   MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight*Size),by="Decile"][order(Decile)]
 MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by="Decile"][order(Decile)]
  
}
#medEXP<-merge(medEXP,inflation,by=c("Year"))
#medEXP<-medEXP[,RealEXP:=V1/CPI*100]

#EXPenditure<-merge(EXPenditure,inflation,by=c("Year"))
#EXPenditure<-EXPenditure[,RealEXP:=V1/CPI*100]

endtime <- proc.time()
cat("\n\n============================\nIt took",
    (endtime-starttime)["elapsed"]," seconds")
