# 26-Total_Exp.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Total =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(dplyr)
library(data.table)

#load weight file
load("AllWeights.rda")
Weights95<-AllWeights[Year==95]
Weights95[,HHID:=as.numeric(HHID)]
Weights95[,Weight:=as.numeric(Weight)]
Weights95[,Year:=NULL]
save(Weights95, file = paste0(Settings$HEISProcessedPath,"Weights95.rda"))

#for(year in (Settings$startyear:Settings$endyear)){
 # cat(paste0("\n------------------------------\nYear:",year,"\n"))

#load Expenditure groups
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Foods.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Cigars.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Cloths.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Amusements.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Communications.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Durables.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Education.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Energy.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Furnitures.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Hotels.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","House.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Medicals.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Transportations.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Others.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Investments.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Weights95.rda"))

#merge Expenditure groups
  MyData<-merge(HHBase,Weights95 ,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,FoodData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,CigarData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,ClothData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,AmusementData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,CommunicationData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,DurableData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,EducData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,EnergyData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,FurnitureData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,HotelData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,HouseData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,MedicalData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,TransportationData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,OtherData,by =c("HHID"),all=TRUE)
  MyData<-merge(MyData,InvestmentData,by =c("HHID"),all=TRUE)
    MyData[is.na(MyData)] <- 0
  MyData<-MyData[Dimension!=0]

  #Calculate Per_Total Expenditures Monthly
  MyData[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=21:35][] 
  #MyData[,lapply(.SD,weighted.mean,w=Weight,by=key,.SDcols=letters[1:5]]
  MyData$Total_Exp_Month_Per<-MyData$Total_Exp_Month/MyData$Dimension
 
#Seperate Urban & Rural data
  MyDataRural<-MyData[(MyData$Region=="Rural"),]
  MyDataUrban<-MyData[(MyData$Region=="Urban"),]



#Additiona
 # MyDataRural[:cumExp] = cumsum(MyDataRural[:Total_Exp_Month_Per])
  #MyDataRural<-MyDataRural[,cumsum(Total_Exp_Month_Per)]
  #cumsum(MyDataRural$Total_Exp_Month_Per)
  #MyDataRural$D1Rural<-MyDataRural[,sum(MyDataRural$Weight)]
    #MyDataRural$cumweight <- cumsum(MyDataRural$Weight)

  #MyDataRural[order(Total_Exp_Month_Per), .SD]
  #MyDataRural[,sort(Total_Exp_Month_Per,decreasing = FALSE)]
  #sum(MyDataRural$Weight)
  #arrange(MyDataRural, Total_Exp_Month_Per)
 
 # setorder(MyDataRural, Total_Exp_Month_Per,  na.last=FALSE)
  #hdquantile(MyDataRural$Total_Exp_Month_Per, probs = seq(0, 1, 0.25),
            # se = FALSE, na.rm = FALSE, names = TRUE, weights=TRUE)
  #quantilefreq(MyDataRural, probs == c(0, 0.25, 0.5, 0.75, 1), MyDataRural$Weight = NULL)
  #MyDataRural[preferred.order, on="Per_Daily_Calories"]
# wtd.quantile(MyDataRural$Total_Exp_Month_Per,q=0.1,na.rm = FALSE, MyDataRural$Weight==FALSE)
  #wtd.quantile (MyDataRural$Total_Exp_Month_Per, q=0.5, na.rm = FALSE, Weight=FALSE)

  #Sort Expenditure data
  MyDataRural<- MyDataRural[with(MyDataRural, order(Total_Exp_Month_Per)), ]
  MyDataUrban<- MyDataUrban[with(MyDataUrban, order(Total_Exp_Month_Per)), ]
  
  #Calculate cumulative weights
  sum(MyDataRural$Weight)
  sum(MyDataUrban$Weight)
  MyDataRural$cumweight <- cumsum(MyDataRural$Weight)
  MyDataUrban$cumweight <- cumsum(MyDataUrban$Weight)
  
  
  #Calculate deciles by weights
  MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per,  wtd.quantile(MyDataRural$Total_Exp_Month_Per, weights=MyDataRural$Weight, probs=1:10/10, 
                                                                                  normwt=TRUE, na.rm=TRUE), left.open=T)
  MyDataRural$decile<- MyDataRural$decile+1

  MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per, wtd.quantile(MyDataUrban$Total_Exp_Month_Per, weights=MyDataUrban$Weight, probs=1:10/10, 
                                                                                 normwt=TRUE, na.rm=TRUE), left.open=T)
  MyDataUrban$decile<- MyDataUrban$decile+1

  
#load and merge calories data  
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Rural.rda"))
  MyDataRural<-merge(MyDataRural,MyFoodRural,by =c("HHID"),all=TRUE)
  MyDataRural$Per_Daily_Calories<-MyDataRural$Daily_Calories/MyDataRural$Dimension
  
  load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Urban.rda"))
  MyDataUrban<-merge(MyDataUrban,MyFoodUrban,by =c("HHID"),all=TRUE)
  MyDataUrban$Per_Daily_Calories<-MyDataUrban$Daily_Calories/MyDataUrban$Dimension
  
  MyDataRural <-subset(MyDataRural,Per_Daily_Calories>0)
  MyDataUrban <-subset(MyDataUrban,Per_Daily_Calories>0)
  
  #Calculate average calories in deciles by weights
  MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
  MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
 #MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")
 # MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")
 # MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")
  
#Calculate Calories for deciles-Rural
  d1<-subset(MyDataRural, decile==1)
  mean(d1$Per_Daily_Calories,na.rm = TRUE)
  
  d2<-subset(MyDataRural, decile==2)
  mean(d2$Per_Daily_Calories,na.rm = TRUE)
  
  d3<-subset(MyDataRural, decile==3)
  mean(d3$Per_Daily_Calories,na.rm = TRUE)
  
  d4<-subset(MyDataRural, decile==4)
  mean(d4$Per_Daily_Calories,na.rm = TRUE)
  
  d5<-subset(MyDataRural, decile==5)
  mean(d5$Per_Daily_Calories,na.rm = TRUE)
  
  d6<-subset(MyDataRural, decile==6)
  mean(d6$Per_Daily_Calories,na.rm = TRUE)
  
  d7<-subset(MyDataRural, decile==7)
  mean(d7$Per_Daily_Calories,na.rm = TRUE)
  
  d8<-subset(MyDataRural, decile==8)
  mean(d8$Per_Daily_Calories,na.rm = TRUE)
  
  d9<-subset(MyDataRural, decile==9)
  mean(d9$Per_Daily_Calories,na.rm = TRUE)
  
  d10<-subset(MyDataRural, decile==10)
  mean(d10$Per_Daily_Calories,na.rm = TRUE)
  
  #Calculate Calories for deciles-Urban
  d1<-subset(MyDataUrban, decile==1)
  mean(d1$Per_Daily_Calories,na.rm = TRUE)
  
  d2<-subset(MyDataUrban, decile==2)
  mean(d2$Per_Daily_Calories,na.rm = TRUE)
  
  d3<-subset(MyDataUrban, decile==3)
  mean(d3$Per_Daily_Calories,na.rm = TRUE)
  
  d4<-subset(MyDataUrban, decile==4)
  mean(d4$Per_Daily_Calories,na.rm = TRUE)
  
  d5<-subset(MyDataUrban, decile==5)
  mean(d5$Per_Daily_Calories,na.rm = TRUE)
  
  d6<-subset(MyDataUrban, decile==6)
  mean(d6$Per_Daily_Calories,na.rm = TRUE)
  
  d7<-subset(MyDataUrban, decile==7)
  mean(d7$Per_Daily_Calories,na.rm = TRUE)
  
  d8<-subset(MyDataUrban, decile==8)
  mean(d8$Per_Daily_Calories,na.rm = TRUE)
  
  d9<-subset(MyDataUrban, decile==9)
  mean(d9$Per_Daily_Calories,na.rm = TRUE)
  
  d10<-subset(MyDataUrban, decile==10)
  mean(d10$Per_Daily_Calories,na.rm = TRUE)
  
  
  #Calculate average expenditures in deciles by weights
  MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
  MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
  #MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")
  
  #Calculate each calory price
  MyDataRural[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]
  MyDataUrban[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]
  #MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2300, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
  
  #Calculate households excess expenditures
  MyDataRural$Excess_Expenditure_decile <-(MyDataRural$Average_Calories_decile-2300)*(MyDataRural$Calory_price_decile)
  MyDataUrban$Excess_Expenditure_decile <-(MyDataUrban$Average_Calories_decile-2300)*(MyDataUrban$Calory_price_decile)
  #MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2300)*(MyData$Calory_price_percentile)
  
  #Poverty line
  MyDataRural$povertyline_decile <-(MyDataRural$Average_Expenditure_decile-MyDataRural$Excess_Expenditure_decile)
  MyDataUrban$povertyline_decile <-(MyDataUrban$Average_Expenditure_decile-MyDataUrban$Excess_Expenditure_decile)

  PovertylineRural<-min(MyDataRural[,"povertyline_decile"], na.rm=TRUE)
  PovertylineUrban<-min(MyDataUrban[,"povertyline_decile"], na.rm=TRUE)
  
  
  
  #Calculate nerkhe sarshomare faghr
  Rural_Pop<-sum(MyDataRural$Dimension*MyDataRural$Weight)
  Urban_Pop<-sum(MyDataUrban$Dimension*MyDataUrban$Weight) 
  
  Rural_Poor<-MyDataRural[Per_Daily_Calories<2300]
  Urban_Poor<-MyDataUrban[Per_Daily_Calories<2300]
  
  Rural_Poor_Pop<-sum(Rural_Poor$Dimension*Rural_Poor$Weight)
  Urban_Poor_Pop<-sum(Urban_Poor$Dimension*Urban_Poor$Weight)
  
  Rural_Poor_Index1<-Rural_Poor_Pop/Rural_Pop
  Urban_Poor_Index1<-Urban_Poor_Pop/Urban_Pop
  ###
  #Calculate nesbat shekafe daramadi (shedate faghr)
  Average_exp_Rural2<-(sum(Rural_Poor$Total_Exp_Month_Per*Rural_Poor$Weight))/Rural_Poor_Pop
  Shekaf_Rural2<-PovertylineRural-Average_exp_Rural2
  Rural_Poor_Index2<-Shekaf_Rural2/PovertylineRural
  
  Average_exp_Urban2<-(sum(Urban_Poor$Total_Exp_Month_Per*Urban_Poor$Weight))/Urban_Poor_Pop
  Shekaf_Urban2<-PovertylineUrban-Average_exp_Urban2
  Urban_Poor_Index2<-Shekaf_Urban2/PovertylineUrban
  
  #Calculate Foster Index
  Average_income_Rural3<-Shekaf_Rural2^2
  Rural_Poor_Index3<-Average_income_Rural3/(PovertylineRural^2)
  
  Average_income_Urban3<-Shekaf_Urban2^2
  Urban_Poor_Index3<-Average_income_Urban3/(PovertylineUrban^2)
  
  
  ###Aditional
 # aggregate(MyDataRural$Dimension, by=list(MyDataRural$Per_Daily_Calories < 2300), FUN=sum)
 # aggregate(MyDataUrban$Dimension, by=list(MyDataUrban$Per_Daily_Calories < 2300), FUN=sum)
  #Total
  # MyData$decile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  #MyData$decile<- MyData$decile+1
  #MyData$percentile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:100/100), left.open=T)
  # MyData$percentile<- MyData$percentile+1
  
  # MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  # MyDataRural$decile<- MyDataRural$decile+1
  # MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
  # MyDataUrban$decile<- MyDataUrban$decile+1

  # MyData <- merge(MyData, MyData[,.(Average_Calories_decile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(decile)], by="decile")
  # MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")
  # MyData <- merge(MyData, MyData[,.(Average_Expenditure_decile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(decile)], by="decile")
  # MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")
  # MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2300, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
  # MyData[, Calory_price_decile := ifelse(Average_Calories_decile > 2300, Average_Expenditure_decile/Average_Calories_decile, NA)]
  # MyData$Excess_Expenditure_decile <-(MyData$Average_Calories_decile-2300)*(MyData$Calory_price_decile)
  # MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2300)*(MyData$Calory_price_percentile)
    # MyData$povertyline_decile <-(MyData$Average_Expenditure_decile-MyData$Excess_Expenditure_decile)
  # MyData$povertyline_percentile <-(MyData$Average_Expenditure_percentil-MyData$Excess_Expenditure_percentile)
    # MyData$Average_Calories<-MyData[,lapply(.SD,mean),by=decile]
  # MyData$Average_Calories<-MyData[,.(Average_Calories=mean(Per_Daily_Calories)),by=decile]
  # MyData[, .(Average_Calories = mean(Per_Daily_Calories) ), by = .(decile)]
  # MyData$Average_Calories<-mean(MyData[,"Per_Daily_Calories",by=.(decile)])
  # MyData$Average_Calories<-MyData[, Average_Calories:=mean(Daily_Calories), by=decile]
  # MyData[,.(Average_Calories=mean(Per_Daily_Calories)), by=decile]
  # tapply(MyData$Per_Daily_Calories, MyData$decile, mean)
  # aggregate( Per_Daily_Calories ~ percentile, MyData, mean )

  
  #save(MyData, file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Exp.rda"))
  

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)