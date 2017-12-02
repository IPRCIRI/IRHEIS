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
library(reldist)
library(Hmisc)
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
load(file=paste0(Settings$HEISProcessedPath,"Y","95","HHI.rda"))
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
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Behdashts.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Transportations.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Others.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Investments.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Weights95.rda"))

#merge Expenditure groups
MyData<-merge(HHBase,Weights95 ,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,HHI ,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,FoodData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,CigarData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,ClothData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,AmusementData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,CommunicationData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,EducData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,EnergyData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,FurnitureData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,HotelData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,HouseData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,BehdashtData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,TransportationData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,OtherData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,InvestmentData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,MedicalData,by =c("HHID"),all=TRUE)
MyData<-merge(MyData,DurableData,by =c("HHID"),all=TRUE)
MyData[is.na(MyData)] <- 0
MyData<-MyData[Dimension!=0]

#Calculate Per_Total Expenditures Monthly
MyData[, Total_Exp_Month := Reduce(`+`, .SD), .SDcols=c(45:57,59:60)][] 
MyData[, Total_Exp_Month_nondurable := Reduce(`+`, .SD), .SDcols=45:57][] 
#MyData[, EqSizeRevOECD := ifelse(Dimension ==1, 1,
                          # ifelse(Dimension ==2, 1.7, ifelse(Dimension ==3,2.2,
                          # ifelse(Dimension ==4, 2.7, ifelse(Dimension ==5, 3.2, 
                          # ifelse(Dimension ==6, 3.7, ifelse(Dimension ==7, 4.2,
                          # ifelse(Dimension ==8, 4.7, ifelse(Dimension ==9, 5.2, 
                          # ifelse(Dimension ==10, 5.7, ifelse(Dimension ==11, 6.2,
                          # ifelse(Dimension ==12, 6.7, ifelse(Dimension ==13, 7.2, 
                          # ifelse(Dimension ==14, 7.7, ifelse(Dimension ==15, 8.2, 
                          # ifelse(Dimension ==16, 8.7, ifelse(Dimension ==17, 9.2,
#ifelse(Dimension ==18, 9.7, ifelse(Dimension ==19, 10.2, ifelse(Dimension ==20, 10.7, NA))))))))))))))))))))]


MyData$Total_Exp_Month_Per<-MyData$Total_Exp_Month/MyData$EqSizeRevOECD
MyData$Total_Exp_Month_Per_nondurable<-MyData$Total_Exp_Month_nondurable/MyData$EqSizeRevOECD
MyData$FoodExpenditure_Per<-MyData$FoodExpenditure/MyData$EqSizeRevOECD

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
MyDataRural<- MyDataRural[order(Total_Exp_Month_Per_nondurable)]
MyDataUrban<- MyDataUrban[order(Total_Exp_Month_Per_nondurable)]

#Calculate cumulative weights
sum(MyDataRural$Weight)
sum(MyDataUrban$Weight)
MyDataRural$cumweight <- cumsum(MyDataRural$Weight)
MyDataUrban$cumweight <- cumsum(MyDataUrban$Weight)
rx <- max(MyDataRural$cumweight)
ux <- max(MyDataUrban$cumweight)
#Calculate deciles by weights

MyDataRural[,Decile:=cut(cumweight,breaks = seq(0,rx,rx/10),labels = 1:10)]
MyDataRural[,Percentile:=cut(cumweight,breaks=seq(0,rx,rx/100),labels=1:100)]

MyDataUrban[,Decile:=cut(cumweight,breaks = seq(0,ux,ux/10),labels = 1:10)]
MyDataUrban[,Percentile:=cut(cumweight,breaks=seq(0,ux,ux/100),labels=1:100)]




# MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per_nondurable,  wtd.quantile(MyDataRural$Total_Exp_Month_Per_nondurable, weights=MyDataRural$Weight, probs=1:10/10, 
#                                                                                 normwt=TRUE, na.rm=TRUE), left.open=T)
# MyDataRural$decile<- MyDataRural$decile+1


# MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per_nondurable, wtd.quantile(MyDataUrban$Total_Exp_Month_Per_nondurable, weights=MyDataUrban$Weight, probs=1:10/10, 
#                                                                                normwt=TRUE, na.rm=TRUE), left.open=T)
# MyDataUrban$decile<- MyDataUrban$decile+1



#load and merge calories data  
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Rural.rda"))
MyDataRural<-merge(MyDataRural,MyFoodRural,by =c("HHID"),all.x=TRUE)
MyDataRural[,Per_Daily_Calories:=Daily_Calories/Size]

load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Urban.rda"))
MyDataUrban<-merge(MyDataUrban,MyFoodUrban,by =c("HHID"),all.x=TRUE)
MyDataUrban[,Per_Daily_Calories:=Daily_Calories/Size]


save(MyDataRural, file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))
save(MyDataUrban, file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))


# # average per_Expenditures per decile
# MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_per_Expenditures_decile=weighted.mean(Total_Exp_Month_Per_nondurable,Weight)),by=.(decile)], by="decile")
# MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_per_Expenditures_decile=weighted.mean(Total_Exp_Month_Per_nondurable,Weight)),by=.(decile)], by="decile")
# 
# # average per_food_Expenditures per decile
# MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_per_FoodExpenditures_decile=weighted.mean(FoodExpenditure_Per,Weight)),by=.(decile)], by="decile")
# MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_per_FoodExpenditures_decile=weighted.mean(FoodExpenditure_Per,Weight)),by=.(decile)], by="decile")
# 
# 
# #### 
# MyDataRural <-subset(MyDataRural,Per_Daily_Calories>0)
# MyDataUrban <-subset(MyDataUrban,Per_Daily_Calories>0)
# 
# #Calculate average calories in deciles by weights
# MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
# MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
# 
# #################################################
# #***Calculate Poverty line by Nondurable Expenditures***
# #################################################
# #Calculate average expenditures in deciles by weights
# MyDataRural1 <- merge(MyDataRural, MyDataRural[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per_nondurable,Weight)),by=.(decile)], by="decile")
# MyDataUrban1 <- merge(MyDataUrban, MyDataUrban[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per_nondurable,Weight)),by=.(decile)], by="decile")
# 
# #Calculate each calory price
# MyDataRural1[, Calory_price_decile := ifelse(Average_Calories_decile > 2000, Average_Expenditure_decile/Average_Calories_decile, NA)]
# MyDataUrban1[, Calory_price_decile := ifelse(Average_Calories_decile > 2000, Average_Expenditure_decile/Average_Calories_decile, NA)]
# 
# #Calculate households excess expenditures
# MyDataRural1$Excess_Expenditure_decile <-(MyDataRural1$Average_Calories_decile-2000)*(MyDataRural1$Calory_price_decile)
# MyDataUrban1$Excess_Expenditure_decile <-(MyDataUrban1$Average_Calories_decile-2000)*(MyDataUrban1$Calory_price_decile)
# 
# #Poverty line
# MyDataRural1$povertyline_decile <-(MyDataRural1$Average_Expenditure_decile-MyDataRural1$Excess_Expenditure_decile)
# MyDataUrban1$povertyline_decile <-(MyDataUrban1$Average_Expenditure_decile-MyDataUrban1$Excess_Expenditure_decile)
# 
# PovertylineRural1<-min(MyDataRural1[,"povertyline_decile"], na.rm=TRUE)
# PovertylineUrban1<-min(MyDataUrban1[,"povertyline_decile"], na.rm=TRUE)
# 
# 
# MyDataRural1 [, Poverty_line_dimensions := ifelse(Dimension ==1, PovertylineRural1,
#                                       ifelse(Dimension ==2, 1.7*PovertylineRural1, ifelse(Dimension ==3,2.2*PovertylineRural1,
#                                       ifelse(Dimension ==4, 2.7*PovertylineRural1, ifelse(Dimension ==5, 3.2*PovertylineRural1, 
#                                       ifelse(Dimension ==6, 3.7*PovertylineRural1, ifelse(Dimension ==7, 4.2*PovertylineRural1,
#                                       ifelse(Dimension ==8, 4.7*PovertylineRural1, ifelse(Dimension ==9, 5.2*PovertylineRural1, 
#                                       ifelse(Dimension ==10, 5.7*PovertylineRural1, ifelse(Dimension ==11, 6.2*PovertylineRural1,
#                                       ifelse(Dimension ==12, 6.7*PovertylineRural1, ifelse(Dimension ==13, 7.2*PovertylineRural1,
#                                       ifelse(Dimension ==14, 7.7*PovertylineRural1, ifelse(Dimension ==15, 8.2*PovertylineRural1, 
#                                       ifelse(Dimension ==16, 8.7*PovertylineRural1, ifelse(Dimension ==17, 9.2*PovertylineRural1,
#    ifelse(Dimension ==18, 9.7*PovertylineRural1, ifelse(Dimension ==19, 10.2*PovertylineRural1, ifelse(Dimension ==20, 10.7*PovertylineRural1, NA))))))))))))))))))))]
# 
# MyDataUrban1 [, Poverty_line_dimensions := ifelse(Dimension ==1, PovertylineUrban1,
#               ifelse(Dimension ==2, 1.7*PovertylineUrban1, ifelse(Dimension ==3,2.2*PovertylineUrban1,
#               ifelse(Dimension ==4, 2.7*PovertylineUrban1, ifelse(Dimension ==5, 3.2*PovertylineUrban1, 
#               ifelse(Dimension ==6, 3.7*PovertylineUrban1, ifelse(Dimension ==7, 4.2*PovertylineUrban1,
#               ifelse(Dimension ==8, 4.7*PovertylineUrban1, ifelse(Dimension ==9, 5.2*PovertylineUrban1, 
#               ifelse(Dimension ==10, 5.7*PovertylineUrban1, ifelse(Dimension ==11, 6.2*PovertylineUrban1,
#               ifelse(Dimension ==14, 7.7*PovertylineUrban1, ifelse(Dimension ==15, 8.2*PovertylineUrban1, 
#               ifelse(Dimension ==16, 8.7*PovertylineUrban1, ifelse(Dimension ==17, 9.2*PovertylineUrban1,
#               ifelse(Dimension ==18, 9.7*PovertylineUrban1, ifelse(Dimension ==19, 10.2*PovertylineUrban1, ifelse(Dimension ==20, 10.7*PovertylineUrban1, NA))))))))))))))))))]
# 
# #################################################
# #***Calculate Poverty line by  All Expenditures***
# #################################################
# #Calculate average expenditures in deciles by weights
# MyDataRural2 <- merge(MyDataRural, MyDataRural[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
# MyDataUrban2 <- merge(MyDataUrban, MyDataUrban[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
# 
# #Calculate each calory price
# MyDataRural2[, Calory_price_decile := ifelse(Average_Calories_decile > 2000, Average_Expenditure_decile/Average_Calories_decile, NA)]
# MyDataUrban2[, Calory_price_decile := ifelse(Average_Calories_decile > 2000, Average_Expenditure_decile/Average_Calories_decile, NA)]
# 
# #Calculate households excess expenditures
# MyDataRural2$Excess_Expenditure_decile <-(MyDataRural2$Average_Calories_decile-2000)*(MyDataRural2$Calory_price_decile)
# MyDataUrban2$Excess_Expenditure_decile <-(MyDataUrban2$Average_Calories_decile-2000)*(MyDataUrban2$Calory_price_decile)
# 
# #Poverty line
# MyDataRural2$povertyline_decile <-(MyDataRural2$Average_Expenditure_decile-MyDataRural2$Excess_Expenditure_decile)
# MyDataUrban2$povertyline_decile <-(MyDataUrban2$Average_Expenditure_decile-MyDataUrban2$Excess_Expenditure_decile)
# 
# PovertylineRural2<-min(MyDataRural2[,"povertyline_decile"], na.rm=TRUE)
# PovertylineUrban2<-min(MyDataUrban2[,"povertyline_decile"], na.rm=TRUE)
# 
# MyDataRural2 [, Poverty_line_dimensions := ifelse(Dimension ==1, PovertylineRural2,
#                                                   ifelse(Dimension ==2, 1.7*PovertylineRural2, ifelse(Dimension ==3,2.2*PovertylineRural2,
#                                                                                                       ifelse(Dimension ==4, 2.7*PovertylineRural2, ifelse(Dimension ==5, 3.2*PovertylineRural2, 
#                                                                                                                                                           ifelse(Dimension ==6, 3.7*PovertylineRural2, ifelse(Dimension ==7, 4.2*PovertylineRural2,
#                                                                                                                                                                                                               ifelse(Dimension ==8, 4.7*PovertylineRural2, ifelse(Dimension ==9, 5.2*PovertylineRural2, 
#                                                                                                                                                                                                                                                                   ifelse(Dimension ==10, 5.7*PovertylineRural2, ifelse(Dimension ==11, 6.2*PovertylineRural2,
#                                                                                                                                                                                                                                                                                                                        ifelse(Dimension ==12, 6.7*PovertylineRural2, ifelse(Dimension ==13, 7.2*PovertylineRural2,
#                                                                                                                                                                                                                                                                                                                                                                             ifelse(Dimension ==14, 7.7*PovertylineRural2, ifelse(Dimension ==15, 8.2*PovertylineRural2, 
#                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(Dimension ==16, 8.7*PovertylineRural2, ifelse(Dimension ==17, 9.2*PovertylineRural2,
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(Dimension ==18, 9.7*PovertylineRural2, ifelse(Dimension ==19, 10.2*PovertylineRural2, ifelse(Dimension ==20, 10.7*PovertylineRural2, NA))))))))))))))))))))]
# 
# MyDataUrban2 [, Poverty_line_dimensions := ifelse(Dimension ==1, PovertylineUrban2,
#                                                   ifelse(Dimension ==2, 1.7*PovertylineUrban2, ifelse(Dimension ==3,2.2*PovertylineUrban2,
#                                                                                                       ifelse(Dimension ==4, 2.7*PovertylineUrban2, ifelse(Dimension ==5, 3.2*PovertylineUrban2, 
#                                                                                                                                                           ifelse(Dimension ==6, 3.7*PovertylineUrban2, ifelse(Dimension ==7, 4.2*PovertylineUrban2,
#                                                                                                                                                                                                               ifelse(Dimension ==8, 4.7*PovertylineUrban2, ifelse(Dimension ==9, 5.2*PovertylineUrban2, 
#                                                                                                                                                                                                                                                                   ifelse(Dimension ==10, 5.7*PovertylineUrban2, ifelse(Dimension ==11, 6.2*PovertylineUrban2,
#                                                                                                                                                                                                                                                                                                                        ifelse(Dimension ==14, 7.7*PovertylineUrban2, ifelse(Dimension ==15, 8.2*PovertylineUrban2, 
#                                                                                                                                                                                                                                                                                                                                                                             ifelse(Dimension ==16, 8.7*PovertylineUrban2, ifelse(Dimension ==17, 9.2*PovertylineUrban2,
#                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(Dimension ==18, 9.7*PovertylineUrban2, ifelse(Dimension ==19, 10.2*PovertylineUrban2, ifelse(Dimension ==20, 10.7*PovertylineUrban2, NA))))))))))))))))))]
# 
# #################################################
# #***Calculate Poverty Indexes
# #################################################
# #Calculate nerkhe sarshomare faghr
# Rural_Pop<-sum(MyDataRural$Dimension*MyDataRural$Weight)
# Urban_Pop<-sum(MyDataUrban$Dimension*MyDataUrban$Weight) 
# 
# Rural_Poor<-MyDataRural[Total_Exp_Month_Per<PovertylineRural2]
# Urban_Poor<-MyDataUrban[Total_Exp_Month_Per<PovertylineUrban2]
# 
# Rural_Poor_Pop<-sum(Rural_Poor$Dimension*Rural_Poor$Weight)
# Urban_Poor_Pop<-sum(Urban_Poor$Dimension*Urban_Poor$Weight)
# 
# Rural_Poor_Index1<-Rural_Poor_Pop/Rural_Pop
# Urban_Poor_Index1<-Urban_Poor_Pop/Urban_Pop
# ###
# #Calculate nesbat shekafe daramadi (shedate faghr)
# #Average_Expenditure_Poors_Rural<-weighted.mean(Rural_Poor$Total_Exp_Month_Per,Rural_Poor$Weight)
# #Shekaf_Rural2<-PovertylineRural2-Average_Expenditure_Poors_Rural
# #Rural_Poor_Index2<-Shekaf_Rural2/PovertylineRural2
# 
# #Average_Expenditure_Poorss_Urban<-weighted.mean(Urban_Poor$Total_Exp_Month_Per,Urban_Poor$Weight)
# #Shekaf_Urban2<-PovertylineUrban2-Average_Expenditure_Poorss_Urban
# #Urban_Poor_Index2<-Shekaf_Urban2/PovertylineUrban2
# 
# #Calculate Foster Index
# #Rural_Poor$Shekaf_Rural3<-PovertylineRural2-Rural_Poor$Total_Exp_Month_Per
# #Rural_Poor$Shekaf_Rural3<-(Rural_Poor$Shekaf_Rural3)^2*(Rural_Poor$Weight)
# #Shekaf_Rural3<-sum(Rural_Poor$Shekaf_Rural3)
# #Rural_Poor_Index3<-Shekaf_Rural3/((Rural_Poor_Pop*(PovertylineRural2)^2))
# 
# #Urban_Poor$Shekaf_Urban3<-PovertylineUrban2-Urban_Poor$Total_Exp_Month_Per
# #Urban_Poor$Shekaf_Urban3<-(Urban_Poor$Shekaf_Urban3)^2*(Urban_Poor$Weight)
# #Shekaf_Urban3<-sum(Urban_Poor$Shekaf_Urban3)
# #Urban_Poor_Index3<-Shekaf_Urban3/((Urban_Poor_Pop*(PovertylineUrban2)^2))
# 
# ###Aditional
# # aggregate(MyDataRural$Dimension, by=list(MyDataRural$Per_Daily_Calories < 2000), FUN=sum)
# # aggregate(MyDataUrban$Dimension, by=list(MyDataUrban$Per_Daily_Calories < 2000), FUN=sum)
# #Total
# # MyData$decile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
# #MyData$decile<- MyData$decile+1
# #MyData$percentile<-findInterval(MyData$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:100/100), left.open=T)
# # MyData$percentile<- MyData$percentile+1
# 
# # MyDataRural$decile<-findInterval(MyDataRural$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
# # MyDataRural$decile<- MyDataRural$decile+1
# # MyDataUrban$decile<-findInterval(MyDataUrban$Total_Exp_Month_Per, quantile(MyData$Total_Exp_Month_Per, probs=1:10/10), left.open=T)
# # MyDataUrban$decile<- MyDataUrban$decile+1
# 
# # MyData <- merge(MyData, MyData[,.(Average_Calories_decile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(decile)], by="decile")
# # MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")
# # MyData <- merge(MyData, MyData[,.(Average_Expenditure_decile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(decile)], by="decile")
# # MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")
# # MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2000, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
# # MyData[, Calory_price_decile := ifelse(Average_Calories_decile > 2000, Average_Expenditure_decile/Average_Calories_decile, NA)]
# # MyData$Excess_Expenditure_decile <-(MyData$Average_Calories_decile-2000)*(MyData$Calory_price_decile)
# # MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2000)*(MyData$Calory_price_percentile)
# # MyData$povertyline_decile <-(MyData$Average_Expenditure_decile-MyData$Excess_Expenditure_decile)
# # MyData$povertyline_percentile <-(MyData$Average_Expenditure_percentil-MyData$Excess_Expenditure_percentile)
# # MyData$Average_Calories<-MyData[,lapply(.SD,mean),by=decile]
# # MyData$Average_Calories<-MyData[,.(Average_Calories=mean(Per_Daily_Calories)),by=decile]
# # MyData[, .(Average_Calories = mean(Per_Daily_Calories) ), by = .(decile)]
# # MyData$Average_Calories<-mean(MyData[,"Per_Daily_Calories",by=.(decile)])
# # MyData$Average_Calories<-MyData[, Average_Calories:=mean(Daily_Calories), by=decile]
# # MyData[,.(Average_Calories=mean(Per_Daily_Calories)), by=decile]
# # tapply(MyData$Per_Daily_Calories, MyData$decile, mean)
# # aggregate( Per_Daily_Calories ~ percentile, MyData, mean )
# 
# DRR1<-subset(MyDataRural2, Dimension==1)
# mean(DRR1$Poverty_line_dimensions)
# 
# DRR2<-subset(MyDataRural2, Dimension==2)
# mean(DRR2$Poverty_line_dimensions)
# 
# DRR3<-subset(MyDataRural2, Dimension==3)
# mean(DRR3$Poverty_line_dimensions)
# 
# DRR4<-subset(MyDataRural2, Dimension==4)
# mean(DRR4$Poverty_line_dimensions)
# 
# DRR5<-subset(MyDataRural2, Dimension==5)
# mean(DRR5$Poverty_line_dimensions)
# 
# DRR6<-subset(MyDataRural2, Dimension==6)
# mean(DRR6$Poverty_line_dimensions)
# 
# DRR7<-subset(MyDataRural2, Dimension==7)
# mean(DRR7$Poverty_line_dimensions)
# 
# DRR8<-subset(MyDataRural2, Dimension==8)
# mean(DRR8$Poverty_line_dimensions)
# 
# DRR9<-subset(MyDataRural2, Dimension==9)
# mean(DRR9$Poverty_line_dimensions)
# 
# DRR10<-subset(MyDataRural2, Dimension==10)
# mean(DRR10$Poverty_line_dimensions)
# 
# DRR11<-subset(MyDataRural2, Dimension==11)
# mean(DRR11$Poverty_line_dimensions)
# 
# DRR12<-subset(MyDataRural2, Dimension==12)
# mean(DRR12$Poverty_line_dimensions)
# 
# DRR13<-subset(MyDataRural2, Dimension==13)
# mean(DRR13$Poverty_line_dimensions)
# 
# DRR14<-subset(MyDataRural2, Dimension==14)
# mean(DRR14$Poverty_line_dimensions)
# 
# DRR15<-subset(MyDataRural2, Dimension==15)
# mean(DRR15$Poverty_line_dimensions)
# 
# DRR16<-subset(MyDataRural2, Dimension==16)
# mean(DRR16$Poverty_line_dimensions)
# 
# DRR17<-subset(MyDataRural2, Dimension==17)
# mean(DRR17$Poverty_line_dimensions)
# 
# DRR18<-subset(MyDataRural2, Dimension==18)
# mean(DRR18$Poverty_line_dimensions)
# 
# DRR19<-subset(MyDataRural2, Dimension==19)
# mean(DRR19$Poverty_line_dimensions)
# 
# DRR20<-subset(MyDataRural2, Dimension==2)
# mean(DRR20$Poverty_line_dimensions)
# 
# ###########
# DUU1<-subset(MyDataUrban2, Dimension==1)
# mean(DUU1$Poverty_line_dimensions)
# 
# DUU2<-subset(MyDataUrban2, Dimension==2)
# mean(DUU2$Poverty_line_dimensions)
# 
# DUU3<-subset(MyDataUrban2, Dimension==3)
# mean(DUU3$Poverty_line_dimensions)
# 
# DUU4<-subset(MyDataUrban2, Dimension==4)
# mean(DUU4$Poverty_line_dimensions)
# 
# DUU5<-subset(MyDataUrban2, Dimension==5)
# mean(DUU5$Poverty_line_dimensions)
# 
# DUU6<-subset(MyDataUrban2, Dimension==6)
# mean(DUU6$Poverty_line_dimensions)
# 
# DUU7<-subset(MyDataUrban2, Dimension==7)
# mean(DUU7$Poverty_line_dimensions)
# 
# DUU8<-subset(MyDataUrban2, Dimension==8)
# mean(DUU8$Poverty_line_dimensions)
# 
# DUU9<-subset(MyDataUrban2, Dimension==9)
# mean(DUU9$Poverty_line_dimensions)
# 
# DUU10<-subset(MyDataUrban2, Dimension==10)
# mean(DUU10$Poverty_line_dimensions)
# 



endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)
