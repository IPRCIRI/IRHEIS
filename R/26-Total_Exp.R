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
MyData$FoodExpenditure_Per<-MyData$FoodExpenditure/MyData$Dimension

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

# average per_Expenditures per decile
MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_per_Expenditures_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_per_Expenditures_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")

# average per_food_Expenditures per decile
MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_per_FoodExpenditures_decile=weighted.mean(FoodExpenditure_Per,Weight)),by=.(decile)], by="decile")
MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_per_FoodExpenditures_decile=weighted.mean(FoodExpenditure_Per,Weight)),by=.(decile)], by="decile")

#Calculate per_Expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$Total_Exp_Month_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$Total_Exp_Month_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$Total_Exp_Month_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$Total_Exp_Month_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$Total_Exp_Month_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$Total_Exp_Month_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$Total_Exp_Month_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$Total_Exp_Month_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$Total_Exp_Month_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$Total_Exp_Month_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$Total_Exp_Month_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$Total_Exp_Month_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$Total_Exp_Month_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$Total_Exp_Month_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$Total_Exp_Month_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$Total_Exp_Month_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$Total_Exp_Month_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$Total_Exp_Month_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$Total_Exp_Month_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$Total_Exp_Month_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$Total_Exp_Month_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$Total_Exp_Month_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$Total_Exp_Month_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$Total_Exp_Month_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$Total_Exp_Month_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$Total_Exp_Month_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$Total_Exp_Month_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$Total_Exp_Month_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$Total_Exp_Month_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$Total_Exp_Month_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$Total_Exp_Month_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$Total_Exp_Month_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$Total_Exp_Month_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$Total_Exp_Month_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$Total_Exp_Month_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$Total_Exp_Month_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$Total_Exp_Month_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$Total_Exp_Month_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$Total_Exp_Month_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$Total_Exp_Month_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$Total_Exp_Month_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$Total_Exp_Month_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$Total_Exp_Month_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$Total_Exp_Month_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$Total_Exp_Month_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$Total_Exp_Month_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$Total_Exp_Month_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$Total_Exp_Month_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$Total_Exp_Month_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$Total_Exp_Month_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$Total_Exp_Month_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$Total_Exp_Month_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$Total_Exp_Month_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$Total_Exp_Month_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$Total_Exp_Month_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$Total_Exp_Month_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$Total_Exp_Month_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$Total_Exp_Month_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$Total_Exp_Month_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$Total_Exp_Month_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$Total_Exp_Month_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$Total_Exp_Month_Per,PU30$Weight,na.rm = TRUE)


#Calculate per_Food_Expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$FoodExpenditure_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$FoodExpenditure_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$FoodExpenditure_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$FoodExpenditure_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$FoodExpenditure_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$FoodExpenditure_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$FoodExpenditure_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$FoodExpenditure_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$FoodExpenditure_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$FoodExpenditure_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$FoodExpenditure_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$FoodExpenditure_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$FoodExpenditure_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$FoodExpenditure_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$FoodExpenditure_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$FoodExpenditure_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$FoodExpenditure_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$FoodExpenditure_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$FoodExpenditure_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$FoodExpenditure_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$FoodExpenditure_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$FoodExpenditure_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$FoodExpenditure_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$FoodExpenditure_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$FoodExpenditure_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$FoodExpenditure_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$FoodExpenditure_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$FoodExpenditure_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$FoodExpenditure_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$FoodExpenditure_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$FoodExpenditure_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$FoodExpenditure_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$FoodExpenditure_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$FoodExpenditure_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$FoodExpenditure_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$FoodExpenditure_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$FoodExpenditure_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$FoodExpenditure_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$FoodExpenditure_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$FoodExpenditure_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$FoodExpenditure_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$FoodExpenditure_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$FoodExpenditure_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$FoodExpenditure_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$FoodExpenditure_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$FoodExpenditure_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$FoodExpenditure_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$FoodExpenditure_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$FoodExpenditure_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$FoodExpenditure_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$FoodExpenditure_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$FoodExpenditure_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$FoodExpenditure_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$FoodExpenditure_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$FoodExpenditure_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$FoodExpenditure_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$FoodExpenditure_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$FoodExpenditure_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$FoodExpenditure_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$FoodExpenditure_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$FoodExpenditure_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$FoodExpenditure_Per,PU30$Weight,na.rm = TRUE)

#Calculate Food_Expenditures share in total expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$FoodExpenditure_Per/PR0$Total_Exp_Month_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$FoodExpenditure_Per/PR1$Total_Exp_Month_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$FoodExpenditure_Per/PR2$Total_Exp_Month_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$FoodExpenditure_Per/PR3$Total_Exp_Month_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$FoodExpenditure_Per/PR4$Total_Exp_Month_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$FoodExpenditure_Per/PR5$Total_Exp_Month_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$FoodExpenditure_Per/PR6$Total_Exp_Month_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$FoodExpenditure_Per/PR7$Total_Exp_Month_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$FoodExpenditure_Per/PR8$Total_Exp_Month_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$FoodExpenditure_Per/PR9$Total_Exp_Month_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$FoodExpenditure_Per/PR10$Total_Exp_Month_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$FoodExpenditure_Per/PR11$Total_Exp_Month_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$FoodExpenditure_Per/PR12$Total_Exp_Month_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$FoodExpenditure_Per/PR13$Total_Exp_Month_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$FoodExpenditure_Per/PR14$Total_Exp_Month_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$FoodExpenditure_Per/PR15$Total_Exp_Month_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$FoodExpenditure_Per/PR16$Total_Exp_Month_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$FoodExpenditure_Per/PR17$Total_Exp_Month_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$FoodExpenditure_Per/PR18$Total_Exp_Month_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$FoodExpenditure_Per/PR19$Total_Exp_Month_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$FoodExpenditure_Per/PR20$Total_Exp_Month_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$FoodExpenditure_Per/PR21$Total_Exp_Month_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$FoodExpenditure_Per/PR22$Total_Exp_Month_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$FoodExpenditure_Per/PR23$Total_Exp_Month_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$FoodExpenditure_Per/PR24$Total_Exp_Month_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$FoodExpenditure_Per/PR25$Total_Exp_Month_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$FoodExpenditure_Per/PR26$Total_Exp_Month_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$FoodExpenditure_Per/PR27$Total_Exp_Month_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$FoodExpenditure_Per/PR28$Total_Exp_Month_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$FoodExpenditure_Per/PR29$Total_Exp_Month_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$FoodExpenditure_Per/PR30$Total_Exp_Month_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$FoodExpenditure_Per/PU0$Total_Exp_Month_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$FoodExpenditure_Per/PU1$Total_Exp_Month_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$FoodExpenditure_Per/PU2$Total_Exp_Month_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$FoodExpenditure_Per/PU3$Total_Exp_Month_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$FoodExpenditure_Per/PU4$Total_Exp_Month_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$FoodExpenditure_Per/PU5$Total_Exp_Month_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$FoodExpenditure_Per/PU6$Total_Exp_Month_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$FoodExpenditure_Per/PU7$Total_Exp_Month_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$FoodExpenditure_Per/PU8$Total_Exp_Month_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$FoodExpenditure_Per/PU9$Total_Exp_Month_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$FoodExpenditure_Per/PU10$Total_Exp_Month_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$FoodExpenditure_Per/PU11$Total_Exp_Month_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$FoodExpenditure_Per/PU12$Total_Exp_Month_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$FoodExpenditure_Per/PU13$Total_Exp_Month_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$FoodExpenditure_Per/PU14$Total_Exp_Month_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$FoodExpenditure_Per/PU15$Total_Exp_Month_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$FoodExpenditure_Per/PU16$Total_Exp_Month_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$FoodExpenditure_Per/PU17$Total_Exp_Month_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$FoodExpenditure_Per/PU18$Total_Exp_Month_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$FoodExpenditure_Per/PU19$Total_Exp_Month_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$FoodExpenditure_Per/PU20$Total_Exp_Month_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$FoodExpenditure_Per/PU21$Total_Exp_Month_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$FoodExpenditure_Per/PU22$Total_Exp_Month_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$FoodExpenditure_Per/PU23$Total_Exp_Month_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$FoodExpenditure_Per/PU24$Total_Exp_Month_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$FoodExpenditure_Per/PU25$Total_Exp_Month_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$FoodExpenditure_Per/PU26$Total_Exp_Month_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$FoodExpenditure_Per/PU27$Total_Exp_Month_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$FoodExpenditure_Per/PU28$Total_Exp_Month_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$FoodExpenditure_Per/PU29$Total_Exp_Month_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$FoodExpenditure_Per/PU30$Total_Exp_Month_Per,PU30$Weight,na.rm = TRUE)


#load and merge calories data  
load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Rural.rda"))
MyDataRural<-merge(MyDataRural,MyFoodRural,by =c("HHID"),all=TRUE)
MyDataRural$Per_Daily_Calories<-MyDataRural$Daily_Calories/MyDataRural$Dimension

load(file=paste0(Settings$HEISProcessedPath,"Y","95","Food_Calories_Urban.rda"))
MyDataUrban<-merge(MyDataUrban,MyFoodUrban,by =c("HHID"),all=TRUE)
MyDataUrban$Per_Daily_Calories<-MyDataUrban$Daily_Calories/MyDataUrban$Dimension

MyDataRural <-subset(MyDataRural,Per_Daily_Calories>0)
MyDataUrban <-subset(MyDataUrban,Per_Daily_Calories>0)

#Calculate per_Food_Calories for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$Per_Daily_Calories,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$Per_Daily_Calories,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$Per_Daily_Calories,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$Per_Daily_Calories,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$Per_Daily_Calories,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$Per_Daily_Calories,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$Per_Daily_Calories,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$Per_Daily_Calories,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$Per_Daily_Calories,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$Per_Daily_Calories,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$Per_Daily_Calories,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$Per_Daily_Calories,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$Per_Daily_Calories,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$Per_Daily_Calories,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$Per_Daily_Calories,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$Per_Daily_Calories,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$Per_Daily_Calories,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$Per_Daily_Calories,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$Per_Daily_Calories,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$Per_Daily_Calories,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$Per_Daily_Calories,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$Per_Daily_Calories,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$Per_Daily_Calories,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$Per_Daily_Calories,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$Per_Daily_Calories,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$Per_Daily_Calories,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$Per_Daily_Calories,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$Per_Daily_Calories,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$Per_Daily_Calories,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$Per_Daily_Calories,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$Per_Daily_Calories,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$Per_Daily_Calories,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$Per_Daily_Calories,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$Per_Daily_Calories,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$Per_Daily_Calories,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$Per_Daily_Calories,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$Per_Daily_Calories,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$Per_Daily_Calories,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$Per_Daily_Calories,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$Per_Daily_Calories,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$Per_Daily_Calories,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$Per_Daily_Calories,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$Per_Daily_Calories,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$Per_Daily_Calories,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$Per_Daily_Calories,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$Per_Daily_Calories,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$Per_Daily_Calories,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$Per_Daily_Calories,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$Per_Daily_Calories,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$Per_Daily_Calories,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$Per_Daily_Calories,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$Per_Daily_Calories,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$Per_Daily_Calories,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$Per_Daily_Calories,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$Per_Daily_Calories,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$Per_Daily_Calories,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$Per_Daily_Calories,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$Per_Daily_Calories,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$Per_Daily_Calories,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$Per_Daily_Calories,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$Per_Daily_Calories,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$Per_Daily_Calories,PU30$Weight,na.rm = TRUE)


#Calculate average calories in deciles by weights
MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_decile=weighted.mean(Per_Daily_Calories,Weight)),by=.(decile)], by="decile")
#MyData <- merge(MyData, MyData[,.(Average_Calories_percentile=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(percentile)], by="percentile")
# MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")
# MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Calories_Province=mean(Per_Daily_Calories,na.rm=TRUE)),by=.(ProvinceCode)], by="ProvinceCode")


#Calculate average expenditures in deciles by weights
MyDataRural <- merge(MyDataRural, MyDataRural[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
MyDataUrban <- merge(MyDataUrban, MyDataUrban[,.(Average_Expenditure_decile=weighted.mean(Total_Exp_Month_Per,Weight)),by=.(decile)], by="decile")
#MyData <- merge(MyData, MyData[,.(Average_Expenditure_percentile=mean(Total_Exp_Month_Per,na.rm=TRUE)),by=.(percentile)], by="percentile")

#Calculate each calory price
MyDataRural[, Calory_price_decile := ifelse(Average_Calories_decile > 2100, Average_Expenditure_decile/Average_Calories_decile, NA)]
MyDataUrban[, Calory_price_decile := ifelse(Average_Calories_decile > 2100, Average_Expenditure_decile/Average_Calories_decile, NA)]
#MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2100, Average_Expenditure_percentile/Average_Calories_percentile, NA)]

#Calculate households excess expenditures
MyDataRural$Excess_Expenditure_decile <-(MyDataRural$Average_Calories_decile-2100)*(MyDataRural$Calory_price_decile)
MyDataUrban$Excess_Expenditure_decile <-(MyDataUrban$Average_Calories_decile-2100)*(MyDataUrban$Calory_price_decile)
#MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2100)*(MyData$Calory_price_percentile)

#Poverty line
MyDataRural$povertyline_decile <-(MyDataRural$Average_Expenditure_decile-MyDataRural$Excess_Expenditure_decile)
MyDataUrban$povertyline_decile <-(MyDataUrban$Average_Expenditure_decile-MyDataUrban$Excess_Expenditure_decile)

PovertylineRural<-min(MyDataRural[,"povertyline_decile"], na.rm=TRUE)
PovertylineUrban<-min(MyDataUrban[,"povertyline_decile"], na.rm=TRUE)



#Calculate nerkhe sarshomare faghr
Rural_Pop<-sum(MyDataRural$Dimension*MyDataRural$Weight)
Urban_Pop<-sum(MyDataUrban$Dimension*MyDataUrban$Weight) 

Rural_Poor<-MyDataRural[Total_Exp_Month_Per<PovertylineRural]
Urban_Poor<-MyDataUrban[Total_Exp_Month_Per<PovertylineUrban]

Rural_Poor_Pop<-sum(Rural_Poor$Dimension*Rural_Poor$Weight)
Urban_Poor_Pop<-sum(Urban_Poor$Dimension*Urban_Poor$Weight)

Rural_Poor_Index1<-Rural_Poor_Pop/Rural_Pop
Urban_Poor_Index1<-Urban_Poor_Pop/Urban_Pop
###
#Calculate nesbat shekafe daramadi (shedate faghr)
Average_Expenditure_Poors_Rural<-weighted.mean(Rural_Poor$Total_Exp_Month_Per,Rural_Poor$Weight)
Shekaf_Rural2<-PovertylineRural-Average_Expenditure_Poors_Rural
Rural_Poor_Index2<-Shekaf_Rural2/PovertylineRural

Average_Expenditure_Poorss_Urban<-weighted.mean(Urban_Poor$Total_Exp_Month_Per,Urban_Poor$Weight)
Shekaf_Urban2<-PovertylineUrban-Average_Expenditure_Poorss_Urban
Urban_Poor_Index2<-Shekaf_Urban2/PovertylineUrban

#Calculate Foster Index
Rural_Poor$Shekaf_Rural3<-PovertylineRural-Rural_Poor$Total_Exp_Month_Per
Rural_Poor$Shekaf_Rural3<-(Rural_Poor$Shekaf_Rural3)^2*(Rural_Poor$Weight)
Shekaf_Rural3<-sum(Rural_Poor$Shekaf_Rural3)
Rural_Poor_Index3<-Shekaf_Rural3/((Rural_Poor_Pop*(PovertylineRural)^2))

Urban_Poor$Shekaf_Urban3<-PovertylineUrban-Urban_Poor$Total_Exp_Month_Per
Urban_Poor$Shekaf_Urban3<-(Urban_Poor$Shekaf_Urban3)^2*(Urban_Poor$Weight)
Shekaf_Urban3<-sum(Urban_Poor$Shekaf_Urban3)
Urban_Poor_Index3<-Shekaf_Urban3/((Urban_Poor_Pop*(PovertylineUrban)^2))

###Aditional
# aggregate(MyDataRural$Dimension, by=list(MyDataRural$Per_Daily_Calories < 2100), FUN=sum)
# aggregate(MyDataUrban$Dimension, by=list(MyDataUrban$Per_Daily_Calories < 2100), FUN=sum)
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
# MyData[, Calory_price_percentile := ifelse(Average_Calories_percentile > 2100, Average_Expenditure_percentile/Average_Calories_percentile, NA)]
# MyData[, Calory_price_decile := ifelse(Average_Calories_decile > 2100, Average_Expenditure_decile/Average_Calories_decile, NA)]
# MyData$Excess_Expenditure_decile <-(MyData$Average_Calories_decile-2100)*(MyData$Calory_price_decile)
# MyData$Excess_Expenditure_percentile <-(MyData$Average_Calories_percentile-2100)*(MyData$Calory_price_percentile)
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
