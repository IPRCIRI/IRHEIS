# 51-PovertyLines.R
# 
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 
library(data.table)

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Lines =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

# TODO: set this in settings:
MinCalories <- 2100
MinCalories2 <- MinCalories^2
# library(readxl)
# library(reldist)
# library(Hmisc)
# library(dplyr)
library(data.table)

load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))



# Explore the Expenditures-Calories Relationship

D <- rbind(MyDataRural,MyDataUrban)
D[,Ost := factor(ProvinceCode)]
D[,Geo5 := factor(floor(HHID/1e6))]

smoothScatter(log(MyDataUrban$Total_Exp_Month_Per),log(MyDataUrban$Per_Daily_Calories))

summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Region, weights = Weight, data=D))
summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Ost * Region, weights = Weight, data=D))
summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Geo5, weights = Weight, data=D))

DSub <- D[as.integer(Percentile) %in% 10:50]

summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Region, weights = Weight, data=DSub))
summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Ost * Region, weights = Weight, data=DSub))
summary(lm(Total_Exp_Month_Per ~ Per_Daily_Calories * Geo5, weights = Weight, data=DSub))
# calculate Rural Pov Line based on MinCalories

d <- MyDataRural[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,Weight)]
setnames(d,c("pct","cal","exp","ndx","w"))
dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=pct]
#dx2 <- dx[pct>3 & pct<50]
dx2 <- dx [pct<86]

plot(cal~exp,data=dx)
plot(cal~exp,data=dx2)

dx$cal2<-dx$cal^2
dx2$cal2<-dx2$cal^2

model <- lm(exp ~ cal + cal2 , weights = w, data=dx)
RuralPovLine <- predict(object = model, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine]

model2 <- lm(exp ~ cal + cal2 , weights = w, data=dx2)
RuralPovLine85 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine85:=RuralPovLine85]

# calculate Urban Pov Line based on MinCalories

d <- MyDataUrban[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,Weight)]
setnames(d,c("pct","cal","exp","ndx","w"))
dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=pct]
#dx2 <- dx[pct>3 & pct<50]
dx2 <- dx [pct<86]

plot(cal~exp,data=dx)
plot(cal~exp,data=dx2)

dx$cal2<-dx$cal^2
dx2$cal2<-dx2$cal^2

model <- lm(exp ~ cal + cal2 , weights = w, data=dx)
UrbanPovLine <- predict(object = model, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine]

model2 <- lm(exp ~ cal + cal2 , weights = w, data=dx2)
UrbanPovLine85 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine85:=UrbanPovLine85]


####### Pov HCR ======================= 

MyDataRural[,Poor:=ifelse(Total_Exp_Month_Per<PovLine85,1,0)]
MyDataUrban[,Poor:=ifelse(Total_Exp_Month_Per<PovLine85,1,0)]

MyDataRural[,weighted.mean(Poor,Weight)]
MyDataUrban[,weighted.mean(Poor,Weight)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight)]

MyDataRural[,weighted.mean(Poor,Weight*Size)]
MyDataUrban[,weighted.mean(Poor,Weight*Size)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight*Size)]

rxi <-sum(MyDataRural$Weight*MyDataRural$Size)
uxi <-sum(MyDataUrban$Weight*MyDataUrban$Size)
rx <- max(MyDataRural$cumweight)
ux <- max(MyDataUrban$cumweight)

rpi<-20730625
upi<-59146847
rp<-6070547
up<-18125488

MyDataRural[,sum(Weight)*rp/rx,by=Poor]
MyDataUrban[,sum(Weight)*up/ux,by=Poor]
rbind(MyDataRural,MyDataUrban)[,sum(Weight)*(rp+up)/(rx+ux),by=.(Poor)]

MyDataRural[,sum(Weight*Size)*rpi/rxi,by=Poor]
MyDataUrban[,sum(Weight*Size)*upi/uxi,by=Poor]
rbind(MyDataRural,MyDataUrban)[,sum(Weight*Size)*(rpi+upi)/(rxi+uxi),by=.(Poor)]

rbind(MyDataRural,MyDataUrban)[Poor==1,sum(Weight)*(rp+up)/(rx+ux),by=ProvinceCode]
rbind(MyDataRural,MyDataUrban)[Poor==1,sum(Weight*Size)*(rpi+upi)/(rxi+uxi),by=ProvinceCode]

MyDataRural[,Welfare0:= ifelse(Poor==1,Total_Exp_Month/Total_Exp_Month_Per*PovLine85-Total_Exp_Month,0)]
MyDataUrban[,Welfare0:= ifelse(Poor==1,Total_Exp_Month/Total_Exp_Month_Per*PovLine85-Total_Exp_Month,0)]

ZaribTabdil <- 1.2/1e9*1.1*1.1*1.02 # Month Rial to Year Toman/Millard*inflation96*inflation97*pop growth
MyDataRural[Poor==1,sum(Welfare0*Weight)*rpi/rxi*ZaribTabdil]
MyDataUrban[Poor==1,sum(Welfare0*Weight)*upi/uxi*ZaribTabdil]
rbind(MyDataRural,MyDataUrban)[Poor==1,sum(Welfare0*Weight)*(rpi+upi)/(rxi+uxi)*ZaribTabdil]

MyDataRural[Poor==1,.(Line=mean(PovLine85*EqSizeRevOECD),Count=sum(Weight)),by=Size][order(Size)]
MyDataUrban[Poor==1,.(Line=mean(PovLine85*EqSizeRevOECD),Count=sum(Weight)),by=Size][order(Size)]
