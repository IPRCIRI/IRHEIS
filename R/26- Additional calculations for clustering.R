# 26- Additional calculations for clustering.R
# 
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

  
# Calories
MinCalories <- 2100
MinCalories2 <- MinCalories^2
library(data.table)

load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))


######### calculate Rural Pov Line #########
###Rural-all
d <- MyDataRural[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight)]
setnames(d,c("pct","cal","exp","ndx","prov","w"))
d2 <- d [pct<86]
plot(cal~exp,data=d)
plot(cal~exp,data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

#Rural-linear
model1 <- lm(exp ~ cal + cal2 , weights = w, data=d)
summary(model1)
RuralPovLine1 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine1]

#Rural-linear with prov dummy
model2 <- lm(exp ~ cal + cal2 + factor(prov) , weights = w, data=d)
summary(model2)
RuralPovLine2 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine2]

#Rural-log
d<-d[cal!=0]
model3 <- lm(log(exp) ~ log(cal) + log(cal2) , weights = w, data=d)
summary(model3)
RuralPovLine3 <- predict(object = model3, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine3]

#Rural-log with prov dummy
d<-d[cal!=0]
model4 <- lm(log(exp) ~ log(cal) + log(cal2) + factor(prov) , weights = w, data=d)
summary(model4)
RuralPovLine4 <- predict(object = model4, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine4]

###Rural-85percent
#Rural-linear
model5 <- lm(exp ~ cal + cal2 , weights = w, data=d)
summary(model5)
RuralPovLine185 <- predict(object = model5, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine185]

#Rural-linear with prov dummy
model6 <- lm(exp ~ cal + cal2 + factor(prov) , weights = w, data=d)
summary(model6)
RuralPovLine285 <- predict(object = model6, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine285]

#Rural-log
d<-d[cal!=0]
model7 <- lm(log(exp) ~ log(cal) + log(cal2) , weights = w, data=d)
summary(model7)
RuralPovLine385 <- predict(object = model7, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine385]

#Rural-log with prov dummy
d<-d[cal!=0]
model8 <- lm(log(exp) ~ log(cal) + log(cal2) + factor(prov) , weights = w, data=d)
summary(model8)
RuralPovLine485 <- predict(object = model8, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataRural[,PovLine:=RuralPovLine485]

######### calculate Urban Pov Line #########
###Urban-all
d <- MyDataUrban[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight)]
setnames(d,c("pct","cal","exp","ndx","prov","w"))
d2 <- d [pct<86]
plot(cal~exp,data=d)
plot(cal~exp,data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

#Urban-linear
model1 <- lm(exp ~ cal + cal2 , weights = w, data=d)
summary(model1)
UrbanPovLine1 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine1]

#Urban-linear with prov dummy
model2 <- lm(exp ~ cal + cal2 + factor(prov) , weights = w, data=d)
summary(model2)
UrbanPovLine2 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine2]

#Urban-log
d<-d[cal!=0]
model3 <- lm(log(exp) ~ log(cal) + log(cal2) , weights = w, data=d)
summary(model3)
UrbanPovLine3 <- predict(object = model3, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine3]

#Urban-log with prov dummy
d<-d[cal!=0]
model4 <- lm(log(exp) ~ log(cal) + log(cal2) + factor(prov) , weights = w, data=d)
summary(model4)
UrbanPovLine4 <- predict(object = model4, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine4]

###Urban-85percent
#Urban-linear
model5 <- lm(exp ~ cal + cal2 , weights = w, data=d)
summary(model5)
UrbanPovLine185 <- predict(object = model5, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine185]

#Urban-linear with prov dummy
model6 <- lm(exp ~ cal + cal2 + factor(prov) , weights = w, data=d)
summary(model6)
UrbanPovLine285 <- predict(object = model6, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine285]

#Urban-log
d<-d[cal!=0]
model7 <- lm(log(exp) ~ log(cal) + log(cal2) , weights = w, data=d)
summary(model7)
UrbanPovLine385 <- predict(object = model7, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine385]

#Urban-log with prov dummy
d<-d[cal!=0]
model8 <- lm(log(exp) ~ log(cal) + log(cal2) + factor(prov) , weights = w, data=d)
summary(model8)
UrbanPovLine485 <- predict(object = model8, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,prov=10,exp=NA,ndx=NA,w=NA))[[1]]
MyDataUrban[,PovLine:=UrbanPovLine485]


####### Pov HCR ======================= 
MyDataRural[,Poor:=ifelse(Total_Exp_Month_Per<PovLine85,1,0)]
MyDataUrban[,Poor:=ifelse(Total_Exp_Month_Per<PovLine85,1,0)]

MyDataRural[,weighted.mean(Poor,Weight)]
MyDataUrban[,weighted.mean(Poor,Weight)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight)]

MyDataRural[,weighted.mean(Poor,Weight*Size)]
MyDataUrban[,weighted.mean(Poor,Weight*Size)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight*Size)]

####Poor but above 2100 calories
RuralPoor<-MyDataRural[Poor==1]
UrbanPoor<-MyDataUrban[Poor==1]

RuralPoor[,x:= ifelse(Per_Daily_Calories>2100,1,0)]
UrbanPoor[,x:= ifelse(Per_Daily_Calories>2100,1,0)]

RuralPoor[,weighted.mean(x,Weight),by=ProvinceCode]
UrbanPoor[,weighted.mean(x,Weight),by=ProvinceCode]
rbind(RuralPoor,UrbanPoor)[,weighted.mean(x,Weight),by=ProvinceCode]

RuralPoor[,weighted.mean(x,Weight*Size),by=ProvinceCode]
UrbanPoor[,weighted.mean(x,Weight*Size),by=ProvinceCode]
rbind(RuralPoor,UrbanPoor)[,weighted.mean(x,Weight*Size),by=ProvinceCode]
###

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
