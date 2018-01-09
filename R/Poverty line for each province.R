# 26- Additional calculations for clustering.R
# 
# 26-Total_Exp.R
# 
# Copyright © 2018:Arin Shahbazian
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


#Sort by Province and Expenditure data
Rur <- MyDataRural[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight)]
Urb <- MyDataUrban[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight)]

Rur<- Rur[order(ProvinceCode,Total_Exp_Month_Per_nondurable)]
Urb<- Urb[order(ProvinceCode,Total_Exp_Month_Per_nondurable)]

#Calculate cumulative weights
Rur$cumWeightProv <-ave(Rur$Weight, Rur$ProvinceCode, FUN=cumsum)
Urb$cumWeightProv <-ave(Urb$Weight, Urb$ProvinceCode, FUN=cumsum)

Rur$rx<-ave(Rur$cumWeight, by=list(Rur$ProvinceCode), FUN=max)
Urb$ux<-ave(Urb$cumWeight, by=list(Urb$ProvinceCode), FUN=max)

#Calculate percentiles by weights for each provinces
Rur<- Rur[, ProvincePercentile := Rur$cumWeightProv/Rur$rx]
Rur<- Rur[, ProvincePercentile := ProvincePercentile*100]
Rur<- Rur[, ProvincePercentile := ceiling(ProvincePercentile)]

Urb<- Urb[, ProvincePercentile := Urb$cumWeightProv/Urb$ux]
Urb<- Urb[, ProvincePercentile := ProvincePercentile*100]
Urb<- Urb[, ProvincePercentile := ceiling(ProvincePercentile)]

######### calculate Rural Pov Line #########
###Rural-all
d <- Rur[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight,cumWeightProv,ProvincePercentile)]
setnames(d,c("pct","cal","exp","ndx","prov","w","cumw","provpct"))
d2 <- d [pct<86]
#plot(cal~exp,data=d)
#plot(cal~exp,data=d2)
#plot(log(cal)~log(exp),data=d)
#plot(log(cal)~log(exp),data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=.(provpct,prov)]
dx2 <- d2[,lapply(.SD, mean, na.rm=TRUE),by=.(provpct,prov)]

#Nonlog
for(ostan in 0:30){  
  nam <- paste0("Rur",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d[prov==ostan]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",ostan)
  assign(nam2,nam3)
}
#log
d<-d[cal!=0]
for(ostan in 0:30){  
  nam <- paste0("Rur",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model2 <- lm(log(exp) ~ log(cal), weights = w, data=assign(nam,d[prov==ostan]))
  summary(model2)
  nam3 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural2PovLine",ostan)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

#Nonlog-85 percent
for(ostan in 0:30){  
  nam <- paste0("Rur",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model3 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d2[prov==ostan]))
  summary(model3)
  nam3 <- predict(object = model3, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural3PovLine",ostan)
  assign(nam2,nam3)
}
#log-85 percent
d2<-d2[cal!=0]
for(ostan in 0:30){  
  nam <- paste0("Rur",ostan)
  assign(nam,d2[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model4 <- lm(log(exp) ~ log(cal), weights = w, data=assign(nam,d[prov==ostan]))
  summary(model4)
  nam3 <- predict(object = model4, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural4PovLine",ostan)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

######### calculate Urban Pov Line #########
###Urban-all
d <- Urb[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight,cumWeightProv,ProvincePercentile)]
setnames(d,c("pct","cal","exp","ndx","prov","w","cumw","provpct"))
d2 <- d [pct<86]
#plot(cal~exp,data=d)
#plot(cal~exp,data=d2)
#plot(log(cal)~log(exp),data=d)
#plot(log(cal)~log(exp),data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=.(provpct,prov)]
dx2 <- d2[,lapply(.SD, mean, na.rm=TRUE),by=.(provpct,prov)]

#Nonlog
for(ostan in 0:30){  
  nam <- paste0("Urb",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d[prov==ostan]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",ostan)
  assign(nam2,nam3)
}
#log
d<-d[cal!=0]
for(ostan in 0:30){  
  nam <- paste0("Urb",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model2 <- lm(log(exp) ~ log(cal), weights = w, data=assign(nam,d[prov==ostan]))
  summary(model2)
  nam3 <- predict(object = model2, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban2PovLine",ostan)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

#Nonlog-85 percent
for(ostan in 0:30){  
  nam <- paste0("Urb",ostan)
  assign(nam,d[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model3 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d2[prov==ostan]))
  summary(model3)
  nam3 <- predict(object = model3, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban3PovLine",ostan)
  assign(nam2,nam3)
}
#log-85 percent
d2<-d2[cal!=0]
for(ostan in 0:30){  
  nam <- paste0("Urb",ostan)
  assign(nam,d2[prov==ostan])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model4 <- lm(log(exp) ~ log(cal), weights = w, data=assign(nam,d[prov==ostan]))
  summary(model4)
  nam3 <- predict(object = model4, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban4PovLine",ostan)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}
