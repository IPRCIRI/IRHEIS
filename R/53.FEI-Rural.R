# FEI method
# 
# 
# 
# Copyright © 2018:Arin Shahbazian
# Licence: GPL-3
# 

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  FEI method =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(reldist)
library(Hmisc)
library(dplyr)
library(data.table)
library(stringr)


# Calories
MinCalories <- 2100
MinCalories2 <- MinCalories^2


load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))



load(file="dt5Rural.rda")
MyDataRural<-merge(MyDataRural,dt2,by =c("ProvinceCode"),all.x=TRUE)

#Sort by Province and Expenditure data
Rur <- MyDataRural[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight, cluster)]
Rur<- Rur[order(cluster,Total_Exp_Month_Per_nondurable)]
Rur<-Rur[Per_Daily_Calories!=0]

#Calculate cumulative weights
Rur$cumWeightcluster <-ave(Rur$Weight, Rur$cluster, FUN=cumsum)

Rur$ux<-ave(Rur$cumWeight, by=list(Rur$cluster), FUN=max)

#Calculate percentiles by weights for each provinces
Rur<- Rur[, clusterPercentile := Rur$cumWeightcluster/Rur$ux]
Rur<- Rur[, clusterPercentile := clusterPercentile*100]
Rur<- Rur[, clusterPercentile := ceiling(clusterPercentile)]

######### calculate Rural Pov Line #########
d <- Rur
setnames(d,c("pct","cal","exp","ndx","prov","w","cluster","cumw","ux","clusterpct"))
d2 <- d [clusterpct<86]
#plot(cal~exp,data=d)
#plot(cal~exp,data=dx2)
#plot(log(cal)~log(exp),data=d)
#plot(log(cal)~log(exp),data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=.(clusterpct,cluster)]
dx2 <- d2[,lapply(.SD, mean, na.rm=TRUE),by=.(clusterpct,cluster)]

############Rural-all############
#Nonlog-d
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,d[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  assign(nam2,nam3)
}

#log-d
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,d[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,d[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

#Nonlog-d2
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,d2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  assign(nam2,nam3)
}

#log-d2
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,d2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,d2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

######### calculate Rural Pov Line- Percentile #########
d <- Rur
setnames(d,c("pct","cal","exp","ndx","prov","w","cluster","cumw","ux","clusterpct"))
d2 <- d [clusterpct<86]
#plot(cal~exp,data=d)
#plot(cal~exp,data=d2)
#plot(log(cal)~log(exp),data=d)
#plot(log(cal)~log(exp),data=d2)

d$cal2<-d$cal^2
d2$cal2<-d2$cal^2

dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=.(clusterpct,cluster)]
dx2 <- d2[,lapply(.SD, mean, na.rm=TRUE),by=.(clusterpct,cluster)]

############Rural-all############
#Nonlog-d
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,dx[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,dx[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  assign(nam2,nam3)
}

#log-d
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,dx[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,dx[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

#Nonlog-d2
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,dx2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,dx2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  assign(nam2,nam3)
}

#log-d2
for(clus in 1:5){  
  nam <- paste0("Rur",clus)
  assign(nam,dx2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,dx2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Rural1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)