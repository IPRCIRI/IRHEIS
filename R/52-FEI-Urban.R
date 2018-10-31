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
MinCalories <- 2300
MinCalories2 <- MinCalories^2


load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))

#Seperate big cities
MyDataUrban[,sum(Weight*Size),by=ProvinceCode][order(V1)]
MyDataUrban[,HHIDs:=as.character(HHID)]
MyDataUrban[,ShahrestanCode:=as.integer(str_sub(HHIDs,2,5))]
MyDataUrban[,sum(Weight*Size),by=ShahrestanCode][order(V1)][330:387]
MyDataUrbanTehran<-MyDataUrban[ProvinceCode==23]
MyDataUrbanTehran[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanTabriz<-MyDataUrban[ProvinceCode==3]
MyDataUrbanTabriz[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanAhvaz<-MyDataUrban[ProvinceCode==6]
MyDataUrbanAhvaz[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanShiraz<-MyDataUrban[ProvinceCode==7]
MyDataUrbanShiraz[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanMashhad<-MyDataUrban[ProvinceCode==9]
MyDataUrbanMashhad[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanEsfahan<-MyDataUrban[ProvinceCode==10]
MyDataUrbanEsfahan[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanKaraj<-MyDataUrban[ProvinceCode==30]
MyDataUrbanKaraj[,sum(Weight*Size),by=ShahrestanCode]
MyDataUrbanKermanshah<-MyDataUrban[ProvinceCode==5]
MyDataUrbanKermanshah[,sum(Weight*Size),by=ShahrestanCode]


MyDataUrban<-MyDataUrban[ShahrestanCode==2301,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==303,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==603,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==707,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==916,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==1002,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==3001,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==2301,ProvinceCode:=as.numeric(ShahrestanCode)]
MyDataUrban<-MyDataUrban[ShahrestanCode==502,ProvinceCode:=as.numeric(ShahrestanCode)]


load(file="dt4Urban.rda")
MyDataUrban<-merge(MyDataUrban,dt2,by =c("ProvinceCode"),all.x=TRUE)

#Sort by Province and Expenditure data
Urb <- MyDataUrban[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,ProvinceCode,Weight, cluster)]
Urb<- Urb[order(cluster,Total_Exp_Month_Per_nondurable)]
Urb<-Urb[Per_Daily_Calories!=0]

#Calculate cumulative weights
Urb$cumWeightcluster <-ave(Urb$Weight, Urb$cluster, FUN=cumsum)

Urb$ux<-ave(Urb$cumWeight, by=list(Urb$cluster), FUN=max)

#Calculate percentiles by weights for each provinces
Urb<- Urb[, clusterPercentile := Urb$cumWeightcluster/Urb$ux]
Urb<- Urb[, clusterPercentile := clusterPercentile*100]
Urb<- Urb[, clusterPercentile := ceiling(clusterPercentile)]

######### calculate Urban Pov Line #########
d <- Urb
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

############Urban-all############
#Nonlog-d
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,d[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  assign(nam2,nam3)
}
summary(model1)
MyDataUrbanCluster<-MyDataUrban[cluster==1]
MyDataUrbanCluster[,Poor:=ifelse(Total_Exp_Month_Per_nondurable < Urban1PovLine1,1,0)]
MyDataUrbanCluster[,sum(HIndivNo),by=cluster][order(cluster)]
MyDataUrbanCluster[,sum(Poor),by=cluster][order(cluster)]
MyDataUrban[,sum(Weight),by=cluster][order(cluster)]

#log-d
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,d[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,d[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}
summary(model1)
#Nonlog-d2
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,d2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,d2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  assign(nam2,nam3)
}
summary(model1)
#log-d2
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,d2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,d2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

######### calculate Urban Pov Line- Percentile #########
d <- Urb
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

############Urban-all############
#Nonlog-d
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,dx[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,dx[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  assign(nam2,nam3)
}

#log-d
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,dx[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,dx[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}

#Nonlog-d2
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,dx2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(exp ~ cal + cal2 , weights = w, data=assign(nam,dx2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  assign(nam2,nam3)
}

#log-d2
for(clus in 1:4){  
  nam <- paste0("Urb",clus)
  assign(nam,dx2[cluster==clus])
  # save(list=ls(pattern = nam),file = paste0(Settings$HEISProcessedPath,nam,".rda"))
  model1 <- lm(log(exp) ~ log(cal) , weights = w, data=assign(nam,dx2[cluster==clus]))
  summary(model1)
  nam3 <- predict(object = model1, newdata = data.table(pct=NA,cal=MinCalories,cal2=MinCalories2,exp=NA,ndx=NA,w=NA))[[1]]
  nam2 <- paste0("Urban1PovLine",clus)
  nam3<-exp(nam3)
  assign(nam2,nam3)
}




endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)