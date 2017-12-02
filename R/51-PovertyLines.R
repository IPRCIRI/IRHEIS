# 51-PovertyLines.R
# 
# Copyright © 2017: Majid Einian
# Licence: GPL-3
# 

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Lines =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

# TODO: set this in settings:
MinCalories <- 2100

# library(readxl)
# library(reldist)
# library(Hmisc)
# library(dplyr)
library(data.table)

load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))
load(file = paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))


# calculate Rural Pov Line based on MinCalories

d <- MyDataRural[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,Weight)]
setnames(d,c("pct","cal","exp","ndx","w"))
dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=pct]
dx2 <- dx[pct>3 & pct<50]

plot(cal~exp,data=dx)
plot(cal~exp,data=dx2)

model <- lm(exp ~ cal, weights = w, data=dx2)
RuralPovLine <- predict(object = model, newdata = data.table(pct=NA,cal=MinCalories,exp=NA,ndx=NA,w=NA))[[1]]

MyDataRural[,PovLine:=RuralPovLine]


# calculate Urban Pov Line based on MinCalories

d <- MyDataUrban[,.(Percentile=as.integer(Percentile),Per_Daily_Calories,Total_Exp_Month_Per,Total_Exp_Month_Per_nondurable,Weight)]
setnames(d,c("pct","cal","exp","ndx","w"))
dx <- d[,lapply(.SD, mean, na.rm=TRUE),by=pct]
dx2 <- dx[pct>3 & pct<50]

plot(cal~exp,data=dx)
plot(cal~exp,data=dx2)

model <- lm(exp ~ cal, weights = w, data=dx2)
UrbanPovLine <- predict(object = model, newdata = data.table(pct=NA,cal=MinCalories,exp=NA,ndx=NA,w=NA))[[1]]

MyDataUrban[,PovLine:=UrbanPovLine]


####### Pov HCR ======================= 

MyDataRural[,Poor:=ifelse(Total_Exp_Month_Per<PovLine,1,0)]
MyDataUrban[,Poor:=ifelse(Total_Exp_Month_Per<PovLine,1,0)]

MyDataRural[,weighted.mean(Poor,Weight)]
MyDataUrban[,weighted.mean(Poor,Weight)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight)]

MyDataRural[,weighted.mean(Poor,Weight*Size)]
MyDataUrban[,weighted.mean(Poor,Weight*Size)]
rbind(MyDataRural,MyDataUrban)[,weighted.mean(Poor,Weight*Size)]

MyDataRural[,sum(Weight)*6070547/6412096,by=Poor]
MyDataUrban[,sum(Weight)*18125488/18441908,by=Poor]
rbind(MyDataRural,MyDataUrban)[,sum(Weight)*24196035/24854004,by=.(Poor)]


MyDataRural[,sum(Weight*Size)*20730625/23402014,by=Poor]
MyDataUrban[,sum(Weight*Size)*59146847/63183262,by=Poor]
rbind(MyDataRural,MyDataUrban)[Poor==1,sum(Weight*Size)*79926270/86585276]

rbind(MyDataRural,MyDataUrban)[Poor==1,sum(Weight*Size)*79926270/86585276,by=ProvinceCode]
