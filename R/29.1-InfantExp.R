rm(list=ls())

starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
year <- 95

load(paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
load(paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
load(paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))

S01 <- rbind(Tables$R95P3S01,Tables$U95P3S01)
setnames(S01, c("HHID","Code","BuyingMethod","Grams","Kilos","Price","Expenditure"))
S03 <- rbind(Tables$R95P3S03,Tables$U95P3S03)
setnames(S03, c("HHID","Code","BuyingMethod","Expenditure"))
S05 <- rbind(Tables$R95P3S05,Tables$U95P3S05)
setnames(S05, c("HHID","Code","BuyingMethod","Expenditure"))
S09 <- rbind(Tables$R95P3S09,Tables$U95P3S09)
setnames(S09, c("HHID","Code","BuyingMethod","Expenditure"))
S12 <- rbind(Tables$R95P3S12,Tables$U95P3S12)
setnames(S12, c("HHID","Code","BuyingMethod","Expenditure"))
S13 <- rbind(Tables$R95P3S13,Tables$U95P3S13)
setnames(S13, c("HHID","Code","I1","I2","BuyingMethod","Expenditure","SellValue"))
S13 <- S13[,Expenditure:=as.integer(Expenditure)]

rm(Tables)

Q1T <- S01[Code==11413][,.(X1=sum(Expenditure)*12),by="HHID"]
Q2T <- S01[Code==11936][,.(X2=sum(Expenditure)*12),by="HHID"]
Q3T1 <- S03[Code %in% 31271:31279][,.(X31=sum(Expenditure)*12),by="HHID"]
Q3T2 <- S03[Code %in% 32141][,.(X32=sum(Expenditure)*12),by="HHID"]
#Q3T3 <- S03[Code %in% c(31251:31269,31416)][,.(X33=sum(Expenditure)*12),by="HHID"]
#Q3T4 <- S03[Code %in% 32131:32134][,.(X34=sum(Expenditure)*12),by="HHID"]
Q4T <- S05[Code==54034][,.(X4=sum(Expenditure)*12),by="HHID"]
Q5T <- S12[Code==121353][,.(X5=sum(Expenditure)*12),by="HHID"]
Q6T <- S12[Code==123215][,.(X6=sum(Expenditure)*12),by="HHID"]
Q7T <- S09[Code %in% 93111:93117][,.(X7=sum(Expenditure)*12),by="HHID"]
Q7TA <- S13[Code %in% 93118:93129][,.(X7A=sum(Expenditure)),by="HHID"]
Q8T1A <- S13[Code %in% 101111:101115][,.(X81A=sum(Expenditure)),by="HHID"]
Q8T2A <- S13[Code %in% 102111:102114][,.(X82A=sum(Expenditure)),by="HHID"]
Q8T3A <- S13[Code %in% 102211:102215][,.(X83A=sum(Expenditure)),by="HHID"]
Q8T4A <- S13[Code %in% 103111:103116][,.(X84A=sum(Expenditure)),by="HHID"]
Q9TA <- S13[Code %in% 124113][,.(X9A=sum(Expenditure)),by="HHID"]


D <- merge(HHBase, Q1T, by="HHID", all.x = TRUE)
D <- merge(D, Q2T, by="HHID", all.x = TRUE)
D <- merge(D, Q3T1, by="HHID", all.x = TRUE)
D <- merge(D, Q3T2, by="HHID", all.x = TRUE)
D <- merge(D, Q3T3, by="HHID", all.x = TRUE)
D <- merge(D, Q3T4, by="HHID", all.x = TRUE)
D <- merge(D, Q4T, by="HHID", all.x = TRUE)
D <- merge(D, Q5T, by="HHID", all.x = TRUE)
D <- merge(D, Q6T, by="HHID", all.x = TRUE)
D <- merge(D, Q7T, by="HHID", all.x = TRUE)
D <- merge(D, Q7TA, by="HHID", all.x = TRUE)
D <- merge(D, Q8T1A, by="HHID", all.x = TRUE)
D <- merge(D, Q8T2A, by="HHID", all.x = TRUE)
D <- merge(D, Q8T3A, by="HHID", all.x = TRUE)
D <- merge(D, Q8T4A, by="HHID", all.x = TRUE)
D <- merge(D, Q9TA, by="HHID", all.x = TRUE)

D[is.na(D)]<-0

D <- merge(HHI,D,by="HHID")

load(paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))

HHWeights[,Year:=NULL]
 
D <- merge(D, HHWeights, by="HHID")

summary(D$X1)
D[,X3:=X31+X32]
Di <- D[NInfants>0]

library(ggplot2)

SW <- sum(Di$Weight)

Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X1)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X1),Region)]
Di[,weighted.mean(X1,Weight),by=sign(X1)]
Di[,weighted.mean(X1,Weight),by=.(sign(X1),Region)]
ggplot(Di[X1>0],aes(X1/1e7, weight=Weight)) + geom_histogram()

Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X2)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X2),Region)]
Di[,weighted.mean(X2,Weight),by=sign(X2)]
Di[,weighted.mean(X2,Weight),by=.(sign(X2),Region)]
ggplot(Di[X2>0],aes(X2/1e7, weight=Weight)) + geom_histogram()



Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X3)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X3),Region)]
Di[,weighted.mean(X3,Weight),by=sign(X3)]
Di[,weighted.mean(X3,Weight),by=.(sign(X3),Region)]
ggplot(Di[X3>0],aes(X3/1e7, weight=Weight)) + geom_histogram()
Di[,weighted.mean(X3,Weight)]
Di[,weighted.mean(X3,Weight),by=.(Region)]


Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X4)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X4),Region)]
Di[,weighted.mean(X4,Weight),by=sign(X4)]
Di[,weighted.mean(X4,Weight),by=.(sign(X4),Region)]
ggplot(Di[X4>0],aes(X4/1e7, weight=Weight)) + geom_histogram()
Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X1+X4)]
Di[,.(sum(Weight),sum(Weight)/323818.9),by=.(sign(X1),sign(X4))]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X1+X4),Region)]
Di[,weighted.mean(X4,Weight),by=sign(X1+X4)]
Di[,weighted.mean(X4,Weight),by=.(sign(X1+X4),Region)]


Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X5)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X5),Region)]
Di[,weighted.mean(X5,Weight),by=sign(X5)]
Di[,weighted.mean(X5,Weight),by=.(sign(X5),Region)]
ggplot(Di[X5>0],aes(X5/1e7, weight=Weight)) + geom_histogram()


Di[,.(.N, sum(Weight),sum(Weight)/SW),by=sign(X6)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X6),Region)]
Di[,weighted.mean(X6,Weight),by=sign(X6)]
Di[,weighted.mean(X6,Weight),by=.(sign(X6),Region)]
ggplot(Di[X6>0],aes(X6/1e7, weight=Weight)) + geom_histogram()
Di[,.(sum(Weight),sum(Weight)/SW*12),by=sign(X6)]
Di[,weighted.mean(X6,Weight)/12,by=sign(X6)]
Di[,weighted.mean(X6,Weight)/12,by=.(sign(X6),sign(X6-1e8))]
ggplot(Di[X6>0],aes(X6/12e7, weight=Weight*12)) + geom_histogram()


Di[,X7T:=X7+X7A]

Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X7)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X7),Region)]
Di[,weighted.mean(X7,Weight),by=sign(X7)]
Di[,weighted.mean(X7,Weight),by=.(sign(X7),Region)]
ggplot(Di[X7>0],aes(X7/1e7, weight=Weight)) + geom_histogram()

Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X7A)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X7A),Region)]
Di[,weighted.mean(X7A,Weight),by=sign(X7A)]
Di[,weighted.mean(X7A,Weight),by=.(sign(X7A),Region)]
ggplot(Di[X7A>0],aes(X7A/1e7, weight=Weight)) + geom_histogram()

Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X7T)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X7T),Region)]
Di[,weighted.mean(X7T,Weight),by=sign(X7T)]
Di[,weighted.mean(X7T,Weight),by=.(sign(X7T),Region)]
ggplot(Di[X7T>0],aes(X7T/1e7, weight=Weight)) + geom_histogram()


Di[,.(sum(Weight),sum(Weight)/SW),by=sign(X9A)]
Di[,.(sum(Weight),sum(Weight)/SW),by=.(sign(X9A),Region)]
Di[,weighted.mean(X9A,Weight),by=sign(X9A)]
Di[,weighted.mean(X9A,Weight),by=.(sign(X9A),Region)]
ggplot(Di[X9A>0],aes(X9A/1e7, weight=Weight)) + geom_histogram()


# library(questionr)
# 
# wtd.table(D$NInfants, D$NSmallKids,weights = D$Weight)
# 
# library(XLConnect)
# 
# writeWorksheetToFile(file = "D:/InfantExp.xlsx", data = 
#                        D[,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45)]
#                      ,sheet = "All")
# writeWorksheetToFile(file = "D:/InfantExp.xlsx", data = 
#                        D[,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45),by=NInfants]
#                      ,sheet = "NInfants")
# writeWorksheetToFile(file = "D:/InfantExp.xlsx", data = 
#                        D[,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45),by=NSmallKids]
#                      ,sheet = "NSmallKids")
# writeWorksheetToFile(file = "D:/InfantExp.xlsx", data = 
#                        D[,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45),by=.(NInfants,NSmallKids)]
#                      ,sheet = "Both")
# 
# 
# D[NInfants>0,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45)]
# D[NInfants>0,lapply(.SD,weighted.mean,Weight),.SDcols=c(22:23, 30:45)]
# 
# D[X9A>0,lapply(.SD,weighted.mean,Weight),.SDcols=c(45),by=.(NInfants,NSmallKids)]
