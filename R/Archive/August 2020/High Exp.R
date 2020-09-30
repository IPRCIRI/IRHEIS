
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ High Exp =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)

year<-98

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoors.rda"))
MD[,Decile:=NULL]
MD[,Percentile:=NULL]
MD<-merge(MD,Deciles,by="HHID")

R10<-MD[Total_Exp_Month>50000000]
R10[,sum(Weight)]
R10[,weighted.mean(Size,Weight)]
R10_Ostan<-R10[,.(Weight10=sum(Weight),Size10=weighted.mean(Size,Weight)),
                 by=c("ProvinceName","ProvinceCode")]

R9<-MD[Total_Exp_Month>40000000 & Total_Exp_Month<50000000]
R9[,sum(Weight)]
R9[,weighted.mean(Size,Weight)]
R9_Ostan<-R9[,.(Weight9=sum(Weight),Size9=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R8<-MD[Total_Exp_Month>30000000 & Total_Exp_Month<40000000]
R8[,sum(Weight)]
R8[,weighted.mean(Size,Weight)]
R8_Ostan<-R8[,.(Weight8=sum(Weight),Size8=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R7<-MD[Total_Exp_Month>22500000 & Total_Exp_Month<30000000]
R7[,sum(Weight)]
R7[,weighted.mean(Size,Weight)]
R7_Ostan<-R7[,.(Weight7=sum(Weight),Size7=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R6<-MD[Total_Exp_Month>16250000 & Total_Exp_Month<22500000]
R6[,sum(Weight)]
R6[,weighted.mean(Size,Weight)]
R6_Ostan<-R6[,.(Weight6=sum(Weight),Size6=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R5<-MD[Total_Exp_Month>13750000 & Total_Exp_Month<16250000]
R5[,sum(Weight)]
R5[,weighted.mean(Size,Weight)]
R5_Ostan<-R5[,.(Weight5=sum(Weight),Size5=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R4<-MD[Total_Exp_Month>10000000 & Total_Exp_Month<13750000]
R4[,sum(Weight)]
R4[,weighted.mean(Size,Weight)]
R4_Ostan<-R4[,.(Weight4=sum(Weight),Size4=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R3<-MD[Total_Exp_Month>8333333 & Total_Exp_Month<10000000]
R3[,sum(Weight)]
R3[,weighted.mean(Size,Weight)]
R3_Ostan<-R3[,.(Weight3=sum(Weight),Size3=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R2<-MD[Total_Exp_Month>6250000 & Total_Exp_Month<8333333]
R2[,sum(Weight)]
R2[,weighted.mean(Size,Weight)]
R2_Ostan<-R2[,.(Weight2=sum(Weight),Size2=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]

R1<-MD[Total_Exp_Month<6250000]
R1[,sum(Weight)]
R1[,weighted.mean(Size,Weight)]
R1_Ostan<-R1[,.(Weight1=sum(Weight),Size1=weighted.mean(Size,Weight)),
               by=c("ProvinceName","ProvinceCode")]


Data<-merge(R10_Ostan,R9_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R8_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R7_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R6_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R5_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R4_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R3_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R2_Ostan,by=c("ProvinceName","ProvinceCode"))
Data<-merge(Data,R1_Ostan,by=c("ProvinceName","ProvinceCode"))






