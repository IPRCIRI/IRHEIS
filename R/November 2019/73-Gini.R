#Gini Coefficient.R
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(acid)

Gini <- data.table(Year=NA_integer_,Gini=NA_integer_,ProvinceCode=NA_integer_)[0]
Pop <- data.table(Year=NA_integer_,Pop=NA_integer_,ProvinceCode=NA_integer_)[0]


#year<-89
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
 load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN.rda"))
  
 m<-c(0:30)
 for(p in m){
   print(MD[ProvinceCode==p,weighted.gini(Total_Exp_Month_Per,Weight)])
   print(MD[,weighted.gini(Total_Exp_Month_Per,Weight)])
}
 
 X1 <- MD[,.(Gini=weighted.gini(Total_Exp_Month_Per,Weight)),by=ProvinceCode]

 X1[,Year:=year]

 Gini <- rbind(Gini,X1)
 Gini<-Gini[,.(Gini=mean(as.numeric(Gini))),by=.(ProvinceCode,Year)]
 
 X2 <- MD[,.(Pop=sum(Size*Weight)),by=ProvinceCode]
 X2[,Year:=year]
 Pop <- rbind(Pop,X2)
 
 write.csv(Pop,file = "Pop.csv")
}

#Final_Dataset <- read_excel("C:/Users/pc1/Desktop/MRC fall 98/Dataset/Final Dataset.xlsx", sheet = "Sheet4")
#Final_Dataset<-as.data.table(Final_Dataset)
#Final_Dataset2<-Final_Dataset[,weighted.mean(Woman_Contribution,Pop),by=.(Year)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
