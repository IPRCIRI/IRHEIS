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
library(DescTools)
Gini <- data.table(Year=NA_integer_,Gini_Income=NA_integer_,Gini_Exp=NA_integer_,Decile=NA_integer_)[0]
Pop <- data.table(Year=NA_integer_,Pop=NA_integer_,ProvinceCode=NA_integer_)[0]


year<-97
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  IncomeTable<-as.data.table(IncomeTable)
  MD<-as.data.table(MD)
  
  IncomeTable<-merge(IncomeTable,MD[,.(HHID,EqSizeOECD,Weight,Total_Exp_Month_Per,Size,Decile)],by=c("HHID"),all.x = TRUE)

  
  IncomeTable<-IncomeTable[,TotalIncome_Per:=NetIncome/EqSizeOECD]
  IncomeTable<-IncomeTable[!is.na(Weight)]
  g<-IncomeTable[,weighted.gini(TotalIncome_Per,Weight)]
  
    print(IncomeTable[,weighted.gini(TotalIncome_Per,Weight*Size)])
    print(IncomeTable[,weighted.gini(Total_Exp_Month_Per,Weight*Size)])
    X1 <- IncomeTable[,.(Gini_Income=weighted.gini(TotalIncome_Per,Weight*Size),
                         Gini_Exp=weighted.gini(Total_Exp_Month_Per,Weight*Size)),by=c("Decile")]
      
    
    X1[,Year:=year]
    
    Gini <- rbind(Gini,X1)
    Gini<-Gini[,.(Gini_Income=mean(as.numeric(Gini_Income)),
                  Gini_Exp=mean(as.numeric(Gini_Exp))),by=.(Year,Decile)]
    
    X2 <- IncomeTable[,.(Pop=sum(Size*Weight))]
    X2[,Year:=year]

  
  #write.csv(Pop,file = "Pop.csv")
}

#Final_Dataset <- read_excel("C:/Users/pc1/Desktop/MRC fall 98/Dataset/Final Dataset.xlsx", sheet = "Sheet4")
#Final_Dataset<-as.data.table(Final_Dataset)
#Final_Dataset2<-Final_Dataset[,weighted.mean(Woman_Contribution,Pop),by=.(Year)]


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
