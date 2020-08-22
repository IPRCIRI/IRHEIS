# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Refah Months =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)



year<-98
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy)],by="HHID")
  MD[,Decile:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  MD<-merge(MD,Deciles,by="HHID")
  
  MD[,All:=G01+G02+G03+G04+G05+G06+G07+G08+G09+G11+G12+G13]
  
  for (col in c("ego","bathroom","electricity")) 
    MD[is.na(get(col)), (col) := "True"]
  
MD[,Size2:=ifelse(Size>4,5,Size)]
  
  A1<-MD[,.(Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE))]
  
  A2<-MD[,.(Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE)),by=Month]
  
  A3<-MD[,.(Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE)),by="Region"]
  
  A4<-MD[,.(Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE)),by="Decile"]
  
  A5<-MD[,.(Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE)),by=Size2]
  
  A6<-MD[,.(.N,Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE))
         ,by=c("Region","Decile")]
  
  A7<-MD[,.(.N,Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE))
         ,by=c("Month","Decile")]
  
 # write.csv(A7,file="A7.csv")
  
  A8<-MD[,.(.N,Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodExpenditure_Per=weighted.mean(FoodExpenditure/EqSizeCalory,Weight,na.rm=TRUE),
            FoodExpenditure=weighted.mean(FoodExpenditure,Weight,na.rm=TRUE),
            Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE),
            Total_Exp_Month=weighted.mean(Total_Exp_Month,Weight,na.rm=TRUE),
            Size=weighted.mean(Size,Weight,na.rm=TRUE))
         ,by=c("Month","Region")]
  
#}

#write.csv(X1,file="X1.csv")
#write.csv(X2,file="X2.csv")
#write.csv(X3,file="X3.csv")
#write.csv(X4,file="X4.csv")


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])