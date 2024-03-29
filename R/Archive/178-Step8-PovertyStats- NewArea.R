# 168-Step8-PovertyStats.R
# 
# Copyright © 2018-2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)
library(ggplot2)

FinalCountryResults <- data.table(Year = numeric(0), 
                                  SampleSize = integer(0), MeterPrice = numeric(0), 
                                  House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                  FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Expenditure_Month_per = numeric(0), 
                                  Total_Consumption_Month_per = numeric(0), PovertyLine = numeric(0), 
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                  PovertyDepth = numeric(0))
FinalRegionResults <- data.table(Year = numeric(0),
                                 Region=character(0),
                                 SampleSize = integer(0), MeterPrice = numeric(0), 
                                 House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                 FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Expenditure_Month_per = numeric(0), 
                                 Total_Consumption_Month_per = numeric(0), PovertyLine = numeric(0), 
                                 PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                 PovertyDepth = numeric(0))
FinalClusterResults <- data.table(Year = numeric(0),
                                  NewArea_Name=character(0),
                                  SampleSize = integer(0), MeterPrice = numeric(0), 
                                  House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                  FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Expenditure_Month_per = numeric(0), 
                                  Total_Consumption_Month_per = numeric(0), PovertyLine = numeric(0), 
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                  PovertyDepth = numeric(0))
FinalProvinceResults <- data.table(Year = numeric(0),
                                   ProvinceName=character(0),
                                   SampleSize = integer(0), MeterPrice = numeric(0), 
                                   House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                   FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Expenditure_Month_per = numeric(0), 
                                   Total_Consumption_Month_per = numeric(0), PovertyLine = numeric(0), 
                                   PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                   PovertyDepth = numeric(0))

OriginalFoodShare <- data.table(Year=NA_integer_,Share=NA_integer_,FinalPoor=NA_integer_)[0]

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor2.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"BigEngelTable2.rda"))
  
  
  
  MD <- merge(MD,BigEngelTable[Year==year,
                               .(NewArea_Name,Region,
                                 PovertyLine,PovertyLine0,
                                 Engel,ModifiedEngel)],
              by=c("Region","NewArea_Name"))
  MD[,FinalPoor:=ifelse(Total_Consumption_Month_per < PovertyLine,1,0 )]
  MD[,FinalPoor0:=ifelse(Total_Consumption_Month_per < PovertyLine0,1,0 )]
  cat(MD[,.(weighted.mean(FinalPoor,Weight*Size))]$V1,"\t")
  MD[,HHEngle:=TOriginalFoodExpenditure/Total_Expenditure_Month]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor2.rda"))
  
  
  
  MD[,FGT1M:=(PovertyLine-Total_Consumption_Month_per)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Consumption_Month_per)/PovertyLine)^2]
  
  
  
  ################Country##################
  
  X1 <- MD[,.(SampleSize=.N,
              MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(House_Exp/Total_Expenditure_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Expenditure_Month,Weight),
              Total_Expenditure_Month_per=weighted.mean(Total_Expenditure_Month_per,Weight),
              Total_Consumption_Month_per=weighted.mean(Total_Consumption_Month_per,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))
  ]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
  ]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by="Year")
  FinalCountryResults <- rbind(FinalCountryResults,X)
  
  ################Region##################
  X1 <- MD[,.(SampleSize=.N,
              MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(House_Exp/Total_Expenditure_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Expenditure_Month,Weight),
              Total_Expenditure_Month_per=weighted.mean(Total_Expenditure_Month_per,Weight),
              Total_Consumption_Month_per=weighted.mean(Total_Consumption_Month_per,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))
           ,by=Region]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           ,by=Region]
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","Region"))
  FinalRegionResults <- rbind(FinalRegionResults,X)
  
  
  ################Cluster##################
  X1 <- MD[,.(SampleSize=.N,
              MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(House_Exp/Total_Expenditure_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Expenditure_Month,Weight),
              Total_Expenditure_Month_per=weighted.mean(Total_Expenditure_Month_per,Weight),
              Total_Consumption_Month_per=weighted.mean(Total_Consumption_Month_per,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))
           ,by=NewArea_Name]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           by=NewArea_Name]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","NewArea_Name"))
  FinalClusterResults <- rbind(FinalClusterResults,X)
  
  ################Province##################
  X1 <- MD[,.(SampleSize=.N,
              MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(House_Exp/Total_Expenditure_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Expenditure_Month,Weight),
              Total_Expenditure_Month_per=weighted.mean(Total_Expenditure_Month_per,Weight),
              Total_Consumption_Month_per=weighted.mean(Total_Consumption_Month_per,Weight),
              PovertyLine=weighted.mean(PovertyLine,Weight*Size),
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size))
           ,by="ProvinceName"]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           ,by="ProvinceName"]
  
  
  X1[,Year:=year]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","ProvinceName"))
  FinalProvinceResults <- rbind(FinalProvinceResults,X)
  
  ####################################################
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FinalPoor0,Weight*Size)],"\t")
  cat(MD[, weighted.mean(PovertyLine,Weight*Size)],"\t")
  cat(MD[, weighted.mean(FPLine,Weight*Size)],"\t")

  
}

load(file="Inflation9098.rda")
BigEngelTable<-merge(BigEngelTable,Inflation9098,by="ProvinceCode")

BigEngelTable[,Final_PovertyLine:=ifelse(Year==90,PovertyLine0*y9091*y9192*y9293*y9394*y9495*y9596*y9697*y9798,
                                  ifelse(Year==91,PovertyLine0*y9192*y9293*y9394*y9495*y9596*y9697*y9798,
                                  ifelse(Year==92,PovertyLine0*y9293*y9394*y9495*y9596*y9697*y9798,
                                  ifelse(Year==93,PovertyLine0*y9394*y9495*y9596*y9697*y9798,
                                  ifelse(Year==94,PovertyLine0*y9495*y9596*y9697*y9798,
                                  ifelse(Year==95,PovertyLine0*y9596*y9697*y9798,
                                  ifelse(Year==96,PovertyLine0*y9697*y9798,
                                  ifelse(Year==97,PovertyLine0*y9798,PovertyLine0))))))))]

BigEngelTable[,Final_PovertyLine_Mean:=mean(Final_PovertyLine),by=c("Region","NewArea_Name")]


A2<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
          HCR=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")]
A2<-A2[,.(PovertyLine,ProvinceName,N)]
A2$ProvinceName <- factor(A2$ProvinceName, levels = A2$ProvinceName[order(A2$PovertyLine)])
ggplot(A2, aes(x = A2$ProvinceName, y = A2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 16000000)


A2<-MD[Region=="Urban",.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
          HCR=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")]
A2<-A2[,.(PovertyLine,ProvinceName,N)]
A2$ProvinceName <- factor(A2$ProvinceName, levels = A2$ProvinceName[order(A2$PovertyLine)])
ggplot(A2, aes(x = A2$ProvinceName, y = A2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 16000000)


A2<-MD[Region=="Rural",.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
          HCR=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")]
A2<-A2[,.(PovertyLine,ProvinceName,N)]
A2$ProvinceName <- factor(A2$ProvinceName, levels = A2$ProvinceName[order(A2$PovertyLine)])
ggplot(A2, aes(x = A2$ProvinceName, y = A2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 16000000)




endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")
