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


FinalCountryResults <- data.table(Year = numeric(0), 
                                  SampleSize = integer(0), MeterPrice = numeric(0), 
                                  House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                  FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                  Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0), 
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                  PovertyDepth = numeric(0))
FinalRegionResults <- data.table(Year = numeric(0),
                                 Region=character(0),
                                 SampleSize = integer(0), MeterPrice = numeric(0), 
                                 House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                 FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                 Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0), 
                                 PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                 PovertyDepth = numeric(0))
FinalClusterResults <- data.table(Year = numeric(0),
                                  NewArea_Name=character(0),
                                  SampleSize = integer(0), MeterPrice = numeric(0), 
                                  House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                  FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                  Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0), 
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                  PovertyDepth = numeric(0))
FinalProvinceResults <- data.table(Year = numeric(0),
                                   ProvinceName=character(0),
                                   SampleSize = integer(0), MeterPrice = numeric(0), 
                                   House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                   FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                   Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0), 
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
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine,1,0 )]
  MD[,FinalPoor0:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine0,1,0 )]
  cat(MD[,.(weighted.mean(FinalPoor,Weight*Size))]$V1,"\t")
  MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor2.rda"))
  
  
  
  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per_nondurable)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per_nondurable)/PovertyLine)^2]
  
  
  
  ################Country##################
  
  X1 <- MD[,.(SampleSize=.N,
              MeterPrice=weighted.mean(MeterPrice,Weight,na.rm = TRUE),
              House_Share=weighted.mean(House_Exp/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
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
              House_Share=weighted.mean(House_Exp/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
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
              House_Share=weighted.mean(House_Exp/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
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
              House_Share=weighted.mean(House_Exp/Total_Exp_Month,Weight),
              FPLine=weighted.mean(FPLine,Weight),
              Bundle_Value=weighted.mean(Bundle_Value,Weight),
              FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight),
              Engel=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight),
              Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight),
              Total_Exp_Month_Per_nondurable=weighted.mean(Total_Exp_Month_Per_nondurable,Weight),
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
  #cat(MD[, sum(Weight*Size)],"\t")
  
  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
  
  DurableD<- MD[ Total_Exp_Month_Per_nondurable>0.8*PovertyLine &
                   Total_Exp_Month_Per_nondurable<1.2*PovertyLine,
                 .(.N,Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight),
                   PovertyLine=mean(PovertyLine)),
                 by=.(Region,NewArea_Name)]
  
  DurableD[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]  
  ##  Total_Exp_Month/TOriginalFoodExpenditure  * FPLIne *(Total_Exp_Month_nondurable+Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable  )
  MD <- merge(MD,DurableD[,.(NewArea_Name,Region,PovertyLine_Final)],by=c("Region","NewArea_Name"))
  
  Z1 <- MD[,.(Share=weighted.mean(TOriginalFoodExpenditure/FoodExpenditure,Weight)),by=FinalPoor]
  Z1[,Year:=year]
  
  OriginalFoodShare <- rbind(OriginalFoodShare,Z1)
  
}
#write_xlsx(FinalClusterResults,path=paste0(Settings$HEISResultsPath,"/ClusterResults.xlsx"),col_names=T)
#write_xlsx(FinalCountryResults,path=paste0(Settings$HEISResultsPath,"/CountryResults.xlsx"),col_names=T)
#write_xlsx(FinalRegionResults,path=paste0(Settings$HEISResultsPath,"/RegionResults.xlsx"),col_names=T)
#write_xlsx(FinalProvinceResults,path=paste0(Settings$HEISResultsPath,"/ProvinceResults.xlsx"),col_names=T)

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")