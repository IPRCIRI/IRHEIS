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
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), Durable_Adds_Final = numeric(0),
                                  PovertyDepth = numeric(0),PovertyLine_Final= numeric(0))
FinalRegionResults <- data.table(Year = numeric(0),
                                 Region=character(0),
                                 SampleSize = integer(0), MeterPrice = numeric(0), 
                                 House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                 FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                 Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0),  Durable_Adds_Final = numeric(0),
                                 PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                 PovertyDepth = numeric(0),PovertyLine_Final= numeric(0))
FinalClusterResults <- data.table(Year = numeric(0),
                                  cluster3=character(0),
                                  SampleSize = integer(0), MeterPrice = numeric(0), 
                                  House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                  FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                  Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0),  Durable_Adds_Final = numeric(0),
                                  PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                  PovertyDepth = numeric(0),PovertyLine_Final= numeric(0))
FinalProvinceResults <- data.table(Year = numeric(0),
                                   ProvinceName=character(0),
                                   SampleSize = integer(0), MeterPrice = numeric(0), 
                                   House_Share = numeric(0), FPLine = numeric(0), Bundle_Value = numeric(0), 
                                   FoodKCaloriesHH_Per = numeric(0), Engel = numeric(0), Total_Exp_Month_Per = numeric(0), 
                                   Total_Exp_Month_Per_nondurable = numeric(0), PovertyLine = numeric(0),  Durable_Adds_Final = numeric(0),
                                   PovertyHCR = numeric(0), PoorSampleSize = integer(0), PovertyGap = numeric(0), 
                                   PovertyDepth = numeric(0),PovertyLine_Final= numeric(0))

OriginalFoodShare <- data.table(Year=NA_integer_,Share=NA_integer_,FinalPoor=NA_integer_)[0]

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))
  
  #MD<-MD[Region=="Rural"]
  
  MD <- merge(MD,BigEngelTable[Year==year,
                               .(cluster3,Region,
                                 PovertyLine,PovertyLine0,
                                 Engel,ModifiedEngel)],
              by=c("Region","cluster3"))
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine,1,0 )]
  MD[,FinalPoor0:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine0,1,0 )]
  cat(MD[,.(weighted.mean(FinalPoor,Weight*Size))]$V1,"\t")
  MD[,HHEngle:=TOriginalFoodExpenditure/Total_Exp_Month]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  MD10<-MD[,.(HHID,NMiddle,NStudents)]
  save(MD10,file=paste0(Settings$HEISProcessedPath,"Y",year,"school.rda"))
  
  MD[,FGT1M:=(PovertyLine-Total_Exp_Month_Per_nondurable)/PovertyLine]
  MD[,FGT2M:=((PovertyLine-Total_Exp_Month_Per_nondurable)/PovertyLine)^2]
  
  
  MD[,Durable_Adds_Final:=(Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable]
  
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
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight))
           ]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           ]
  X1[,Year:=year]
  X1[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]
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
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight))
           ,by=Region]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           ,by=Region]
  X1[,Year:=year]
  X1[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]
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
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight))
           ,by=cluster3]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           by=cluster3]
  
  
  X1[,Year:=year]
  X1[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","cluster3"))
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
              PovertyHCR=weighted.mean(FinalPoor,Weight*Size),
              Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight))
           ,by="ProvinceName"]
  X2 <- MD[FinalPoor==1,
           .(PoorSampleSize=.N,
             PovertyGap=weighted.mean(FGT1M,Weight*Size),
             PovertyDepth=weighted.mean(FGT2M,Weight*Size)),
           ,by="ProvinceName"]
  
  
  X1[,Year:=year]
  X1[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]
  X2[,Year:=year]
  X <- merge(X1,X2,by=c("Year","ProvinceName"))
  FinalProvinceResults <- rbind(FinalProvinceResults,X)
  
  
  ####################################################
  
  cat(MD[, weighted.mean(FinalPoor,Weight*Size)],"\t")
  #cat(MD[, weighted.mean(FinalPoor0,Weight*Size)],"\t")
  #cat(MD[, weighted.mean(PovertyLine,Weight*Size)],"\t")
  #cat(MD[, weighted.mean(FPLine,Weight*Size)],"\n")
  cat(MD[, sum(FinalPoor*Weight*Size)],"\n")
  #cat(MD[, sum(Weight*Size)],"\t")
  
  MD1<-MD[,.(HHID,FinalPoor)]
  save(MD1,file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"tahsil.rda"))
  School<-as.data.table(School)
  MD<-merge(MD,School,all.x=TRUE,by="HHID")
  
  DurableD<- MD[ Total_Exp_Month_Per_nondurable>0.8*PovertyLine &
                   Total_Exp_Month_Per_nondurable<1.2*PovertyLine,
                 .(.N,Durable_Adds_Final=weighted.mean((Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable,Weight),
                   PovertyLine=mean(PovertyLine)),
                 by=.(Region,cluster3)]
  
  DurableD[,PovertyLine_Final:=PovertyLine*(1+Durable_Adds_Final)]  
  ##  Total_Exp_Month/TOriginalFoodExpenditure  * FPLIne *(Total_Exp_Month_nondurable+Durable_NoDep+Durable_Emergency)/Total_Exp_Month_nondurable  )
  MD <- merge(MD,DurableD[,.(cluster3,Region,PovertyLine_Final)],by=c("Region","cluster3"))
  
  Z1 <- MD[,.(Share=weighted.mean(TOriginalFoodExpenditure/FoodExpenditure,Weight)),by=FinalPoor]
  Z1[,Year:=year]
  
  OriginalFoodShare <- rbind(OriginalFoodShare,Z1)
  
  #cat(MD[as.numeric(Decile)<2,weighted.mean(Total_Exp_Month_Per,Weight)],"\t")
  #cat(MD[as.numeric(Decile)>9,weighted.mean(Total_Exp_Month_Per,Weight)])
  
  #cat(MD[,weighted.mean(Total_Exp_Month_Per,Weight)],"\t")
  #cat(MD[,weighted.mean(FoodExpenditure_Per,Weight)],"\t")
  
  # a<-MD[,weighted.mean(PovertyLine,Weight)]
  # MD[,F_P:=ifelse(Total_Exp_Month_Per_nondurable < a,1,0 )]
  #cat(MD[, weighted.mean(F_P,Weight*Size)],"\t")
}
MD[FinalPoor==1,sum(Weight*Size),by=Region]
MD[CountyCode==2301 & FinalPoor==1,sum(Weight*Size)]

MD[ProvinceCode==11 & CountyCode!=1105 & Region=="Urban", weighted.mean(FinalPoor,Weight*Size)]
MD[ProvinceCode==30 & Region=="Rural", weighted.mean(FinalPoor,Weight*Size)]
MD[Region=="Rural", weighted.mean(FinalPoor,Weight*Size),by=ProvinceName]

MD[,1-weighted.mean(Knowledge,Weight,na.rm = TRUE)]
MD[,1-weighted.mean(Knowledge,Weight,na.rm = TRUE),by="Region"]
MD[,1-weighted.mean(Knowledge,Weight,na.rm = TRUE),by="ProvinceName"][order(V1)]

MD[FinalPoor==1,1-weighted.mean(Knowledge,Weight,na.rm = TRUE)]
MD[FinalPoor==1,1-weighted.mean(Knowledge,Weight,na.rm = TRUE),by="Region"]
MD[FinalPoor==1,1-weighted.mean(Knowledge,Weight,na.rm = TRUE),by="ProvinceName"]

MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE)]
MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE),by="Region"]
MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE),by="ProvinceName"]

MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE)]
MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE),by="Region"]
MD[,weighted.mean(bathroom=="FALSE",Weight,na.rm = TRUE),by="ProvinceName"]

MD[FinalPoor==1,weighted.mean(bathroom=="False",Weight,na.rm = TRUE)]
MD[FinalPoor==1,weighted.mean(bathroom=="False",Weight,na.rm = TRUE),by="Region"]
MD[FinalPoor==1,weighted.mean(bathroom=="False",Weight,na.rm = TRUE),by="ProvinceName"]
MD[FinalPoor==1 & ProvinceCode==11,weighted.mean(bathroom=="False",Weight,na.rm = TRUE)]

MD[,weighted.mean(pipewater=="False",Weight,na.rm = TRUE)]
MD[,weighted.mean(pipewater=="False",Weight,na.rm = TRUE),by="Region"]
#MD[FinalPoor==1,weighted.mean(pipewater=="False",Weight,na.rm = TRUE),by="ProvinceName"]
MD[FinalPoor==0 &  ProvinceCode==11,weighted.mean(pipewater=="False",Weight,na.rm = TRUE)]

MD[,weighted.mean(No_Insurance,Weight,na.rm = TRUE)]
MD[,weighted.mean(No_Insurance,Weight,na.rm = TRUE),by="Region"]
#MD[FinalPoor==1,weighted.mean(pipewater=="False",Weight,na.rm = TRUE),by="ProvinceName"]
MD[FinalPoor==1 &  ProvinceCode==11,weighted.mean(No_Insurance,Weight,na.rm = TRUE)]

Pooldar<-MD[as.numeric((Decile))>7]
Pooldar[,sum(Weight*Size),by=ProvinceName][order(V1)]

#A1<-MD[,weighted.mean(Total_Exp_Month,Weight),by=Decile]
#A2<-MD[,weighted.mean(Total_Exp_Month,Weight),by=ProvinceCode]
#A3<-MD[,weighted.mean(Total_Exp_Month_Per,Weight),by=Decile]
#A4<-MD[,weighted.mean(Total_Exp_Month_Per,Weight),by=ProvinceCode]

#A5<-MD[,weighted.mean(Total_Exp_Month,Weight),by=c("Decile","ProvinceCode")]
#A6<-MD[,weighted.mean(Total_Exp_Month_Per,Weight),by=c("Decile","ProvinceCode")]
#write.csv(A5,file="A5.csv")
#write.csv(A6,file="A6.csv")

AA<-MD[,.(food=weighted.mean(FoodExpenditure_Per,Weight),
          total=weighted.mean(Total_Exp_Month_Per,Weight),
          foodshare=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight),
          houseshare=weighted.mean(House_Exp/Total_Exp_Month,Weight),
          pooshakshare=weighted.mean(Cloth_Exp/Total_Exp_Month,Weight),
          Transportation_Exp=weighted.mean(Transportation_Exp/Total_Exp_Month,Weight),
          Amusement_Exp=weighted.mean(Amusement_Exp/Total_Exp_Month,Weight),
          behdasht=weighted.mean(Hygiene_Exp/Total_Exp_Month,Weight)),by=Region]

Pop<-MD[,sum(Weight*Size)]

MD[TFoodKCaloriesHH_Per<1200,sum(Weight*Size)/Pop]
MD[TFoodKCaloriesHH_Per>1200 & TFoodKCaloriesHH_Per<1500,sum(Weight*Size)/Pop]
MD[TFoodKCaloriesHH_Per>1500 & TFoodKCaloriesHH_Per<1800,sum(Weight*Size)/Pop]
MD[TFoodKCaloriesHH_Per>1800 & TFoodKCaloriesHH_Per<2100,sum(Weight*Size)/Pop]
MD[TFoodKCaloriesHH_Per>2100 & TFoodKCaloriesHH_Per<3000,sum(Weight*Size)/Pop]
MD[TFoodKCaloriesHH_Per>3000,sum(Weight*Size)/Pop]

MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight),by=c("Region","Decile")][order(Region,Decile)]

MD[,Under2100:=ifelse(TFoodKCaloriesHH_Per<2100,1,0)]
MD[,weighted.mean(Under2100,Weight*Size)]
MD[,weighted.mean(Under2100,Weight*Size),by=Region]
MD[,weighted.mean(Under2100,Weight*Size),by=ProvinceCode][order(ProvinceCode)]

write_xlsx(FinalClusterResults,path=paste0(Settings$HEISResultsPath,"/ClusterResults.xlsx"),col_names=T)
write_xlsx(FinalCountryResults,path=paste0(Settings$HEISResultsPath,"/CountryResults.xlsx"),col_names=T)
write_xlsx(FinalRegionResults,path=paste0(Settings$HEISResultsPath,"/RegionResults.xlsx"),col_names=T)
#write_xlsx(FinalProvinceResults,path=paste0(Settings$HEWeightsultsPath,"/ProvinceResults.xlsx"),col_names=T)

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")