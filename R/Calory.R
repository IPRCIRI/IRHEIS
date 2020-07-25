# 703-Bundle 2100
# Builds the Food Groups data.table for households
#
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(readxl)
library(spatstat)
library(writexl)
cat("\n\n================ FoodGroups =====================================\n")
year<-95
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
 load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
 load( file = paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
 MetaData <- read_excel(Settings$MetaDataFilePath, Settings$MDS_FoodGroups)
 MetaData<-as.data.table(MetaData) 
 MetaData<-MetaData[,SheetName:=NULL]
 FoodNames<-MetaData
 save(FoodNames,file ="FoodNames.rda" )
 load(file = "FoodNames.rda")
 
 Base2<-merge(BigFData,FData,by="HHID")
 Base2<-merge(Base2,MD[,.(HHID,NewArea,ProvinceName,cluster3,Decile,
                            FoodProtein_Per,FinalFoodPoor,FinalPoor,Weight,EqSizeOECD,EqSizeCalory,TFoodKCaloriesHH_Per,FoodKCaloriesHH_Per,NewArea_Name)],by="HHID")
 #Base2<-Base2[Region=="Urban" & Decile==2 | Region=="Rural" & Decile==3]
 Basep<-Base2[,FGrams_Per:=FGrams/EqSizeCalory]
 Basep<-Basep[,FoodKCalories_Per:=FoodKCaloriesHH/EqSizeCalory]
 Basep1<-Basep[,weighted.mean(FGrams_Per,Weight*Size),by=c("FoodType","Decile","Region")]
 Basep1<-merge(Basep1,FoodNames,by=c("FoodType"))

 Basep1<-Basep1[,Calory:=V1*KCalories]
     
 dahak<-Basep1[,sum(Calory),by=c("Decile", "Region")]
  
 Base2<-Base2[,FoodProtein_Per:=FoodProteinHH/EqSizeCalory]
 Base<-Base[,Coef:=FoodKCalories_Per/Settings$KCaloryNeed_Adult]
 
 Base<-Base[,FoodKCalories_PerNew:=FoodKCalories_Per/Coef]
 Base<-Base[,FGrams_PerNew:=FGrams_Per/Coef]
 Basepp<-merge(Basepp,FoodNames,by=c("FoodType"))
 
 Base<-Basepp[,calory:=FGrams_Per*KCalories]
 Basepoor<-Base[,weighted.mean(FGrams_Per,Weight*Size),by=c("ProvinceName","FoodType")]
 Basepoor<-Basepoor[,calory:=V1*KCalories]
 Basepoor<-Basepoor[,sum(calory),by=c("ProvinceName")]
 Basepoor<-merge(Basepoor,FoodNames,by=c("FoodType"))
 Basepoor<-Basepoor[,calory:=V1*KCalories]
 S<-Basepoor[,sum(calory),by=c("ProvinceCode")]
 poor<-Base[,weighted.mean(calory,Weight*Size),by=c("Decile","FoodType")]
 
 poorc<-poor[,sum(V1),by=c("ProvinceCode")]
 s<-Base[,weighted.mean(FoodKCalories_Per,Weight*Size),by=c("ProvinceCode")]
 BaseX <- Base[,.(FoodKCalories2=mean(FoodKCalories_PerNew),
                  FoodKCalories3=mean(FoodKCalories_Per),
                  FoodProtein2=mean(FoodProtein_Per),
                  Weight=mean(Weight),
                  ProvinceCode=mean(ProvinceCode)),by=HHID]
 
 BaseX1<-Base[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight),
                        cluster3=mean(cluster3)),
         by=.(ProvinceCode,FoodType)]
 
 #BaseX1<-Base[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight),
       #          cluster3=mean(cluster3)),
            #  by=.(FoodType)]

 BaseX1<-BaseX1[,N2:=max(N),by=.(ProvinceCode)]
 BaseX1<-BaseX1[,Average_New:=N*Average_Consumption/N2]
 BaseM<-BaseX1[,.(ProvinceCode,N,N2,FoodType,Average_New,Average_Consumption)]
 BaseM2<-BaseM[FoodType=="Berenj"]
 BaseM<-BaseM[,.(ProvinceCode,FoodType,N,Average_New,Average_Consumption)]
 BaseM<-merge(BaseM,FoodNames)
 BaseM<-BaseM[,Average_Protein:=Average_New*Protein]
 baseM<-BaseM[,A_calory:=Average_New*KCalories]
 baseM<-baseM[,darsad:=A_calory/2100]
 baseM<-baseM[,Calory:=KCalories]
 BaseM<-BaseM[order(ProvinceCode,FoodType)]
 write_xlsx(price,path="price98.xlsx",col_names=T)
 
 BaseP<-BaseM[,.(Total_Protein=sum(Average_Protein)),
              by=.(ProvinceCode,FoodType)]
 basepoor<-BaseM[,.(Total_Calory=sum(Calory)),
                  by=.(ProvinceCode)]
 BaseX<-BaseX[,Region:=as.integer(str_sub(HHID,1,1))]
 BaseX[,weighted.mean(FoodProtein2,Weight)]
 BaseX[,weighted.mean(FoodProtein2,Weight),by=.(Region,ProvinceCode)]
 
 
 BaseXUrban<-BaseX[Region==1]
 BaseXRural<-BaseX[Region==2]
 plot(FoodKCalories3~FoodProtein2,data=BaseXUrban)
 BaseXRural<-BaseXRural[FoodProtein2<700]
 
 smoothScatter(BaseXRural$FoodKCalories3~BaseXRural$FoodProtein2)
 
#Base2<-Base[FinalPoor==0,.(.N,Average_Consumption=weighted.mean(FGrams_Per,Weight),
# cluster3=mean(cluster3)),by=.(ProvinceCode,FoodType)]
 
# Base3<-MD[PEngel>0.6 & FinalPoor==1]

# Base4<-MD[FinalPoor==0,.(.N,cluster3=mean(cluster3)),by=.(ProvinceCode)]

# Base[FoodType=="Goosht" & FinalPoor==1 ,weighted.mean(FGrams_Per,Weight,na.rm = TRUE),by=.(ProvinceCode)]

 Base3<-Base[FinalPoor==1]
 Base3<-Base2[,FGrams_Per:=FGrams/EqSizeCalory]
 Base3<-Base3[,FoodKCalories_Per:=FoodKCaloriesHH/EqSizeCalory]
 BaseX3<-Base3[,.(.N,Average_Consumption=weighted.mean(FGrams_Per,Weight)),
              by=.(Decile)]
 BaseX3<-merge(BaseX3,FoodNames,by=c("FoodType"))
 #BaseX3<-BaseX3[,A_calory:=]
 BaseX3<-BaseX3[,N2:=max(N),by=.(ProvinceCode)]
 BaseX3<-BaseX3[,Average_New:=N*Average_Consumption/N2]
 BaseX3<-Base3[,FCalory:=Average_Consumption*KCalories]
 BaseX31<-Base3[,sum(FCalory),by=c("ProvinceCode")]
 BaseM3<-BaseX3[,.(ProvinceCode,N,N2,FoodType,Average_New)]
 BaseM3<-BaseM3[order(ProvinceCode,FoodType)]
 write.csv(BaseM3,file="BaseM3.csv")
 Base<-Base[,Decile:=as.numeric(Decile)]
 BaseDecile<- Base3[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight),
                   Decile=mean(Decile)),
                by=.(Decile,FoodType)]
 Base123<-Base[Decile==1 | Decile==2 | Decile==3]
 BaseF123<-Base123[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight)),
                by=.(FoodType)]
 BaseF123<-merge(BaseF123,FoodNames,by=c("FoodType"))
 BaseF123<-BaseF123[,A_Calory:=Average_Consumption*KCalories]
 BaseF123<-BaseF123[,darsad:=A_Calory/2100]
 BaseF123<-BaseF123[,A_Protein:=Average_Consumption*Protein]
 write_xlsx(BaseF123,path="BaseF123darsad.xlsx",col_names=T)
 
 Base8910<-Base[Decile==8 | Decile==9 | Decile==10]
 BaseF8910<-Base8910[,.(.N,Average_Consumption=weighted.mean(FGrams_PerNew,Weight)),
                   by=.(FoodType)]
 BaseF8910<-merge(BaseF8910,FoodNames,by=c("FoodType"))
 BaseF8910<-BaseF8910[,A_Calory:=Average_Consumption*KCalories]
 BaseF8910<-BaseF8910[,darsad:=A_Calory/2100]
 write_xlsx(BaseF8910,path="BaseF8910darsad.xlsx",col_names=T)
 BaseP123<-Base123[,.(.N,Average_Potein1=weighted.mean(ave,Weight))]
 BaseP8910<-Base8910[,.(.N,Average_Potein1=weighted.mean(FoodProtein_Per,Weight))]
 
 write_xlsx(baseM,path="ostana-darsad.xlsx",col_names=T)

cat("\n\n==============Finish==============\nIt took ")
endtime <- proc.time()
cat((endtime-starttime)[3],"seconds.")