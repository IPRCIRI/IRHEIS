# 168-Step8-PovertyStats.R
# 
# Copyright © 2018-2020:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Properties =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)
library(ggplot2)

Data <- data.table(Year = numeric(0),House = numeric(0),car = numeric(0),Knowledge= numeric(0),
                   Meat_Gram = numeric(0),Chicken_Gram = numeric(0),Rice_Gram= numeric(0),
                   internet = numeric(0),HEduYears = numeric(0),NEmployed = numeric(0),
                   HSex = numeric(0),HActivityState = numeric(0),Job_Code_Poor = numeric(0),
                   Job_Code_Rich = numeric(0),Engle = numeric(0),TFoodKCaloriesHH_Per = numeric(0),
                   FoodProtein_Per = numeric(0),Government = numeric(0),FinalPoor= numeric(0))

Data2 <- data.table(Year = numeric(0),House = numeric(0),car = numeric(0),Knowledge= numeric(0),
                    Meat_Gram = numeric(0),Chicken_Gram = numeric(0),Rice_Gram= numeric(0),
                   internet = numeric(0),HEduYears = numeric(0),NEmployed = numeric(0),
                   HSex = numeric(0),HActivityState = numeric(0),Job_Code_Poor = numeric(0),
                   Job_Code_Rich = numeric(0),Engle = numeric(0),TFoodKCaloriesHH_Per = numeric(0),
                   FoodProtein_Per = numeric(0),Government = numeric(0),FinalPoor= numeric(0)
                   ,Region= NA_character_)

Data3 <- data.table(Year = numeric(0),House = numeric(0),car = numeric(0),Knowledge= numeric(0),
                    internet = numeric(0),HEduYears = numeric(0),NEmployed = numeric(0),
                    Meat_Gram = numeric(0),Chicken_Gram = numeric(0),Rice_Gram= numeric(0),
                    HSex = numeric(0),HActivityState = numeric(0),Job_Code_Poor = numeric(0),
                    Job_Code_Rich = numeric(0),Engle = numeric(0),TFoodKCaloriesHH_Per = numeric(0),
                    FoodProtein_Per = numeric(0),Government = numeric(0),FinalPoor= numeric(0)
                    ,Region2= NA_character_)

Data4 <- data.table(Year = numeric(0),House = numeric(0),car = numeric(0),Knowledge= numeric(0),
                    Meat_Gram = numeric(0),Chicken_Gram = numeric(0),Rice_Gram= numeric(0),
                    internet = numeric(0),HEduYears = numeric(0),NEmployed = numeric(0),
                    HSex = numeric(0),HActivityState = numeric(0),Job_Code_Poor = numeric(0),
                    Job_Code_Rich = numeric(0),Engle = numeric(0),TFoodKCaloriesHH_Per = numeric(0),
                    FoodProtein_Per = numeric(0),Government = numeric(0)
                    ,Region3= NA_character_)

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
  load( file = paste0(Settings$HEISProcessedPath,"Y",year,"tahsil.rda"))
  
  Total<-Total[,.(HHID,Job_Main_Code_Pub,Job_Main_Code_Cooperative,Job_Main_Code_Prv,
                  Job_Main_Code_Buss,Job_Main_Code_Agri)]

  Meat<-BigFData[FoodType=="Meat"]
  Chicken<-BigFData[FoodType=="Chicken"]
  Rice<-BigFData[FoodType=="Rice"]
  
  Meat[,Meat_Gram:=FGrams]
  Chicken[,Chicken_Gram:=FGrams]
  Rice[,Rice_Gram:=FGrams]
  
  Meat<-Meat[,.(HHID,Meat_Gram)]
  Chicken<-Chicken[,.(HHID,Chicken_Gram)]
  Rice<-Rice[,.(HHID,Rice_Gram)]
  
  Meat<-Meat[,lapply(.SD,sum),by="HHID",.SDcols=c("Meat_Gram")]
  Chicken<-Chicken[,lapply(.SD,sum),by="HHID",.SDcols=c("Chicken_Gram")]
  Rice<-Rice[,lapply(.SD,sum),by="HHID",.SDcols=c("Rice_Gram")]

  Total[Total==0]<-NA
  
  MD<-merge(MD,HHHouseProperties,by="HHID")
  MD<-merge(MD,Total,by="HHID")
  MD[,Job_Code:=pmin(Job_Main_Code_Pub,Job_Main_Code_Cooperative,
                    Job_Main_Code_Prv,
                    Job_Main_Code_Buss,Job_Main_Code_Agri,na.rm = TRUE)]
  
  MD<-merge(MD,School[,.(HHID,Knowledge)],by="HHID")
  
  MD<-merge(MD,Meat,by="HHID",all.x = TRUE)
  MD<-merge(MD,Chicken,by="HHID",all.x = TRUE)
  MD<-merge(MD,Rice,by="HHID",all.x = TRUE)
  
  MD[is.na(Meat_Gram),Meat_Gram:=0]
  MD[is.na(Chicken_Gram),Chicken_Gram:=0]
  MD[is.na(Rice_Gram),Rice_Gram:=0]
  
  MD[,Meat_Gram:=Meat_Gram/EqSizeCalory]
  MD[,Chicken_Gram:=Chicken_Gram/EqSizeCalory]
  MD[,Rice_Gram:=Rice_Gram/EqSizeCalory]
  
  MD[,Government:=ifelse(is.na(Job_Main_Code_Pub),0,1)]
  
  MD[,Region2:=ifelse(NewArea_Name=="Sh_Tehran","Sh_Tehran","Other")]
  
  MD[,Region3:=ifelse(NewArea_Name=="Sh_Tehran","Sh_Tehran",
               ifelse(ProvinceCode==11,"Sistan",
               ifelse(Region=="Urban","Urban","Rural")))]
  
  A<-MD[,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           Meat_Gram=weighted.mean(Meat_Gram,Weight),
           Chicken_Gram=weighted.mean(Chicken_Gram,Weight),
           Rice_Gram=weighted.mean(Rice_Gram,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           Government=weighted.mean(Government,Weight),
           Knowledge=weighted.mean(Knowledge,Weight,na.rm = TRUE),
           HSex=weighted.mean(HSex=="Female",Weight),
           HActivityState=weighted.mean(HActivityState=="Employed",Weight),
           Job_Code_Poor=weighted.mean(Job_Code==9,Weight,na.rm = TRUE),
           Job_Code_Rich=weighted.mean(Job_Code==1 | Job_Code==2,Weight,na.rm = TRUE),
           Engle=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
           TFoodKCaloriesHH_Per=weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE),
           FoodProtein_Per=weighted.mean(FoodProtein_Per,Weight,na.rm = TRUE)),by=FinalPoor]
  
  A[,Year:=year]
  Data <- rbind(Data,A)
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=Government, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Government,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=car, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(car,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=House, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,0.8,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(House,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=HEduYears, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,10,1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(HEduYears,1)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=NEmployed, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,1.5,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(NEmployed,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=HSex, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,0.5,0.05))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(HSex,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=HActivityState, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,1,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(HActivityState,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=Job_Code_Poor, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,1,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Job_Code_Poor,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=Job_Code_Rich, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,1,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Job_Code_Rich,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=Engle, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,1,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Engle,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=TFoodKCaloriesHH_Per, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,3200,200))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(TFoodKCaloriesHH_Per,1)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=FoodProtein_Per, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,120,10))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(FoodProtein_Per,1)), position = position_dodge(width=0.9), vjust=-0.5)
  
  ggplot(Data, aes(fill=factor(FinalPoor), y=Knowledge, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    scale_y_continuous(breaks=seq(0,120,10))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Knowledge,2)), position = position_dodge(width=0.9), vjust=-0.5)
  
  
  B<-MD[,weighted.mean(FinalPoor,Weight),by=HSex]
  
  
  
  Government<-MD[Government==1]
  Government2<-MD[Government==0]
  
  Government2[,weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight)]
  Government[,weighted.mean(tenure=="AgainstService",Weight)]
  
  Government[,Pop_G:=sum(Weight*Size)]
  Government2[,weighted.mean(car=="TRUE",Weight)]
  a<-Government[,.(a=sum(Weight*Size)/Pop_G),by=Decile]
  Government2[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size)]
  Government2[,weighted.mean(FoodProtein_Per,Weight*Size)]
  Government[,weighted.mean(FinalPoor,Weight*Size)]
  
  MD[,weighted.mean(FinalPoor,Weight*Size),by=Government]
  
  
  B<-MD[,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           Meat_Gram=weighted.mean(Meat_Gram,Weight),
           Chicken_Gram=weighted.mean(Chicken_Gram,Weight),
           Rice_Gram=weighted.mean(Rice_Gram,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           Government=weighted.mean(Government,Weight),
           Knowledge=weighted.mean(Knowledge,Weight,na.rm = TRUE),
           HSex=weighted.mean(HSex=="Female",Weight),
           HActivityState=weighted.mean(HActivityState=="Employed",Weight),
           Job_Code_Poor=weighted.mean(Job_Code==9,Weight,na.rm = TRUE),
           Job_Code_Rich=weighted.mean(Job_Code==1 | Job_Code==2,Weight,na.rm = TRUE),
           Engle=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
           TFoodKCaloriesHH_Per=weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE),
           FoodProtein_Per=weighted.mean(FoodProtein_Per,Weight,na.rm = TRUE)),by=c("FinalPoor","Region")]
  
  B[,Year:=year]
  Data2 <- rbind(Data2,B)
  
  C<-MD[,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           Meat_Gram=weighted.mean(Meat_Gram,Weight),
           Chicken_Gram=weighted.mean(Chicken_Gram,Weight),
           Rice_Gram=weighted.mean(Rice_Gram,Weight),
           Government=weighted.mean(Government,Weight),
           Knowledge=weighted.mean(Knowledge,Weight,na.rm = TRUE),
           HSex=weighted.mean(HSex=="Female",Weight),
           HActivityState=weighted.mean(HActivityState=="Employed",Weight),
           Job_Code_Poor=weighted.mean(Job_Code==9,Weight,na.rm = TRUE),
           Job_Code_Rich=weighted.mean(Job_Code==1 | Job_Code==2,Weight,na.rm = TRUE),
           Engle=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
           TFoodKCaloriesHH_Per=weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE),
           FoodProtein_Per=weighted.mean(FoodProtein_Per,Weight,na.rm = TRUE)),by=c("FinalPoor","Region2")]
  
  C[,Year:=year]
  Data3 <- rbind(Data3,C)
  
  
  Z<-Data2[Region=="Urban"]
  png(file="C:/IRHEIS/R/11.png",width=1200, height=800)
  ggplot(Z, aes(fill=factor(FinalPoor), y=Knowledge, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    #scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Knowledge,2)), position = position_dodge(width=0.9), vjust=-0.5)
  dev.off()
  

  Z<-Data2[Region=="Rural"]
  png(file="C:/IRHEIS/R/12.png",width=1200, height=800)
  ggplot(Z, aes(fill=factor(FinalPoor), y=Knowledge, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
   # scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Knowledge,2)), position = position_dodge(width=0.9), vjust=-0.5)
  dev.off()
  
  Z<-Data3[Region2=="Sh_Tehran"]
  png(file="C:/IRHEIS/R/13.png",width=1200, height=800)
  ggplot(Z, aes(fill=factor(FinalPoor), y=Knowledge, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
   # scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=20))+
    geom_text(aes(label=round(Knowledge,2)), position = position_dodge(width=0.9), vjust=-0.5)
  dev.off()
  
  
  D<-MD[FinalPoor==1,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           Meat_Gram=weighted.mean(Meat_Gram,Weight),
           Chicken_Gram=weighted.mean(Chicken_Gram,Weight),
           Rice_Gram=weighted.mean(Rice_Gram,Weight),
           Government=weighted.mean(Government,Weight),
           HSex=weighted.mean(HSex=="Female",Weight),
           Knowledge=weighted.mean(Knowledge,Weight,na.rm = TRUE),
           HActivityState=weighted.mean(HActivityState=="Employed",Weight),
           Job_Code_Poor=weighted.mean(Job_Code==9,Weight,na.rm = TRUE),
           Job_Code_Rich=weighted.mean(Job_Code==1 | Job_Code==2,Weight,na.rm = TRUE),
           Engle=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
           TFoodKCaloriesHH_Per=weighted.mean(TFoodKCaloriesHH_Per,Weight,na.rm = TRUE),
           FoodProtein_Per=weighted.mean(FoodProtein_Per,Weight,na.rm = TRUE)),by=c("Region3")]
  
  D[,Year:=year]
  Data4 <- rbind(Data4,D)
  
  Data4<-Data4[Region3=="Urban" | Region3=="Rural" | Region3=="Sh_Tehran" | Region3=="Sistan"]
  
  png(file="C:/IRHEIS/R/21.png",width=1200, height=750)
  ggplot(Data4, aes(fill=factor(Region3), y=Knowledge, x=Year)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_x_continuous(breaks=seq(90,98,1))+
    # scale_y_continuous(breaks=seq(0,0.7,0.1))+
    theme(text = element_text(size=15))+
    geom_text(aes(label=round(Knowledge,2)), position = position_dodge(width=0.9), vjust=-0.5)
  dev.off()
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")