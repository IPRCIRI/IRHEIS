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

Data <- data.table(Year = numeric(0),House = numeric(0),car = numeric(0),
                   internet = numeric(0),HEduYears = numeric(0),NEmployed = numeric(0),
                   HSex = numeric(0),HActivityState = numeric(0),Job_Code_Poor = numeric(0),
                   Job_Code_Rich = numeric(0),Engle = numeric(0),TFoodKCaloriesHH_Per = numeric(0),
                   FoodProtein_Per = numeric(0),Government = numeric(0),FinalPoor= numeric(0))

year<-98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  
  Total<-Total[,.(HHID,Job_Main_Code_Pub,Job_Main_Code_Cooperative,Job_Main_Code_Prv,
                  Job_Main_Code_Buss,Job_Main_Code_Agri)]

  

  Total[Total==0]<-NA
  
  MD<-merge(MD,HHHouseProperties,by="HHID")
  MD<-merge(MD,Total,by="HHID")
  MD[,Job_Code:=pmin(Job_Main_Code_Pub,Job_Main_Code_Cooperative,
                    Job_Main_Code_Prv,
                    Job_Main_Code_Buss,Job_Main_Code_Agri,na.rm = TRUE)]
  
  MD[,Government:=ifelse(is.na(Job_Main_Code_Pub),0,1)]
  
  A<-MD[,.(House=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight),
           car=weighted.mean(car=="TRUE",Weight),
           internet=weighted.mean(internet=="TRUE",Weight),
           HEduYears=weighted.mean(HEduYears,Weight),
           NEmployed=weighted.mean(NEmployed,Weight),
           Government=weighted.mean(Government,Weight),
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
}

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")