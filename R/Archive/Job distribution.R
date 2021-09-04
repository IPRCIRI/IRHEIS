# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Job distribution =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

year<-98
#for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  T2<-Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
              G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
              G0451,G0452,G0453,G0454,Subsidy,
              Job_Main_Code_Pub,Job_Original_Code_Pub,
              Job_Main_Code_Cooperative,Job_Original_Code_Cooperative,
              Job_Main_Code_Prv,Job_Original_Code_Prv,
              Job_Main_Code_Buss,Job_Original_Code_Buss,
              Job_Main_Code_Agri,Job_Original_Code_Agri)]
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy,
                        Job_Main_Code_Pub,Job_Original_Code_Pub,
                        Job_Main_Code_Cooperative,Job_Original_Code_Cooperative,
                        Job_Main_Code_Prv,Job_Original_Code_Prv,
                        Job_Main_Code_Buss,Job_Original_Code_Buss,
                        Job_Main_Code_Agri,Job_Original_Code_Agri,
                        Retirement,Aid,Homemade,Rent,Interest,Intra)],by="HHID")
  
  MD[,All:=G01+G02+G03+G04+G05+G06+G07+G08+G09+G101+
       G102+G103+G104+G105+G11+G12+G13]
  
  for (col in c("ego","bathroom","electricity")) 
    MD[is.na(get(col)), (col) := "True"]
  

MD[,Other_Income:=max(Retirement,Aid,Homemade,Rent,Interest,Intra),by="HHID"]
MD[,Other_Income_Code:=ifelse(Other_Income>0,0.5,0),by="HHID"]

MD[,Job_Code:=max(Job_Main_Code_Pub,Job_Main_Code_Cooperative,Job_Main_Code_Prv,
                  Job_Main_Code_Buss,Job_Main_Code_Agri,Other_Income_Code),by="HHID"]
MD[,Job_Code:=ifelse(Job_Code==0,0.5,Job_Code)]

T3<-MD[,.(Job_Main_Code_Pub,Job_Main_Code_Cooperative,Job_Main_Code_Prv,
          Job_Main_Code_Buss,Job_Main_Code_Agri,Other_Income_Code,Job_Code)]
  
A<-T3[Job_Code>0]

MD[,Category:=ifelse(Job_Main_Code_Pub>0,"Pub",
              ifelse(Job_Main_Code_Prv>0,"Prv",
              ifelse(Job_Main_Code_Buss>0 | Job_Main_Code_Agri>0,
                     "Buss","Other")))]

MD[,weighted.mean(Job_Code==9,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(Job_Main_Code_Pub>0,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(Job_Main_Code_Prv>0,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(Job_Main_Code_Buss>0,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(Job_Main_Code_Agri>0,Weight),by=Decile][order(Decile)]
MD[,weighted.mean(Other_Income_Code>0,Weight),by=Decile][order(Decile)]

MD[,HH_Decile:=sum(Weight),by=Decile]

x1<-MD[Decile==1,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]#[order("Category")]
x2<-MD[Decile==2,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x3<-MD[Decile==3,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x4<-MD[Decile==4,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x5<-MD[Decile==5,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x6<-MD[Decile==6,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x7<-MD[Decile==7,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x8<-MD[Decile==8,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x9<-MD[Decile==9,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x10<-MD[Decile==10,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]


y1<-MD[Decile==1,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]#[order("Job_Code")]
y2<-MD[Decile==2,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y3<-MD[Decile==3,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y4<-MD[Decile==4,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y5<-MD[Decile==5,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y6<-MD[Decile==6,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y7<-MD[Decile==7,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y8<-MD[Decile==8,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y9<-MD[Decile==9,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
y10<-MD[Decile==10,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Job_Code"]
#}


Sale<-MD[Job_Original_Code_Prv==52 | Job_Original_Code_Buss==52]

Sale[,sum(HIndivNo),by=Decile][order(Decile)]
Sale[,sum(HIndivNo*Weight),by=Decile][order(Decile)]

x1<-Sale[Decile==1,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]#[order("Category")]
x2<-Sale[Decile==2,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x3<-Sale[Decile==3,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x4<-Sale[Decile==4,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x5<-Sale[Decile==5,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x6<-Sale[Decile==6,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x7<-Sale[Decile==7,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x8<-Sale[Decile==8,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x9<-Sale[Decile==9,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]
x10<-Sale[Decile==10,.(sum(HIndivNo*Weight)/mean(HH_Decile)),by="Category"]

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])