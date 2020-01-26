# Foods Expenditue types.R
# Builds the Food expenditures data.table for households
#
# Copyright © 2020: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(data.table)
library(stringr)
library(ggplot2)
library(readxl)

FoodExpShare <- data.table(Year=NA_integer_,Region=NA_integer_,ProvinceCode=NA_real_,
                           Share1=NA_real_,Share2=NA_real_,
                           Share3=NA_real_,Share4=NA_real_)[0]

FoodMethodShare <- data.table(Year=NA_integer_,Method=NA_real_,
                              Region=NA_character_,share=NA_real_)[0]

for (year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:", year, "\n"))
  load(file = paste0(Settings$HEISProcessedPath, "Y", year, "Merged4CBN.rda"))
  load(file = paste0(Settings$HEISProcessedPath, "Y", year, "TF.rda"))
  
  #TF<-merge(TF,MD[,.(HHID,ProvinceCode,Region,Weight,Size)],all.x = TRUE,by="HHID")
  #TF[,isUrban:=ifelse(Region=="Urban",1,0)]
  #TF[,Region:=NULL]
  #TF2 <- TF[,.(Food_Exp=sum(FoodExpenditure),
   #            ProvinceCode=mean(ProvinceCode),
    #           isUrban=mean(isUrban),
    #           Weight=mean(Weight)),by=.(HHID,BuyingMethod)]
  
  #TF3<-TF2[,.(.N,Sum=sum(Food_Exp*Weight)),by=.(ProvinceCode,BuyingMethod)]
  #TF4<-TF3[BuyingMethod==8]
  
  G1<-TF[BuyingMethod==1]
  G1 <- G1[,lapply(.SD,sum),by=HHID]
  G1[,BuyingMethod:=NULL]
  names(G1)<-c("HHID","FoodExp1")
  
  G2<-TF[BuyingMethod==2]
  G2 <- G2[,lapply(.SD,sum),by=HHID]
  G2[,BuyingMethod:=NULL]
  names(G2)<-c("HHID","FoodExp2")
  
  G3<-TF[BuyingMethod==3]
  G3 <- G3[,lapply(.SD,sum),by=HHID]
  G3[,BuyingMethod:=NULL]
  names(G3)<-c("HHID","FoodExp3")
  
  G4<-TF[BuyingMethod==4]
  G4 <- G4[,lapply(.SD,sum),by=HHID]
  G4[,BuyingMethod:=NULL]
  names(G4)<-c("HHID","FoodExp4")
  
  G5<-TF[BuyingMethod==5]
  G5 <- G5[,lapply(.SD,sum),by=HHID]
  G5[,BuyingMethod:=NULL]
  names(G5)<-c("HHID","FoodExp5")
  
  G6<-TF[BuyingMethod==6]
  G6 <- G6[,lapply(.SD,sum),by=HHID]
  G6[,BuyingMethod:=NULL]
  names(G6)<-c("HHID","FoodExp6")
  
  G7<-TF[BuyingMethod==7]
  G7 <- G7[,lapply(.SD,sum),by=HHID]
  G7[,BuyingMethod:=NULL]
  names(G7)<-c("HHID","FoodExp7")
  
  G8<-TF[BuyingMethod==8]
  G8 <- G8[,lapply(.SD,sum),by=HHID]
  G8[,BuyingMethod:=NULL]
  names(G8)<-c("HHID","FoodExp8")
  
  TTF<-merge(MD[,.(HHID,Region,ProvinceCode,Weight,
                   Size,EqSizeRevOECD,FoodExpenditure)],G1)
  
  TTF<-merge(TTF,G2,all.x = TRUE)
  TTF<-merge(TTF,G3,all.x = TRUE)
  TTF<-merge(TTF,G4,all.x = TRUE)
  TTF<-merge(TTF,G5,all.x = TRUE)
  TTF<-merge(TTF,G6,all.x = TRUE)
  TTF<-merge(TTF,G7,all.x = TRUE)
  TTF<-merge(TTF,G8,all.x = TRUE)
  TTF[is.na(TTF)] <- 0
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  TF<-merge(TF,MD[,.(HHID,Decile,FinalPoor,Region,ProvinceCode,Weight)],all.x = TRUE)
  TF<-TF[Region=="Urban" | Region=="Rural"]
  TF<-TF[ Region=="Rural" ]
    TF[,Method:=ifelse(BuyingMethod==1,"Buying",
              ifelse(BuyingMethod==2,"Home_Production",
              ifelse(BuyingMethod==3 | BuyingMethod==4 | 
              BuyingMethod==5 | BuyingMethod==7,"Against_Service",
              ifelse(BuyingMethod==6,"Agriculture","Free"))))]
    
    TF<-TF[Method!="Buying"]
    TF <- TF[,Total:=sum(FoodExpenditure*Weight),by=.(Region)]
    TF <- TF[,share:=FoodExpenditure/Total]
    X2 <- TF[,.(share=sum(share*Weight)),by=.(Method,Region)]

    
  
  X2[,Year:=year]
  FoodMethodShare <- rbind(FoodMethodShare,X2)

  
   TTF<-merge(TTF,MD[,.(HHID,Decile,FinalPoor)],all.x = TRUE)
  
  TTF[,weighted.mean(FoodExp8/FoodExpenditure),by=ProvinceCode]
  
  X1 <- TTF[,.(Share1=weighted.mean(FoodExp1/FoodExpenditure),
               Share2=weighted.mean(FoodExp2/FoodExpenditure),
               Share3=weighted.mean((FoodExp3+FoodExp4+FoodExp5+FoodExp7)/FoodExpenditure),
               Share4=weighted.mean(FoodExp6/FoodExpenditure)),by=.(Region,ProvinceCode)]
  X1[,Year:=year]
  
  FoodExpShare <- rbind(FoodExpShare,X1)
  #FoodExpShare<-FoodExpShare[ProvinceCode==1]
}

ggplot(FoodMethodShare, aes(fill=Method, y=share, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Types of food preparation")

ggplot(FoodExpShare)+
  geom_line(mapping = aes(x=Year,y=Share1,col=factor(Region)))




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
