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


year<-98
  cat(paste0("\nYear:",year,"\t"))

  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))
   MD <- merge(MD,BigEngelTable[Year==year,
                               .(cluster3,Region,
                                 PovertyLine,PovertyLine0,
                                 Engel,ModifiedEngel)],
              by=c("Region","cluster3"))
   MD98<-copy(MD)
   
   year<-97
   cat(paste0("\nYear:",year,"\t"))
   
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))
   MD <- merge(MD,BigEngelTable[Year==year,
                                .(cluster3,Region,
                                  PovertyLine,PovertyLine0,
                                  Engel,ModifiedEngel)],
               by=c("Region","cluster3"))
   MD97<-copy(MD)
   
   year<-96
   cat(paste0("\nYear:",year,"\t"))
   
   load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
   load(file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))
   MD <- merge(MD,BigEngelTable[Year==year,
                                .(cluster3,Region,
                                  PovertyLine,PovertyLine0,
                                  Engel,ModifiedEngel)],
               by=c("Region","cluster3"))
   MD96<-copy(MD)
   
   MD<-rbind(MD98,MD97)
   MD<-rbind(MD,MD96) 
   
   load(file="Inflation9098.rda")
   MD<-merge(MD,Inflation9098,by="ProvinceCode")
   MD[,FPLine:=ifelse(Year==96,FPLine*y9697*y9798,
                 ifelse(Year==97,FPLine*y9798,FPLine))]
   MD[,Total_Exp_Month_Per:=ifelse(Year==96,Total_Exp_Month_Per*y9697*y9798,
                      ifelse(Year==97,Total_Exp_Month_Per*y9798,Total_Exp_Month_Per))]
   MD[,Total_Exp_Month_Per_nondurable:=ifelse(Year==96,Total_Exp_Month_Per_nondurable*y9697*y9798,
                      ifelse(Year==97,Total_Exp_Month_Per_nondurable*y9798,Total_Exp_Month_Per_nondurable))]

   #MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight),by=Year]
   MD[,PovertyLine:=FPLine/ModifiedEngel]
   MD[,PovertyLine0:=FPLine/Engel]
   
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine,1,0 )]
  MD[,FinalPoor0:=ifelse(Total_Exp_Month_Per_nondurable < PovertyLine0,1,0 )]

  A2<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")]

  A2<-A2[,.(PovertyLine,ProvinceName,N)]
  A2$ProvinceName <- factor(A2$ProvinceName, levels = A2$ProvinceName[order(A2$PovertyLine)])
  ggplot(A2, aes(x = A2$ProvinceName, y = A2$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 16000000)

  A4<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("Region","ProvinceName")]
  
  A4<-A4[Region=="Urban",.(PovertyLine,ProvinceName,N)]
  A4$ProvinceName <- factor(A4$ProvinceName, levels = A4$ProvinceName[order(A4$PovertyLine)])
  ggplot(A4, aes(x = A4$ProvinceName, y = A4$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 18000000)

    
  A4<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("Region","ProvinceName")]
  
  A4<-A4[Region=="Rural",.(PovertyLine,ProvinceName,N)]
  A4$ProvinceName <- factor(A4$ProvinceName, levels = A4$ProvinceName[order(A4$PovertyLine)])
  ggplot(A4, aes(x = A4$ProvinceName, y = A4$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 8000000)
  
  ###############################################################
  ##############################################################
  ###############################################################
  
  A2<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("ProvinceName")]
  
  A2<-A2[,.(HCR,ProvinceName,N)]
  A2$ProvinceName <- factor(A2$ProvinceName, levels = A2$ProvinceName[order(A2$HCR)])
  ggplot(A2, aes(x = A2$ProvinceName, y = A2$HCR)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 0.5)#+
   # scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5))
  
  A4<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("Region","ProvinceName")]
  
  A4<-A4[Region=="Urban",.(HCR,ProvinceName,N)]
  A4$ProvinceName <- factor(A4$ProvinceName, levels = A4$ProvinceName[order(A4$HCR)])
  ggplot(A4, aes(x = A4$ProvinceName, y = A4$HCR)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 0.5)
  
  
  A4<-MD[,.(.N,PovertyLine=weighted.mean(PovertyLine,Weight),
            HCR=weighted.mean(FinalPoor,Weight)),by=c("Region","ProvinceName")]
  
  A4<-A4[Region=="Rural",.(HCR,ProvinceName,N)]
  A4$ProvinceName <- factor(A4$ProvinceName, levels = A4$ProvinceName[order(A4$HCR)])
  ggplot(A4, aes(x = A4$ProvinceName, y = A4$HCR)) + theme_bw() + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
    geom_text(aes(label=N),angle=90,vjust=0, hjust=-0.03) + ylim(0, 0.5)
  
    
  
  

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")