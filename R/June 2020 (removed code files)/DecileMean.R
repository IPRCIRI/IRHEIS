#Decile Mean
#Calculate Mean Expendirure of Different Deciles
#Zahra Shahidi
#2020

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(writexl)
library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  
  MD<-MD[order(Decile)]
  dovom<-MD[Decile==1 | Decile==2]
  nohom<-MD[Decile==9 | Decile==10]
  p1<-MD[,weighted.mean(Total_Exp_Month_Per_nondurable,Weight*Size),by=c("Decile")]
  p2<-MD[,weighted.mean(Total_Exp_Month_Per,Weight*Size),by=c("Decile")]
  p3<-MD[,weighted.mean(Year),by=c("Decile")]
  p4<- MD[,.N,by=Decile]
  p5<-MD[,weighted.mean(Weight),by=c("Decile")]
  p6<-dovom[,weighted.mean(Total_Exp_Month_Per,Weight*Size)]
  p7<-nohom[,weighted.mean(Total_Exp_Month_Per,Weight*Size)]
  nesbat1<-p7/p6
  y<-p2[Decile==1]
  yekom<-y$V1
  d<-p2[Decile==10]
  dahom<-d$V1
  nesbst<-dahom/yekom
  P<-as.data.table(cbind(p1$Decile,p1$V1,p2$V1,p3$V1,nesbst,nesbat1,p5$V1))
  names(P)<-c("Decile","Total_Exp_Month_Per_nondurable","Total_Exp_Month_Per","Year","nesbat","nesbat1","Weight")
  
  save(P, file=paste0(Settings$HEISProcessedPath,"Y",year,"poor.rda"))
  d<-MD[,weighted.mean(Total_Exp_Month,Weight*Size)]
  
  if (year==90){PANEL<-P
  }else{
    PANEL<-rbind(P,PANEL)
  }
}
save(PANEL, file=paste0(Settings$HEISProcessedPath,"Decile"))
PANEL<-PANEL[,Decile:=as.factor(Decile)]
PANEL<-PANEL[,Year:=as.factor(Year)]

PANEL<-as.data.table(PANEL)
write_xlsx(PANEL,path="PANEL1.xlsx",col_names=T)
ggplot(data=PANEL,aes(x=Year, y=fp, group=Decile, colour=Decile))+geom_line()

ggplot(data=PANEL,aes(x=Year, y=poverty_R, group=Decile, colour=Decile))+geom_line()



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")