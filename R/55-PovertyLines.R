# 55-PovertyLines.R
# 
# Copyright © 2018:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Prepare Data =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
  
  EngleNewUrban1 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Urban" & cluster==1]
  EngleNewUrban2 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Urban" & cluster==2]
  EngleNewUrban3 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Urban" & cluster==3]
  EngleNewUrban4 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Urban" & cluster==4]
  EngleNewUrban<-rbind(EngleNewUrban1,EngleNewUrban2,EngleNewUrban3,EngleNewUrban4)
  save(EngleNewUrban,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUrban.rda"))
  
  
  EngleNewRural1 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Rural" & cluster==1]
  EngleNewRural2 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Rural" & cluster==2]
  EngleNewRural3 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Rural" & cluster==3]
  EngleNewRural4 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Rural" & cluster==4]
  EngleNewRural5 <- MD[TFoodExpenditure_Per<1.1*FPLine & TFoodExpenditure_Per>0.9*FPLine & Region=="Rural" & cluster==5]
  EngleNewRural<-rbind(EngleNewRural1,EngleNewRural2,EngleNewRural3,EngleNewRural4,EngleNewRural5)
  save(EngleNewRural,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewRural.rda"))
  
  EngleNewUnderUrban1 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Urban" & cluster==1]
  EngleNewUnderUrban2 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Urban" & cluster==2]
  EngleNewUnderUrban3 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Urban" & cluster==3]
  EngleNewUnderUrban4 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Urban" & cluster==4]
  EngleNewUnderUrban<-rbind(EngleNewUnderUrban1,EngleNewUnderUrban2,EngleNewUnderUrban3,EngleNewUnderUrban4)
  save(EngleNewUnderUrban,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUnderUrban.rda"))
  
  EngleNewUnderRural1 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Rural" & cluster==1]
  EngleNewUnderRural2 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Rural" & cluster==2]
  EngleNewUnderRural3 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Rural" & cluster==3]
  EngleNewUnderRural4 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Rural" & cluster==4]
  EngleNewUnderRural5 <- MD[TFoodExpenditure_Per<0.9*FPLine & Region=="Rural" & cluster==5]
  EngleNewUnderRural<-rbind(EngleNewUnderRural1,EngleNewUnderRural2,EngleNewUnderRural3,EngleNewUnderRural4,EngleNewUnderRural5)
  save(EngleNewUnderRural,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewUnderRural.rda"))
  
  EngleNewAboveUrban1 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Urban" & cluster==1]
  EngleNewAboveUrban2 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Urban" & cluster==2]
  EngleNewAboveUrban3 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Urban" & cluster==3]
  EngleNewAboveUrban4 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Urban" & cluster==4]
  EngleNewAboveUrban<-rbind(EngleNewAboveUrban1,EngleNewAboveUrban2,EngleNewAboveUrban3,EngleNewAboveUrban4)
  save(EngleNewAboveUrban,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewAboveUrban.rda"))
  
  EngleNewAboveRural1 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Rural" & cluster==1]
  EngleNewAboveRural2 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Rural" & cluster==2]
  EngleNewAboveRural3 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Rural" & cluster==3]
  EngleNewAboveRural4 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Rural" & cluster==4]
  EngleNewAboveRural5 <- MD[TFoodExpenditure_Per>1.1*FPLine  & Region=="Rural" & cluster==5]
  EngleNewAboveRural<-rbind(EngleNewAboveRural1,EngleNewAboveRural2,EngleNewAboveRural3,EngleNewAboveRural4,EngleNewAboveRural5)
  save(EngleNewAboveRural,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngleNewAboveRural.rda"))
  
  EngleD <- MD[ TFoodExpenditure_Per>0.9*FPLine & TFoodExpenditure_Per<1.1*FPLine,
               .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,cluster)]
  EngleP <- MD[ ,
                .(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                  FPLine=mean(FPLine)),by=.(Region,NewArea2)]
  save(EngleP,file = "EngleP.rda")
  EngleD[,PovertyLine:=FPLine/Engel]
  EngleP[,PovertyLine:=FPLine/Engel]
  MD <- merge(MD,EngleD[,.(cluster,Region,PovertyLine,Engel)],by=c("Region","cluster"))
  MD<-MD[Region=="Urban" & NewArea==2301]
  MD[,FinalPoor:=ifelse(Total_Exp_Month_Per < PovertyLine,1,0 )]
  cat(MD[,weighted.mean(FinalPoor,Weight*Size)],"\t",
      MD[,weighted.mean(PovertyLine,Weight*Size)],"\t",
      MD[,weighted.mean(Engel,Weight*Size)],"\t",
      MD[,weighted.mean(FPLine,Weight*Size)])
  MD[,weighted.mean(FinalPoor,Weight*Size),by=c("Region","NewArea")][order(Region,NewArea)]
  MD[,weighted.mean(FinalPoor,Weight),by=c("Region")]
  save(MD,file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
}

x<-EngleP[Region=="Rural",.(Engel,NewArea2)]
x$NewArea <- factor(x$NewArea, levels = x$NewArea[order(x$Engel)])
ggplot(x, aes(x = x$NewArea, y = x$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

y<-EngleP[Region=="Urban",.(Engel,NewArea2)]
y$NewArea <- factor(y$NewArea, levels = y$NewArea[order(y$Engel)])
ggplot(y, aes(x = y$NewArea, y = y$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


x2<-EngleP[Region=="Rural",.(FPLine,NewArea2)]
x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$FPLine)])
ggplot(x2, aes(x = x2$NewArea, y = x2$FPLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

y2<-EngleP[Region=="Urban",.(FPLine,NewArea2)]
y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$FPLine)])
ggplot(y2, aes(x = y2$NewArea, y = y2$FPLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

x3<-EngleP[Region=="Rural",.(PovertyLine,NewArea2)]
x3$NewArea <- factor(x3$NewArea, levels = x3$NewArea[order(x3$PovertyLine)])
ggplot(x3, aes(x = x3$NewArea, y = x3$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

y3<-EngleP[Region=="Urban",.(PovertyLine,NewArea2)]
y3$NewArea <- factor(y3$NewArea, levels = y3$NewArea[order(y3$PovertyLine)])
ggplot(y3, aes(x = y3$NewArea, y = y3$PovertyLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

#NewFinalPoor<-MD[,.(HHID,Region,NewArea,cluster,FinalPoor)]
NewFinalPoor<-MD[,.(HHID,Region,NewArea,cluster,Weight,HAge,HSex,
                    ProvinceCode,Size,HLiterate,HEduLevel0,Area,
                    Rooms,MetrPrice, HActivityState,FinalPoor)]
                   
save(NewFinalPoor,file=paste0(Settings$HEISProcessedPath,"Y",year,"NewFinalPoor.rda"))
Final<-NewFinalPoor[,.(HHID,Region,Weight,FinalPoor)]
save(Final,file=paste0(Settings$HEISProcessedPath,"Y",year,"Final.rda"))

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")