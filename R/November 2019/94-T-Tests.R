# T-Tests.R
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ T-Tests =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

year<-95
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
MD<-MD[,Engel:=TFoodExpenditure/Total_Exp_Month]


##########Total Households##########
#####Tehran
a<-MD[Region=="Urban" & NewArea==23,.(Engel)]
b<-MD[Region=="Urban" & NewArea==2301,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Mashhad
a<-MD[Region=="Urban" & NewArea==9,.(Engel)]
b<-MD[Region=="Urban" & NewArea==916,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Esfahan
a<-MD[Region=="Urban" & NewArea==10,.(Engel)]
b<-MD[Region=="Urban" & NewArea==1002,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Karaj
a<-MD[Region=="Urban" & NewArea==30,.(Engel)]
b<-MD[Region=="Urban" & NewArea==3001,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Shiraz
a<-MD[Region=="Urban" & NewArea==7,.(Engel)]
b<-MD[Region=="Urban" & NewArea==707,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Tabriz
a<-MD[Region=="Urban" & NewArea==3,.(Engel)]
b<-MD[Region=="Urban" & NewArea==303,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Ahvaz
a<-MD[Region=="Urban" & NewArea==6,.(Engel)]
b<-MD[Region=="Urban" & NewArea==603,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Kermanshah
a<-MD[Region=="Urban" & NewArea==5,.(Engel)]
b<-MD[Region=="Urban" & NewArea==502,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Urmia
a<-MD[Region=="Urban" & NewArea==4,.(Engel)]
b<-MD[Region=="Urban" & NewArea==2301,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=401, paired=FALSE)

#####Rasht
#a<-MD[Region=="Urban" & NewArea==1,.(Engel)]
#b<-MD[Region=="Urban" & NewArea==105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Zahedan
#a<-MD[Region=="Urban" & NewArea==11,.(Engel)]
#b<-MD[Region=="Urban" & NewArea==1105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Kerman
a<-MD[Region=="Urban" & NewArea==8,.(Engel)]
b<-MD[Region=="Urban" & NewArea==808,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Bandarabas
a<-MD[Region=="Urban" & NewArea==22,.(Engel)]
b<-MD[Region=="Urban" & NewArea==2202,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Hamedan
#a<-MD[Region=="Urban" & NewArea==13,.(Engel)]
#b<-MD[Region=="Urban" & NewArea==1304,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Yazd
#a<-MD[Region=="Urban" & NewArea==21,.(Engel)]
#b<-MD[Region=="Urban" & NewArea==2105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Arak
a<-MD[Region=="Urban" & NewArea==0,.(Engel)]
b<-MD[Region=="Urban" & NewArea==1,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

####Engle
EngleP <- MD[ ,.(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,NewArea_Name)]


y<-EngleP[Region=="Urban",.(Engel,NewArea_Name)]
y$NewArea <- factor(y$NewArea, levels = y$NewArea[order(y$Engel)])
ggplot(y, aes(x = y$NewArea, y = y$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


x<-EngleP[Region=="Rural",.(Engel,NewArea_Name)]
x$NewArea <- factor(x$NewArea, levels = x$NewArea[order(x$Engel)])
ggplot(x, aes(x = x$NewArea, y = x$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


##########Near FoodPLine Households##########
MDH <- MD[ TFoodExpenditure_Per>0.8*FPLine & TFoodExpenditure_Per<1.2*FPLine]

#####Tehran
a<-MDH[Region=="Urban" & NewArea==23,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==2301,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Mashhad
a<-MDH[Region=="Urban" & NewArea==9,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==916,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Esfahan
a<-MDH[Region=="Urban" & NewArea==10,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==1002,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Karaj
a<-MDH[Region=="Urban" & NewArea==30,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==3001,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Shiraz
a<-MDH[Region=="Urban" & NewArea==7,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==707,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Tabriz
a<-MDH[Region=="Urban" & NewArea==3,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==303,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Ahvaz
a<-MDH[Region=="Urban" & NewArea==6,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==603,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Kermanshah
a<-MDH[Region=="Urban" & NewArea==5,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==502,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Urmia
a<-MDH[Region=="Urban" & NewArea==4,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==2301,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=401, paired=FALSE)

#####Rasht
#a<-MDH[Region=="Urban" & NewArea==1,.(Engel)]
#b<-MDH[Region=="Urban" & NewArea==105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Zahedan
#a<-MDH[Region=="Urban" & NewArea==11,.(Engel)]
#b<-MDH[Region=="Urban" & NewArea==1105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Kerman
a<-MDH[Region=="Urban" & NewArea==8,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==808,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Bandarabas
a<-MDH[Region=="Urban" & NewArea==22,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==2202,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Hamedan
#a<-MDH[Region=="Urban" & NewArea==13,.(Engel)]
#b<-MDH[Region=="Urban" & NewArea==1304,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Yazd
#a<-MDH[Region=="Urban" & NewArea==21,.(Engel)]
#b<-MDH[Region=="Urban" & NewArea==2105,.(Engel)]
#t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Arak
a<-MDH[Region=="Urban" & NewArea==0,.(Engel)]
b<-MDH[Region=="Urban" & NewArea==1,.(Engel)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

####Engle
EngleH <- MDH[ ,.(.N,Engel=weighted.mean(TFoodExpenditure/Total_Exp_Month,Weight),
                 FPLine=mean(FPLine)),by=.(Region,NewArea_Name)]


x2<-EngleH[Region=="Rural",.(Engel,NewArea_Name)]
x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$Engel)])
ggplot(x2, aes(x = x2$NewArea, y = x2$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

y2<-EngleH[Region=="Urban",.(Engel,NewArea_Name)]
y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$Engel)])
ggplot(y2, aes(x = y2$NewArea, y = y2$Engel)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))




endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")