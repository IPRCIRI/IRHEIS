# T-Test for PLines in Clusters.R
# 
# Copyright © 2019: Arin Shahbazian
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
load("MD4test.rda")

MDH <- MD[ TFoodExpenditure_Per>0.8*FPLine & TFoodExpenditure_Per<1.2*FPLine]

###############Urban###############
#####Cluster 2
a<-MDH[Region=="Urban" & NewArea==707,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=707 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==1002,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=1002 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==3001,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=3001 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==30,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=30 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==2202,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=2202 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 3
a<-MDH[Region=="Urban" & NewArea==23,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=23 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea2=="Gilan",.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea2!="Gilan" & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea2=="Sh_Arak",.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea2!="Sh_Arak" & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==17,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=17 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==2,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=2 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)


a<-MDH[Region=="Urban" & NewArea==916,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=916 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==303,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=303 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==25,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=25 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==808,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=808 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==603,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=603 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==0,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=0 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==22,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=22 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==21,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=21 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==7,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=7 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==26,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=26 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==10,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=10 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==13,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=13 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 4
a<-MDH[Region=="Urban" & NewArea==20,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=20 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==3,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=3 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==8,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=8 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==19,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=19 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==12,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=12 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==18,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=18 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==24,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=24 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==14,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=14 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==9,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=9 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 5
a<-MDH[Region=="Urban" & NewArea==6,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=6 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==16,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=16 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==29,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=29 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==4,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=4 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==401,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=401 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==15,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=15 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==5,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=5 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==502,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=502 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 6
a<-MDH[Region=="Urban" & NewArea==28,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=28 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==27,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=27 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Urban" & NewArea==1105,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=1105 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

###############Rural###############
#####Cluster 1
a<-MDH[Region=="Rural" & NewArea==30,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=30 & cluster3==1,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==23,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=23 & cluster3==1,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 2
a<-MDH[Region=="Rural" & NewArea==2,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=2 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==10,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=10 & cluster3==2,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

###Cluster3
a<-MDH[Region=="Rural" & NewArea==1,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=1 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)


a<-MDH[Region=="Rural" & NewArea==21,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=21 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==17,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=17 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==18,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=18 & cluster3==3,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)


#####Cluster 4
a<-MDH[Region=="Rural" & NewArea==22,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=22 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)


#####Cluster 4
a<-MDH[Region=="Rural" & NewArea==0,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=0 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==3,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=3 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==6,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=6 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==7,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=7 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==12,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=12 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==14,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=14 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==19,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=19 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==20,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=20 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==22,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=22 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==24,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=24 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==25,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=25 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==26,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=26 & cluster3==4,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

#####Cluster 5
a<-MDH[Region=="Rural" & NewArea==5,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=5 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==8,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=8 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==9,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=9 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==13,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=13 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==27,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=27 & cluster3==5,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)


#####Cluster 6
a<-MDH[Region=="Rural" & NewArea==4,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=4 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==15,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=15 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==16,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=16 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==28,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=28 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

a<-MDH[Region=="Rural" & NewArea==29,.(PersonalPLine,cluster3,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=29 & cluster3==6,.(PersonalPLine,cluster3,NewArea2)]
t.test(a$PersonalPLine,b$PersonalPLine, var.equal=TRUE, paired=FALSE)

##############################################
PPH <- MDH[ ,.(.N,PPLine=weighted.mean(PersonalPLine,Weight),
                  FPLine=mean(FPLine)),by=.(Region,NewArea2)]

x2<-PPH[Region=="Rural",.(PPLine,NewArea2)]
x2$NewArea <- factor(x2$NewArea, levels = x2$NewArea[order(x2$PPLine)])
ggplot(x2, aes(x = x2$NewArea, y = x2$PPLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

y2<-PPH[Region=="Urban",.(PPLine,NewArea2)]
y2$NewArea <- factor(y2$NewArea, levels = y2$NewArea[order(y2$PPLine)])
ggplot(y2, aes(x = y2$NewArea, y = y2$PPLine)) + theme_bw() + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")