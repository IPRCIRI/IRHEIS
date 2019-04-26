# T-Test in Clusters.R
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
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalFoodPoor.rda"))
MD<-MD[,Engel:=TFoodExpenditure/Total_Exp_Month]

MDH <- MD[ TFoodExpenditure_Per>0.8*FPLine & TFoodExpenditure_Per<1.2*FPLine]

###############Rural###############
#####Cluster 1
a<-MDH[Region=="Rural" & NewArea==30,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=30 & cluster2==1,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 2
a<-MDH[Region=="Rural" & NewArea==2,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=2 & cluster2==2,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 3
a<-MDH[Region=="Rural" & NewArea==18,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=18 & cluster2==3,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 4
a<-MDH[Region=="Rural" & NewArea==12,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=12 & cluster2==4,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 5
a<-MDH[Region=="Rural" & NewArea==8,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Rural" & NewArea!=8 & cluster2==5,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)


###############Urban###############
#####Cluster 2
a<-MDH[Region=="Urban" & NewArea==3001,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=3001 & cluster2==2,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 3
a<-MDH[Region=="Urban" & NewArea==3,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=3 & cluster2==3,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)



#####Cluster 5
a<-MDH[Region=="Urban" & NewArea==9,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=9 & cluster2==5,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

#####Cluster 4
a<-MDH[Region=="Urban" & NewArea==4,.(Engel,cluster2,NewArea2)]
b<-MDH[Region=="Urban" & NewArea!=4 & cluster2==4,.(Engel,cluster2,NewArea2)]
t.test(a$Engel,b$Engel, var.equal=TRUE, paired=FALSE)

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")