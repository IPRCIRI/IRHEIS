#Poors properties.R
# 
# Copyright © 2018:Arin Shahbazian
# Licence: GPL-3
# 
rm(list=ls())
starttime <- proc.time()

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(sm)
library(ggplot2)
library(xlsx)
library(haven)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  #Load Data
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"NewFinalPoor.rda"))
  UNewFinalPoor<-NewFinalPoor[Region=="Urban"]
  RNewFinalPoor<-NewFinalPoor[Region=="Rural"]
  
  #Add house properties
  T95P2 <- read_dta("D:/R/T95P2.dta")
  R95P2 <- read_dta("D:/R/R95P2.dta")
  U95P2 <- read_dta("D:/R/U95P2.dta")
  
  CBN95<-merge(NewFinalPoor,T95P2,by =c("HHID"),all.x=TRUE)
  CBN_Urban<-merge(UNewFinalPoor,U95P2,by =c("HHID"),all.x=TRUE)
  CBN_Rural<-merge(RNewFinalPoor,R95P2,by =c("HHID"),all.x=TRUE)
  
  CBN_Poor95<-CBN95[FinalPoor==1]
  CBN_Poor_Urban<-CBN_Urban[FinalPoor==1]
  CBN_Poor_Rural<-CBN_Rural[FinalPoor==1]
  
  
  CBN_NonPoor_Urban<-CBN_Urban[FinalPoor==0]
  CBN_NonPoor_Rural<-CBN_Rural[FinalPoor==0]
  CBN_NonPoor95<-rbind(CBN_NonPoor_Urban,CBN_NonPoor_Rural)

##### HouseOwn #####

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors1,NewArea,Weight)]
HouseOwn1<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
HouseOwn1[,Weight:=NULL]

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors2,NewArea,Weight)]
HouseOwn2<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors3,NewArea,Weight)]
HouseOwn3<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors4,NewArea,Weight)]
HouseOwn4<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors5,NewArea,Weight)]
HouseOwn5<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_NonPoor95[,HouseOwn_Poors6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors6,NewArea,Weight)]
HouseOwn6<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors7:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors7,NewArea,Weight)]
HouseOwn7<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
HouseOwn7[,Weight:=NULL]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors8:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors8,NewArea,Weight)]
HouseOwn8<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors9:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors9,NewArea,Weight)]
HouseOwn9<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors10:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors10,NewArea,Weight)]
HouseOwn10<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors11:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors11,NewArea,Weight)]
HouseOwn11<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn<-CBN_Poor95[,HouseOwn_Poors12:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=NewArea][order(NewArea)]
HouseOwn<-HouseOwn[,.(HouseOwn_Poors12,NewArea,Weight)]
HouseOwn12<-HouseOwn[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

HouseOwn1<-merge(HouseOwn1,HouseOwn2,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn3,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn4,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn5,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn6,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn7,by =c("NewArea"),all.x=TRUE)
HouseOwn1<-merge(HouseOwn1,HouseOwn8,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn9,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn10,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn11,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
HouseOwn1<-merge(HouseOwn1,HouseOwn12,by =c("NewArea"),all.x=TRUE)
HouseOwn1[,Weight:=NULL]
write.xlsx(HouseOwn1, "D:/R/Poors_House2.xlsx",sheetName = "HouseOwn1")


HouseOwn13<-CBN95[,HouseOwn1:=weighted.mean(ifelse(HouseOwn==1,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn13<-HouseOwn13[,.(HouseOwn1,FinalPoor,Weight)]
HouseOwn13<-HouseOwn13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn13[,Weight:=NULL]

HouseOwn14<-CBN95[,HouseOwn2:=weighted.mean(ifelse(HouseOwn==2,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn14<-HouseOwn14[,.(HouseOwn2,FinalPoor,Weight)]
HouseOwn14<-HouseOwn14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn14[,Weight:=NULL]

HouseOwn15<-CBN95[,HouseOwn3:=weighted.mean(ifelse(HouseOwn==3,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn15<-HouseOwn15[,.(HouseOwn3,FinalPoor,Weight)]
HouseOwn15<-HouseOwn15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn15[,Weight:=NULL]

HouseOwn16<-CBN95[,HouseOwn4:=weighted.mean(ifelse(HouseOwn==4,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn16<-HouseOwn16[,.(HouseOwn4,FinalPoor,Weight)]
HouseOwn16<-HouseOwn16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn16[,Weight:=NULL]

HouseOwn17<-CBN95[,HouseOwn5:=weighted.mean(ifelse(HouseOwn==5,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn17<-HouseOwn17[,.(HouseOwn5,FinalPoor,Weight)]
HouseOwn17<-HouseOwn17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn17[,Weight:=NULL]

HouseOwn18<-CBN95[,HouseOwn6:=weighted.mean(ifelse(HouseOwn==6,1,0),Weight),by=FinalPoor][order(FinalPoor)]
HouseOwn18<-HouseOwn18[,.(HouseOwn6,FinalPoor,Weight)]
HouseOwn18<-HouseOwn18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
HouseOwn18[,Weight:=NULL]

HouseOwn2<-merge(HouseOwn13,HouseOwn14,by =c("FinalPoor"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn15,by =c("FinalPoor"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn16,by =c("FinalPoor"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn17,by =c("FinalPoor"),all.x=TRUE)
HouseOwn2<-merge(HouseOwn2,HouseOwn18,by =c("FinalPoor"),all.x=TRUE)
write.xlsx(HouseOwn2, "D:/R/Poors_House2.xlsx",sheetName = "HouseOwn2",append = TRUE)

##### skeleton #####

skeleton<-CBN_NonPoor95[,skeleton_Poors1:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors1,NewArea,Weight)]
skeleton1<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
skeleton1[,Weight:=NULL]

skeleton<-CBN_NonPoor95[,skeleton_Poors2:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors2,NewArea,Weight)]
skeleton2<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors3:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors3,NewArea,Weight)]
skeleton3<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors4:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors4,NewArea,Weight)]
skeleton4<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors5:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors5,NewArea,Weight)]
skeleton5<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors6:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors6,NewArea,Weight)]
skeleton6<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors13:=weighted.mean(ifelse(skeleton==7,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors13,NewArea,Weight)]
skeleton13<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors14:=weighted.mean(ifelse(skeleton==8,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors14,NewArea,Weight)]
skeleton14<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors15:=weighted.mean(ifelse(skeleton==20,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors15,NewArea,Weight)]
skeleton15<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_NonPoor95[,skeleton_Poors16:=weighted.mean(ifelse(skeleton==10,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors16,NewArea,Weight)]
skeleton16<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]


skeleton<-CBN_Poor95[,skeleton_Poors7:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors7,NewArea,Weight)]
skeleton7<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]
skeleton7[,Weight:=NULL]

skeleton<-CBN_Poor95[,skeleton_Poors8:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors8,NewArea,Weight)]
skeleton8<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors9:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors9,NewArea,Weight)]
skeleton9<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors10:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors10,NewArea,Weight)]
skeleton10<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors11:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors11,NewArea,Weight)]
skeleton11<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors12:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors12,NewArea,Weight)]
skeleton12<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors17:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors17,NewArea,Weight)]
skeleton17<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors18:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors18,NewArea,Weight)]
skeleton18<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors19:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors19,NewArea,Weight)]
skeleton19<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton<-CBN_Poor95[,skeleton_Poors20:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=NewArea][order(NewArea)]
skeleton<-skeleton[,.(skeleton_Poors20,NewArea,Weight)]
skeleton20<-skeleton[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(NewArea)]

skeleton1<-merge(skeleton1,skeleton2,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton3,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton4,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton5,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton6,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton13,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton14,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton15,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton16,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton7,by =c("NewArea"),all.x=TRUE)
skeleton1<-merge(skeleton1,skeleton8,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton9,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton10,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton11,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton12,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton17,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton18,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton19,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
skeleton1<-merge(skeleton1,skeleton20,by =c("NewArea"),all.x=TRUE)
skeleton1[,Weight:=NULL]
write.xlsx(skeleton1, "D:/R/Poors_House2.xlsx",sheetName = "skeleton1",append = TRUE)


skeleton13<-CBN95[,skeleton1:=weighted.mean(ifelse(skeleton==1,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton13<-skeleton13[,.(skeleton1,FinalPoor,Weight)]
skeleton13<-skeleton13[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton13[,Weight:=NULL]

skeleton14<-CBN95[,skeleton2:=weighted.mean(ifelse(skeleton==2,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton14<-skeleton14[,.(skeleton2,FinalPoor,Weight)]
skeleton14<-skeleton14[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton14[,Weight:=NULL]

skeleton15<-CBN95[,skeleton3:=weighted.mean(ifelse(skeleton==3,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton15<-skeleton15[,.(skeleton3,FinalPoor,Weight)]
skeleton15<-skeleton15[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton15[,Weight:=NULL]

skeleton16<-CBN95[,skeleton4:=weighted.mean(ifelse(skeleton==4,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton16<-skeleton16[,.(skeleton4,FinalPoor,Weight)]
skeleton16<-skeleton16[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton16[,Weight:=NULL]

skeleton17<-CBN95[,skeleton5:=weighted.mean(ifelse(skeleton==5,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton17<-skeleton17[,.(skeleton5,FinalPoor,Weight)]
skeleton17<-skeleton17[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton17[,Weight:=NULL]

skeleton18<-CBN95[,skeleton6:=weighted.mean(ifelse(skeleton==6,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton18<-skeleton18[,.(skeleton6,FinalPoor,Weight)]
skeleton18<-skeleton18[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton18[,Weight:=NULL]

skeleton19<-CBN95[,skeleton7:=weighted.mean(ifelse(skeleton==7,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton19<-skeleton19[,.(skeleton7,FinalPoor,Weight)]
skeleton19<-skeleton19[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton19[,Weight:=NULL]

skeleton20<-CBN95[,skeleton8:=weighted.mean(ifelse(skeleton==8,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton20<-skeleton20[,.(skeleton8,FinalPoor,Weight)]
skeleton20<-skeleton20[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton20[,Weight:=NULL]

skeleton21<-CBN95[,skeleton10:=weighted.mean(ifelse(skeleton==10,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton21<-skeleton21[,.(skeleton10,FinalPoor,Weight)]
skeleton21<-skeleton21[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton21[,Weight:=NULL]

skeleton22<-CBN95[,skeleton20:=weighted.mean(ifelse(skeleton==20,1,0),Weight),by=FinalPoor][order(FinalPoor)]
skeleton22<-skeleton22[,.(skeleton20,FinalPoor,Weight)]
skeleton22<-skeleton22[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(FinalPoor)]
skeleton22[,Weight:=NULL]

skeleton2<-merge(skeleton13,skeleton14,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton15,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton16,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton17,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton18,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton19,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton20,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton21,by =c("FinalPoor"),all.x=TRUE)
skeleton2<-merge(skeleton2,skeleton22,by =c("FinalPoor"),all.x=TRUE)
write.xlsx(skeleton2, "D:/R/Poors_House2.xlsx",sheetName = "skeleton2",append = TRUE)

}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)