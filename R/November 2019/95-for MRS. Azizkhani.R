#60-Poors properties.R
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
library(haven)

#for(year in (Settings$startyear:Settings$endyear)){
year<-96
  cat(paste0("\nYear:",year,"\t"))
  
  #Load Data
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"PoorsforMerge.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  
  Total<-merge(MD2,HHHouseProperties,by =c("HHID"),all.x=TRUE)
  Total<-merge(Total,HHBase,by =c("HHID"),all.x=TRUE)
  
  ##### Activity #####Region##########
  Activity<-Total[FinalPoor==1,Activity_Poors1:=weighted.mean(ifelse(ActivityState %in% "Employed",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors1,Region,Weight)]
  Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity1[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors2:=weighted.mean(ifelse(ActivityState %in% "Unemployed",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors2,Region,Weight)]
  Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity2[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors3:=weighted.mean(ifelse(ActivityState %in% "Income without Work",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors3,Region,Weight)]
  Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity3[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors4:=weighted.mean(ifelse(ActivityState %in% "Student",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors4,Region,Weight)]
  Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity4[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors5:=weighted.mean(ifelse(ActivityState %in% "Housekeeper",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors5,Region,Weight)]
  Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity5[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors6:=weighted.mean(ifelse(ActivityState %in% "Other",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors6,Region,Weight)]
  Activity6<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity6[,Weight:=NULL]
  
  Activity1<-merge(Activity1,Activity2,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity3,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity4,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity5,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity6,by =c("Region"),all.x=TRUE)
 # write.xlsx(ActivityPoorsRegion, "D:/R/Poors.xlsx",sheetName = "Activity1")
  
  
  ##### Activity #####Country##########
  Activity<-Total[FinalPoor==1,Activity_Poors1:=weighted.mean(ifelse(ActivityState %in% "Employed",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors1,Region,Weight)]
  Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity1[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors2:=weighted.mean(ifelse(ActivityState %in% "Unemployed",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors2,Region,Weight)]
  Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity2[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors3:=weighted.mean(ifelse(ActivityState %in% "Income without Work",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors3,Region,Weight)]
  Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity3[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors4:=weighted.mean(ifelse(ActivityState %in% "Student",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors4,Region,Weight)]
  Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity4[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors5:=weighted.mean(ifelse(ActivityState %in% "Housekeeper",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors5,Region,Weight)]
  Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity5[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1,Activity_Poors6:=weighted.mean(ifelse(ActivityState %in% "Other",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors6,Region,Weight)]
  Activity6<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity6[,Weight:=NULL]
  
  Activity1<-merge(Activity1,Activity2,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity3,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity4,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity5,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity6,by =c("Region"),all.x=TRUE)
#  write.xlsx(ActivityPoorsRegion, "D:/R/Poors.xlsx",sheetName = "Activity2",append = TRUE)
#########################################################
  #######################################################
  #####################################################
 
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors1:=weighted.mean(ifelse(EduLevel0 %in% "Illiterate",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors1,Region,Weight)]
  Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity1[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors2:=weighted.mean(ifelse(EduLevel0 %in% "Elementary",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors2,Region,Weight)]
  Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity2[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors3:=weighted.mean(ifelse(EduLevel0 %in% "Middle",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors3,Region,Weight)]
  Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity3[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors4:=weighted.mean(ifelse(EduLevel0 %in% "High" | EduLevel0 %in% "Pre",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors4,Region,Weight)]
  Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity4[,Weight:=NULL]
  

  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors5:=weighted.mean(ifelse(EduLevel0 %in% "University",1,0),Weight),by=Region][order(Region)]
  Activity<-Activity[,.(Activity_Poors5,Region,Weight)]
  Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE),by=.(Region)]
  Activity5[,Weight:=NULL]
  
  Activity1<-merge(Activity1,Activity2,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity3,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity4,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity5,by =c("Region"),all.x=TRUE)

  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors1:=weighted.mean(ifelse(EduLevel0 %in% "Illiterate",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors1,Region,Weight)]
  Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity1[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors2:=weighted.mean(ifelse(EduLevel0 %in% "Elementary",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors2,Region,Weight)]
  Activity2<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity2[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors3:=weighted.mean(ifelse(EduLevel0 %in% "Middle",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors3,Region,Weight)]
  Activity3<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity3[,Weight:=NULL]
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors4:=weighted.mean(ifelse(EduLevel0 %in% "High" | EduLevel0 %in% "Pre",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors4,Region,Weight)]
  Activity4<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity4[,Weight:=NULL]
  
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors5:=weighted.mean(ifelse(EduLevel0 %in% "University",1,0),Weight)]
  Activity<-Activity[,.(Activity_Poors5,Region,Weight)]
  Activity5<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity5[,Weight:=NULL]
  
  Activity1<-merge(Activity1,Activity2,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity3,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity4,by =c("Region"),all.x=TRUE)
  Activity1<-merge(Activity1,Activity5,by =c("Region"),all.x=TRUE)
  
  
  Activity<-Total[FinalPoor==1 & ActivityState=="Income without Work",Activity_Poors1:=weighted.mean(ifelse(tenure %in% "OwnLandandBuilding" |
                                                                      tenure %in% "Apartment",1,0),Weight),]
  Activity<-Activity[,.(Activity_Poors1,Region,Weight)]
  Activity1<-Activity[,lapply(.SD,weighted.mean,w=Weight,na.rm = TRUE)]
  Activity1[,Weight:=NULL]
  
 
#}


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
