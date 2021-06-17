# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================  Indices =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)
library(ggplot2)

X1 <- data.table(Year=NA_integer_,Exp_Per=NA_real_,Exp=NA_real_,Share=NA_real_)

X2 <- data.table(Year=NA_integer_,Exp_Per=NA_real_,Exp=NA_real_,Share=NA_real_,
                 No_Insurance=NA_integer_)

X3 <- data.table(Year=NA_integer_,Exp_Per=NA_real_,Exp=NA_real_,Share=NA_real_,
                 Malek=NA_integer_)

X4 <- data.table(Year=NA_integer_,Exp_Per=NA_real_,Exp=NA_real_,Share=NA_real_,
                 Poor_Area=NA_integer_)




for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"school.rda"))
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy,G125,G1253)],by="HHID")
  MD[,Decile:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  MD<-merge(MD,Deciles,by="HHID")
  MD<-merge(MD,MD10,by="HHID")
  
  MD[,All:=G01+G02+G03+G04+G05+G06+G07+G08+G09+G101+
       G102+G103+G104+G105+G11+G12+G13]
  
  MD[,Malek:=ifelse(tenure=="OwnLandandBuilding" | tenure=="Apartment" ,1,0)]
  
  for (col in c("ego","bathroom","electricity")) 
    MD[is.na(get(col)), (col) := "True"]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
  P1<-P1[Age<=16 & Age>=6,.(HHID,Age,Literate,Student)]
  P1<-P1[Literate==TRUE | Literate==FALSE]
  P1<-P1[,Lit:=ifelse(Literate==FALSE | Student==FALSE,100,1)]
  P<-P1[,lapply(.SD,sum),by="HHID"]
  P<-P[,Knowledge:=ifelse(Lit>=100,0,1)]
  MD<-merge(MD,P[,.(HHID,Knowledge)],all.x = TRUE)
  
  MD[,Low_Calorie:=ifelse(FoodKCaloriesHH_Per<2100,1,0)]
  MD[,Low_Protein:=ifelse(FoodProtein_Per<60,1,0)]
  MD[,No_Insurance:=ifelse(G1253==0,1,0)]
  MD[,House_High_Share:=ifelse(((G041+G042)/All>0.3 & (tenure=="Rented" |
                                                         tenure=="Mortgage") & Region=="Urban") | 
                                 ((G041+G042)/All>0.2 & (tenure=="Rented" |
                                                           tenure=="Mortgage") & Region=="Rural"),1,0)]
  MD[,Tech_low:=ifelse((phone=="False" & cellphone=="False" &
                          computer=="False") | 
                         (phone=="False" & cellphone=="False" &
                            internet=="False") |
                         (phone=="False"  &
                            computer=="False" & internet=="False") |
                         ( cellphone=="False" &
                             computer=="False" & internet=="False"),1,0)]
  MD[,Yakhchal:=ifelse(refrigerator=="True" | freezer=="True" |
                         frez_refrig=="True","True","False")]
  MD[,Equip_low:=ifelse((Yakhchal=="False" & washer=="False" &
                           oven=="False") | 
                          (Yakhchal=="False" & washer=="False" &
                             tvcr=="False") |
                          (Yakhchal=="False"  &
                             oven=="False" & tvcr=="False") |
                          ( washer=="False" &
                              oven=="False" & tvcr=="False") |
                          ( washer=="False" & oven=="False") | 
                          ( washer=="False" & Yakhchal=="False") |
                          ( washer=="False" & tvcr=="False") |
                          ( tvcr=="False" & oven=="False") |
                          ( tvcr=="False" & Yakhchal=="False") |
                          ( Yakhchal=="False" & oven=="False") ,1,0)]
  
MD[,Poor_Area:=ifelse(ProvinceCode==11 | ProvinceCode==6 | ProvinceCode==29 | 
                        ProvinceCode==16,1,0)]

MD[,Total_Exp_Month:=ifelse(Malek==1,Total_Exp_Month-G041-G042,Total_Exp_Month)]
MD[,Total_Exp_Month_Per:=ifelse(Malek==1,Total_Exp_Month_Per-G041/EqSizeOECD-G042/EqSizeOECD,Total_Exp_Month_Per)]

MD[,Total_Exp_Month:=ifelse(No_Insurance==1,Total_Exp_Month,Total_Exp_Month-G1253)]
MD[,Total_Exp_Month_Per:=ifelse(No_Insurance==1,Total_Exp_Month_Per,Total_Exp_Month_Per-G1253/EqSizeOECD)]

  A1<-MD[,.(Exp=weighted.mean(Total_Exp_Month,Weight),
            Exp_Per=weighted.mean(Total_Exp_Month_Per,Weight),
            Share=weighted.mean(G01/All,Weight))]
  
  A2<-MD[,.(Exp=weighted.mean(Total_Exp_Month,Weight),
            Exp_Per=weighted.mean(Total_Exp_Month_Per,Weight),
            Share=weighted.mean(G01/All,Weight)),by=No_Insurance]
  
  A3<-MD[,.(Exp=weighted.mean(Total_Exp_Month,Weight),
            Exp_Per=weighted.mean(Total_Exp_Month_Per,Weight),
            Share=weighted.mean(G01/All,Weight)),by=Malek]
  
  A4<-MD[,.(Exp=weighted.mean(Total_Exp_Month,Weight),
            Exp_Per=weighted.mean(Total_Exp_Month_Per,Weight),
            Share=weighted.mean(G01/All,Weight)),by=Poor_Area]
  
 
 
  A1[,Year:=year]
  X1 <- rbind(X1,A1)
  
  A2[,Year:=year]
  X2 <- rbind(X2,A2)
  
  A3[,Year:=year]
  X3 <- rbind(X3,A3)
  
  A4[,Year:=year]
  X4 <- rbind(X4,A4)
  

 x<- MD[NStudents>0,weighted.mean(internet=="True",Weight),by="CountyCode"]
 x[,Deprivation:=ifelse(V1==0,1,0)]
 MD<-merge(MD,x,by="CountyCode")
 
 MD[,weighted.mean(internet=="True",Weight)]
 MD[Deprivation==0,weighted.mean(internet=="True",Weight)]
 MD[NStudents>0,weighted.mean(internet=="True",Weight)]
 MD[NStudents>0 & Deprivation==0,weighted.mean(internet=="True",Weight)]
 
 MD[,weighted.mean(internet=="True",Weight),by="Region"]
 MD[Deprivation==0,weighted.mean(internet=="True",Weight),by="Region"]
 MD[NStudents>0,weighted.mean(internet=="True",Weight),by="Region"]
 MD[NStudents>0 & Deprivation==0,weighted.mean(internet=="True",Weight),by="Region"]
 
 a<-MD[,weighted.mean(internet=="True",Weight),by="ProvinceName"]
 a<- MD[Deprivation==0,weighted.mean(internet=="True",Weight),by="ProvinceName"]
 a<-MD[NStudents>0,weighted.mean(internet=="True",Weight),by="ProvinceName"]
 a<-MD[NStudents>0 & Deprivation==0,weighted.mean(internet=="True",Weight),by="ProvinceName"]
 
 


 
}
X1<-X1[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]
X2<-X2[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]
X3<-X3[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]
X4<-X4[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])