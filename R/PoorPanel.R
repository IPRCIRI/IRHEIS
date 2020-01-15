


rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Line =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/inflation.xlsx",col_names = T))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"InitialPoorClustered.rda"))
  C<-MD[,.(HHID,cluster3,EqSizeCalory)]
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"SMD.rda"))
SMD<-SMD[,.(HHID,PriceIndex)]
MD<-merge(MD,SMD,by=c("HHID"))
inflation<-inflation[Year==year]
cpi<-inflation$CPI
cpi<-as.numeric(cpi)
MD<-MD[,Medical_Exp1:=Medical_Exp/cpi]
MD<-MD[,Medical_Exp:=Medical_Exp1/EqSizeOECD]
MD<-MD[,Total_Exp_Month1:=Total_Exp_Month/cpi]
MD<-MD[,Total_Exp_Month:=Total_Exp_Month1/EqSizeOECD]
MD<-MD[,Durable_Exp1:=Durable_Exp/cpi]
MD<-MD[,Durable_Exp:=Durable_Exp1/EqSizeOECD]
MD<-merge(MD,MD1,by=c("HHID"))
MD<-merge(MD,C,by=c("HHID"))
MD<-MD[FinalPoor==1]
MD<-MD[,poverty_R:=weighted.mean(Total_Exp_Month,Weight*Size)]
MD<-MD[!is.na(Area)]
MD<-MD[,House_PL:=Size*10]
MD<-MD[,HousePoor:=ifelse(Area<House_PL,1,0)]
H<-MD[,weighted.mean(HousePoor,Weight*Size),by=c("Year")]
MD<-MD[order(cluster3)]
p1<-MD[,weighted.mean(Total_Exp_Month,Weight*Size),by=c("cluster3")]
p2<-MD[,weighted.mean(FoodKCaloriesHH/EqSizeCalory,Weight*Size),by=c("cluster3")]
p3<-MD[,weighted.mean(FoodProteinHH,Weight*Size),by=c("cluster3")]
p4<-MD[,weighted.mean(FoodExpenditure,Weight*Size),by=c("cluster3")]
p5<-MD[,weighted.mean(Cigar_Exp,Weight*Size),by=c("cluster3")]
p6<-MD[,weighted.mean(Cloth_Exp,Weight*Size),by=c("cluster3")]
p7<-MD[,weighted.mean(Amusement_Exp,Weight*Size),by=c("cluster3")]
p8<-MD[,weighted.mean(Education_Exp,Weight*Size),by=c("cluster3")]
p9<-MD[,weighted.mean(ServiceExp,Weight*Size),by=c("cluster3")]
p10<-MD[,weighted.mean(Area,Weight*Size),by=c("cluster3")]
p11<-MD[,weighted.mean(MetrPrice,Weight*Size),by=c("cluster3")]
p12<-MD[,weighted.mean(ServiceExp,Weight*Size),by=c("cluster3")]
p13<-MD[,weighted.mean(Furniture_Exp,Weight*Size),by=c("cluster3")]
p14<-MD[,weighted.mean(HotelRestaurant_Exp,Weight*Size),by=c("cluster3")]
p15<-MD[,weighted.mean(Hygiene_Exp,Weight*Size),by=c("cluster3")]
p16<-MD[,weighted.mean(Transportation_Exp,Weight*Size),by=c("cluster3")]
p17<-MD[,weighted.mean(Durable_Exp,Weight*Size),by=c("cluster3")]
p18<-MD[,weighted.mean(Total_Exp_Month_nondurable,Weight*Size),by=c("cluster3")]
p19<-MD[,weighted.mean(Medical_Exp,Weight*Size),by=c("cluster3")]
p20<-MD[,weighted.mean(Year),by=c("cluster3")]
p21<- MD[,.N,by=cluster3]
p22<-MD[,weighted.mean(EqSizeOECD,Weight),by=c("cluster3")]
p23<-MD[,weighted.mean(HousePoor,Weight*Size),by=c("cluster3")]
P<-as.data.table(cbind(p1$cluster3,p1$V1,p2$V1,p3$V1,p4$V1,p5$V1,p6$V1,p7$V1,p8$V1,p9$V1,
                       p10$V1,p11$V1,p12$V1,p13$V1,p14$V1,p15$V1,p16$V1,p17$V1,p18$V1,p19$V1,p20$V1,p21$N,p22$V1,p23$V1))
names(P)<-c("cluster3","Total_Exp_Month","FoodKCaloriesHH","FoodProteinHH","FoodExpenditure",
            "Cigar_Exp","Cloth_Exp","Amusement_Exp","Education_Exp","HouseandEnergy_Exp",
            "Area","MetrPrice","ServiceExp","Furniture_Exp","HotelRestaurant_Exp","Hygiene_Exp",
            "Transportation_Exp","Durable_Exp","Total_Exp_Month_nondurable","Medical_Exp","Year","size","E_Scale","HousePoverty")

save(P, file=paste0(Settings$HEISProcessedPath,"Y",year,"poor.rda"))
d<-MD[,weighted.mean(Total_Exp_Month,Weight*Size)]

if (year==77){PANEL<-P
HP<-H
d<-as.data.table(d)
d<-d[,Year:=year]
Real<-d

}else{
  PANEL<-rbind(P,PANEL)
  HP<-rbind(HP,H)
  d<-as.data.table(d)
  d<-d[,Year:=year]
  Real<-rbind(Real,d)
}
}
save(PANEL, file=paste0(Settings$HEISProcessedPath,"panelpoor.rda"))
PANEL<-PANEL[,cluster3:=as.factor(cluster3)]
PANEL<-PANEL[,Year:=as.factor(Year)]
PANEL<-PANEL[,e:=ServiceExp/Total_Exp_Month]
ggplot(data=PANEL,aes(x=Year, y=Medical_Exp, group=cluster3, colour=cluster3))+geom_line()

ggplot(data=PANEL,aes(x=Year, y=poverty_R, group=cluster3, colour=cluster3))+geom_line()

ggplot(data=Real,aes(x=Year, y=d))+geom_line()


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")