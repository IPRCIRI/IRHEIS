


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
  inflation <- as.data.table(read_excel("~/GitHub/IRHEIS/Data/ProvinceCPI.xlsx",
                                         sheet = "Province"))
  I<-inflation[Year==year]
  C<-MD[,.(HHID,cluster3,EqSizeCalory)]
load(file = paste0(Settings$HEISProcessedPath,"Y",year,"Total_Income.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Specific.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"POORS.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Job.rda"))
MD<-merge(MD,job,by=("HHID"))
MD<-merge(MD,Specific,by=c("HHID"))
MD<-merge(MD,I,by=c("ProvinceCode","Year","NewArea2"))
MD<-merge(MD,IncomeTable[,.(HHID,NetIncome)],by=c("HHID"))
MD<-MD[FinalPoor==1]
MD[,job:=ifelse((Job_Main_Code_Pub==1 | Job_Main_Code_Prv==1 | Job_Main_Code_Cooperative==1 | Job_Main_Code_Buss==1 | Job_Main_Code_Agri==1),1,0)]
goosht<-MD[,weighted.mean(dandan/Size,Weight*Size),by=c("ProvinceName","FinalPoor")]
MD<-MD[,Job_Main_Code_Pub:=as.numeric(job)]
MD<-MD[,dandan:=tooth_G+tooth_NG+tooth_Jarahi_G+tooth_Jarahi_NG]
MD<-MD[,LivestockExpenditure:=(LivestockExpenditure+BirdsMeatExpenditure)/(EqSizeCalory*r_meat)]
MD<-MD[,SheepMeatExpenditure:=SheepMeatExpenditure/(EqSizeCalory*r_meat)]
MD<-MD[,CowMeatExpenditure:=CowMeatExpenditure/(EqSizeCalory*r_meat)]
MD<-MD[,CamelMeatExpenditure:=CamelMeatExpenditure/(EqSizeCalory*r_meat)]

morgh<-MD[,BirdsMeatExpenditure:=BirdsMeatExpenditure/EqSizeCalory]
morgh<-MD[,weighted.mean(BirdsMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]

goosht<-MD[,weighted.mean(dandan/Size,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht1<-MD[,weighted.mean(Job_Main_Code_Pub+Job_Main_Code_Prv+Job_Main_Code_Cooperative,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht2<-MD[,weighted.mean(CowMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht3<-MD[,weighted.mean(CamelMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]
ggplot(goosht, aes(fill=factor(FinalPoor), y=V1, x=ProvinceName)) + 
  geom_col(position="stack") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


meat<-as.data.table(cbind(goosht$ProvinceName,goosht$FinalPoor,goosht$V1,goosht1$V1,goosht2$V1,goosht3$V1))
Dabestan<-MD[NDabirestan>0 | NDabestan>0 | NRahnamayi>0 | NPish>0 | KelasKonkoor>0]
Dabestan<-Dabestan[,Enrollment:=Enrollment_Dabirestan_G+Enrollment_Dabirestan_NG+Enrollment_Dabirestan_Shabane+
                     Enrollment_Dabestan_G+Enrollment_Dabestan_NG+Enrollment_Rahnamayi_G+Enrollment_Rahnamayi_NG+
                     Enrollment_pish_G+Enrollment_pish_NG+Enrollment_Rahnamayi_Shabane+Enrollment_pish_Shabane+
                     KelasKonkoor]
female<-MD[Size>0]

female<-female[,female:=ifelse(HSex=="Female",1,0)]
female<-female[,weighted.mean(female,Weight*Size,na.rm = T),by=c("ProvinceName","FinalPoor")]
Dabestan1<-MD[,weighted.mean(Enrollment_Dabestan_NG/(Total_Exp_Month_Per*education),Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan2<-MD[,weighted.mean(Enrollment_Rahnamayi_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan3<-MD[,weighted.mean(Enrollment_Rahnamayi_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan4<-MD[,weighted.mean(Enrollment_Dabirestan_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan5<-MD[,weighted.mean(Enrollment_Dabirestan_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan6<-MD[,weighted.mean(KelasKonkoor,Weight*Size),by=c("ProvinceName","FinalPoor")]

edu<-as.data.table(cbind(Dabestan$ProvinceName,Dabestan$FinalPoor,Dabestan$V1,
                         Dabestan1$V1,Dabestan2$V1,Dabestan3$V1,Dabestan4$V1,
                         Dabestan5$V1,Dabestan6$V1))

tooth<-MD[,weighted.mean(tooth_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
tooth<-MD[,weighted.mean(tooth_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]

visit<-MD[,weighted.mean(Visit_Omoomi_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit1<-MD[,weighted.mean(Visit_Omoomi_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit2<-MD[,weighted.mean(Visit_Motekhases_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit3<-MD[,weighted.mean(Visit_Motekhases_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit4<-MD[,weighted.mean(Visit_Mama_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit5<-MD[,weighted.mean(Visit_Mama_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
visit<-as.data.table(cbind(visit$ProvinceName,visit$FinalPoor,visit$V1,visit1$V1,
                     visit2$V1,visit3$V1,visit4$V1,visit5$V1))

phd<-MD[,weighted.mean(NPhD+NMasters,Weight*Size),by=c("ProvinceName","FinalPoor")]
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
p10<-MD[,weighted.mean(Area/Size,Weight*Size),by=c("ProvinceName","FinalPoor")]
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