


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
year<-97

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
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Data.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFData.rda"))
MD<-merge(BigFData,MD,by=c("HHID"),all.x = T)
MD<-MD[FinalPoor==1]
Calory<-MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("ProvinceCode")]
MD<-merge(MD,FoodNames,by=c("FoodType"),all.x = T)
base<-MD[,weighted.mean(FGrams/EqSizeCalory*KCalories,Weight*Size),by=c("ProvinceName","FoodType")]

base<-MD[,fcalory:=FGrams*KCalories/EqSizeCalory]
base<-base[,weighted.mean(V1,Weight*Size),by=c("ProvinceName","FoodType")]
s<-base[,sum(V1),by=c("ProvinceName")]

MD<-MD[,foodpoor:=ifelse(FoodKCaloriesHH<Calorie_Need2,1,0)]
foodpoor<-MD[,weighted.mean(foodpoor,Weight*Size),by=c("ProvinceName","FinalPoor")]
MD3<-MD[,weighted.mean(FoodKCaloriesHH_Per,Weight*Size),by=c("ProvinceCode")]

MD<-MD[,foodpoorW:=ifelse(FoodKCaloriesHH<Calorie_Need1,1,0)]
foodpoorW<-MD[,weighted.mean(foodpoorW,Weight*Size),by=c("ProvinceName","FinalPoor")]

Calory<-MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("ProvinceName","FinalPoor")]
Calory_Decile_Poor<-MD[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("Decile","FinalPoor")]
Calory_Decile<-MD[,weighted.mean(FoodKCaloriesHH_Per,Weight*Size),by=c("Decile")]

Decile123<-MD[Decile==1 | Decile==2 | Decile==3]
Calory_Decile123<-as.data.table(Decile123[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size)])
Calory_Decile123_Poor<-as.data.table(Decile123[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("FinalPoor")])


Decile8910<-MD[Decile==8 | Decile==9 | Decile==10]
Calory_Decile18910<-as.data.table(Decile8910[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size)])
Calory_Decile18910_<-as.data.table(Decile8910[,weighted.mean(TFoodKCaloriesHH_Per,Weight*Size),by=c("FinalPoor")])

Protein<-MD[,weighted.mean(FoodProtein_Per,Weight*Size),by=c("ProvinceName","FinalPoor")]
Protein_Decile_Poor<-MD[,weighted.mean(FoodProtein_Per,Weight*Size),by=c("Decile","FinalPoor")]
Protein_Decile<-MD[,weighted.mean(FoodProtein_Per,Weight*Size),by=c("Decile")]

Decile123<-MD[Decile==1 | Decile==2 | Decile==3]
Protein_Decile123<-as.data.table(Decile123[,weighted.mean(FoodProtein_Per,Weight*Size)])
Protein_Decile123_Poor<-as.data.table(Decile123[,weighted.mean(FoodProtein_Per,Weight*Size),by=c("FinalPoor")])


Decile8910<-MD[Decile==8 | Decile==9 | Decile==10]
Protein_Decile18910<-as.data.table(Decile8910[,weighted.mean(FoodProtein_Per,Weight*Size)])
Protein_Decile18910_<-as.data.table(Decile8910[,weighted.mean(FoodProtein_Per,Weight*Size),by=c("FinalPoor")])

goosht<-BigFData[FoodType=="Goosht"]
goosht<-goosht[,Goosht_Grams:=FGrams]
mahi<-BigFData[FoodType=="Mahi"]
mahi<-mahi[,Mahi_Grams:=FGrams]
morgh<-BigFData[FoodType=="Morgh"]
morgh<-morgh[,Morgh_Grams:=FGrams]
mive<-BigFData[FoodType=="Mive"]
mive<-mive[,Mive_Grams:=FGrams]
nan<-BigFData[FoodType=="Nan"]
nan<-goosht[,Nan_Grams:=FGrams]
sibzamini<-BigFData[FoodType=="Sibzamini"]
sibzamini<-sibzamini[,Sibzamini_Grams:=FGrams]
makarooni<-BigFData[FoodType=="Makarooni"]
makarooni<-makarooni[,Makarooni_Grams:=FGrams]
khoshkbar<-BigFData[FoodType=="Khoshkbar"]
khoshkbar<-khoshkbar[,Khoshkbar_Grams:=FGrams]

berenj<-BigFData[FoodType=="Berenj"]
berenj<-berenj[,Berenj_Grams:=FGrams]
ghand<-BigFData[FoodType=="Ghand"]
ghand<-ghand[,Ghand_Grams:=FGrams]
hoboobat<-BigFData[FoodType=="Hoboobat"]
hoboobat<-hoboobat[,Hoboobat_Grams:=FGrams]
mast<-BigFData[FoodType=="Mast"]
mast<-berenj[,Mast_Grams:=FGrams]
panir<-BigFData[FoodType=="Panir"]
panir<-panir[,Panir_Grams:=FGrams]
roghan<-BigFData[FoodType=="Roghan"]
roghan<-roghan[,Roghan_Grams:=FGrams]
sabzi<-BigFData[FoodType=="Sabzi"]
sabzi<-sabzi[,Sabzi_Grams:=FGrams]
shir<-BigFData[FoodType=="Shir"]
shir<-shir[,Shir_Grams:=FGrams]
tokhmemorgh<-BigFData[FoodType=="Tokhmemorgh"]
tokhmemorgh<-tokhmemorgh[,Tokhmemorgh_Grams:=FGrams]



MD<-merge(MD,goosht[,.(HHID,Goosht_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mahi[,.(HHID,Mahi_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,morgh[,.(HHID,Morgh_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mive[,.(HHID,Mive_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,nan[,.(HHID,Nan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sibzamini[,.(HHID,Sibzamini_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,makarooni[,.(HHID,Makarooni_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,khoshkbar[,.(HHID,Khoshkbar_Grams)],by=c("HHID"),all.x = T)

MD<-merge(MD,berenj[,.(HHID,Berenj_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,ghand[,.(HHID,Ghand_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,hoboobat[,.(HHID,Hoboobat_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,mast[,.(HHID,Mast_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,panir[,.(HHID,Panir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,roghan[,.(HHID,Roghan_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,sabzi[,.(HHID,Sabzi_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,shir[,.(HHID,Shir_Grams)],by=c("HHID"),all.x = T)
MD<-merge(MD,tokhmemorgh[,.(HHID,Tokhmemorgh_Grams)],by=c("HHID"),all.x = T)


for (col in c("Goosht_Grams","Mahi_Grams", "Morgh_Grams", "Mive_Grams", "Nan_Grams", 
              "Sibzamini_Grams", "Makarooni_Grams", "Khoshkbar_Grams","Berenj_Grams",
              "Ghand_Grams","Hoboobat_Grams","Mast_Grams","Panir_Grams","Roghan_Grams",
              "Sabzi_Grams","Shir_Grams","Tokhmemorgh_Grams"
)) 
  MD[is.na(get(col)), (col) := 0]



MD<-merge(MD,job,by=("HHID"))
MD<-merge(MD,Specific,by=c("HHID"))
MD<-merge(MD,I,by=c("ProvinceCode","Year","NewArea_Name"))
MD<-merge(MD,IncomeTable[,.(HHID,NetIncome)],by=c("HHID"))
MD<-MD[FinalPoor==1]
MD[,job:=ifelse((Job_Main_Code_Pub==1 | Job_Main_Code_Prv==1 | Job_Main_Code_Cooperative==1 | Job_Main_Code_Buss==1 | Job_Main_Code_Agri==1),1,0)]
j<-MD[,weighted.mean(job,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht<-MD[,weighted.mean(dandan/Size,Weight*Size),by=c("ProvinceName","FinalPoor")]
MD<-MD[,Job_Main_Code_Pub:=as.numeric(job)]
MD<-MD[,dandan:=tooth_G+tooth_NG+tooth_Jarahi_G+tooth_Jarahi_NG]
MD<-MD[,LivestockExpenditure:=(LivestockExpenditure+BirdsMeatExpenditure)/(EqSizeCalory*r_meat)]
MD<-MD[,SheepMeatExpenditure:=SheepMeatExpenditure/(EqSizeCalory*r_meat)]
MD<-MD[,CowMeatExpenditure:=CowMeatExpenditure/(EqSizeCalory*r_meat)]
MD<-MD[,CamelMeatExpenditure:=CamelMeatExpenditure/(EqSizeCalory*r_meat)]

morgh<-MD[,BirdsMeatExpenditure:=BirdsMeatExpenditure/EqSizeCalory]
morgh<-MD[,weighted.mean(BirdsMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]
MD<-MD[,public:=ifelse(Job_Main_Code_Pub==1 | Job_Main_Code_Prv==1 | Job_Main_Code_Cooperative==1 |Job_Main_Code_Pub==2 | Job_Main_Code_Prv==2 | Job_Main_Code_Cooperative==2,1,0)]
MD<-MD[,pri:=ifelse(Job_Main_Code_Prv==1,1,0)]
MD<-MD[,coop:=ifelse(Job_Main_Code_Cooperative==1,1,0)]

goosht<-MD[,weighted.mean(dandan/Size,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht1<-MD[,weighted.mean(Job_Main_Code_Pub,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht2<-MD[,weighted.mean(CowMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]
goosht3<-MD[,weighted.mean(CamelMeatExpenditure,Weight*Size),by=c("ProvinceName","FinalPoor")]
ggplot(Dabestan1, aes(fill=factor(FinalPoor), y=V1, x=ProvinceName)) + 
  geom_col(position="stack") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


meat<-as.data.table(cbind(goosht$ProvinceName,goosht$FinalPoor,goosht$V1,goosht1$V1,goosht2$V1,goosht3$V1))
Dabestan<-MD[NDabirestan>0 | NDabestan>0 | NRahnamayi>0 | NPish>0 | KelasKonkoor>0]
Dabestan<-Dabestan[,Enrollment:=Enrollment_Dabirestan_G+Enrollment_Dabirestan_NG+Enrollment_Dabirestan_Shabane+
                     Enrollment_Dabestan_G+Enrollment_Dabestan_NG+Enrollment_Rahnamayi_G+Enrollment_Rahnamayi_NG+
                     Enrollment_pish_G+Enrollment_pish_NG+Enrollment_Rahnamayi_Shabane+Enrollment_pish_Shabane+
                     KelasKonkoor]
Dabestan<-MD[NDabestan>0]
Dabestan<-Dabestan[,enrol:=Enrollment_Dabestan_NG/NDabestan]
female<-MD[Size>1]

female<-female[,female:=ifelse(HSex=="Female",1,0)]
female<-female[,weighted.mean(female,Weight*Size,na.rm = T),by=c("ProvinceName","FinalPoor")]
Dabestan2<-Dabestan[,weighted.mean(enrol/Total_Exp_Month_Per,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan2<-MD[,weighted.mean(Enrollment_Rahnamayi_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan3<-MD[,weighted.mean(Enrollment_Rahnamayi_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan4<-MD[,weighted.mean(Enrollment_Dabirestan_G,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan5<-MD[,weighted.mean(Enrollment_Dabirestan_NG,Weight*Size),by=c("ProvinceName","FinalPoor")]
Dabestan6<-MD[,weighted.mean(KelasKonkoor,Weight*Size),by=c("ProvinceName","FinalPoor")]

edu<-as.data.table(cbind(Dabestan1$ProvinceName,Dabestan1$FinalPoor,Dabestan1$V1,
                         Dabestan2$V1))

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

phd<-MD[,weighted.mean(NPhD+NMasters+NBachelors,Weight*Size),by=c("ProvinceName")]
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
Data<-Data[Decile==1 | Decile==2 | Decile==3]
J1<-as.data.table(Data[,weighted.mean(Simple_Jobs_Staff,Weight*Size)])
J2<-as.data.table(Data[,weighted.mean(Opreators_machinery_equipment,Weight*Size)])
J3<-as.data.table(Data[,weighted.mean(Craftsman,Weight*Size)])
J4<-as.data.table(Data[,weighted.mean(Skilled_staff_agriculture_forestr_fishing,Weight*Size)])
J5<-as.data.table(Data[,weighted.mean(Staff_service_sales,Weight*Size)])
J6<-as.data.table(Data[,weighted.mean(Office_staff,Weight*Size)])
J7<-as.data.table(Data[,weighted.mean(Technician,Weight*Size)])
J8<-as.data.table(Data[,weighted.mean(Expert,Weight*Size)])
J9<-as.data.table(Data[,weighted.mean(Manager,Weight*Size)])

J<-rbind(J1,J2,J3,J4,J5,J6,J7,J8,J9)




}
save(PANEL, file=paste0(Settings$HEISProcessedPath,"panelpoor.rda"))
FinalClusterResults<-FinalClusterResults[,cluster3:=as.factor(cluster3)]
FinalClusterResults<-FinalClusterResults[,Year:=as.factor(Year)]
PANEL<-PANEL[,e:=ServiceExp/Total_Exp_Month]
ggplot(data=FinalClusterResults,aes(x=Year, y=pov, group=cluster3, colour=cluster3))+geom_line()

ggplot(data=FinalClusterResults,aes(x=Year, y=PovertyHCR, group=cluster3, colour=cluster3))+geom_line()

ggplot(data=Real,aes(x=Year, y=d))+geom_line()


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
