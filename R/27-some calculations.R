# 26-Total_Exp.R
# 
# Copyright © 2017:Arin Shahbazian
# Licence: GPL-3
# 

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Total =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(reldist)
library(Hmisc)
library(dplyr)
library(data.table)

load(file=paste0(Settings$HEISProcessedPath,"Y","95","MyDataRural.rda"))
load(file=paste0(Settings$HEISProcessedPath,"Y","95","MyDataUrban.rda"))


#Calculate per_Expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$Total_Exp_Month_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$Total_Exp_Month_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$Total_Exp_Month_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$Total_Exp_Month_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$Total_Exp_Month_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$Total_Exp_Month_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$Total_Exp_Month_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$Total_Exp_Month_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$Total_Exp_Month_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$Total_Exp_Month_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$Total_Exp_Month_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$Total_Exp_Month_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$Total_Exp_Month_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$Total_Exp_Month_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$Total_Exp_Month_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$Total_Exp_Month_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$Total_Exp_Month_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$Total_Exp_Month_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$Total_Exp_Month_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$Total_Exp_Month_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$Total_Exp_Month_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$Total_Exp_Month_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$Total_Exp_Month_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$Total_Exp_Month_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$Total_Exp_Month_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$Total_Exp_Month_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$Total_Exp_Month_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$Total_Exp_Month_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$Total_Exp_Month_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$Total_Exp_Month_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$Total_Exp_Month_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$Total_Exp_Month_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$Total_Exp_Month_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$Total_Exp_Month_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$Total_Exp_Month_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$Total_Exp_Month_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$Total_Exp_Month_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$Total_Exp_Month_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$Total_Exp_Month_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$Total_Exp_Month_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$Total_Exp_Month_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$Total_Exp_Month_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$Total_Exp_Month_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$Total_Exp_Month_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$Total_Exp_Month_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$Total_Exp_Month_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$Total_Exp_Month_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$Total_Exp_Month_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$Total_Exp_Month_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$Total_Exp_Month_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$Total_Exp_Month_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$Total_Exp_Month_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$Total_Exp_Month_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$Total_Exp_Month_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$Total_Exp_Month_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$Total_Exp_Month_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$Total_Exp_Month_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$Total_Exp_Month_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$Total_Exp_Month_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$Total_Exp_Month_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$Total_Exp_Month_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$Total_Exp_Month_Per,PU30$Weight,na.rm = TRUE)


#Calculate per_Food_Expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$FoodExpenditure_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$FoodExpenditure_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$FoodExpenditure_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$FoodExpenditure_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$FoodExpenditure_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$FoodExpenditure_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$FoodExpenditure_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$FoodExpenditure_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$FoodExpenditure_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$FoodExpenditure_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$FoodExpenditure_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$FoodExpenditure_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$FoodExpenditure_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$FoodExpenditure_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$FoodExpenditure_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$FoodExpenditure_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$FoodExpenditure_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$FoodExpenditure_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$FoodExpenditure_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$FoodExpenditure_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$FoodExpenditure_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$FoodExpenditure_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$FoodExpenditure_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$FoodExpenditure_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$FoodExpenditure_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$FoodExpenditure_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$FoodExpenditure_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$FoodExpenditure_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$FoodExpenditure_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$FoodExpenditure_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$FoodExpenditure_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$FoodExpenditure_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$FoodExpenditure_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$FoodExpenditure_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$FoodExpenditure_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$FoodExpenditure_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$FoodExpenditure_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$FoodExpenditure_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$FoodExpenditure_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$FoodExpenditure_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$FoodExpenditure_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$FoodExpenditure_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$FoodExpenditure_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$FoodExpenditure_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$FoodExpenditure_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$FoodExpenditure_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$FoodExpenditure_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$FoodExpenditure_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$FoodExpenditure_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$FoodExpenditure_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$FoodExpenditure_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$FoodExpenditure_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$FoodExpenditure_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$FoodExpenditure_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$FoodExpenditure_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$FoodExpenditure_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$FoodExpenditure_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$FoodExpenditure_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$FoodExpenditure_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$FoodExpenditure_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$FoodExpenditure_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$FoodExpenditure_Per,PU30$Weight,na.rm = TRUE)

#Calculate Food_Expenditures share in total expenditures for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$FoodExpenditure_Per/PR0$Total_Exp_Month_Per,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$FoodExpenditure_Per/PR1$Total_Exp_Month_Per,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$FoodExpenditure_Per/PR2$Total_Exp_Month_Per,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$FoodExpenditure_Per/PR3$Total_Exp_Month_Per,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$FoodExpenditure_Per/PR4$Total_Exp_Month_Per,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$FoodExpenditure_Per/PR5$Total_Exp_Month_Per,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$FoodExpenditure_Per/PR6$Total_Exp_Month_Per,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$FoodExpenditure_Per/PR7$Total_Exp_Month_Per,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$FoodExpenditure_Per/PR8$Total_Exp_Month_Per,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$FoodExpenditure_Per/PR9$Total_Exp_Month_Per,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$FoodExpenditure_Per/PR10$Total_Exp_Month_Per,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$FoodExpenditure_Per/PR11$Total_Exp_Month_Per,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$FoodExpenditure_Per/PR12$Total_Exp_Month_Per,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$FoodExpenditure_Per/PR13$Total_Exp_Month_Per,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$FoodExpenditure_Per/PR14$Total_Exp_Month_Per,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$FoodExpenditure_Per/PR15$Total_Exp_Month_Per,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$FoodExpenditure_Per/PR16$Total_Exp_Month_Per,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$FoodExpenditure_Per/PR17$Total_Exp_Month_Per,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$FoodExpenditure_Per/PR18$Total_Exp_Month_Per,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$FoodExpenditure_Per/PR19$Total_Exp_Month_Per,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$FoodExpenditure_Per/PR20$Total_Exp_Month_Per,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$FoodExpenditure_Per/PR21$Total_Exp_Month_Per,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$FoodExpenditure_Per/PR22$Total_Exp_Month_Per,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$FoodExpenditure_Per/PR23$Total_Exp_Month_Per,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$FoodExpenditure_Per/PR24$Total_Exp_Month_Per,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$FoodExpenditure_Per/PR25$Total_Exp_Month_Per,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$FoodExpenditure_Per/PR26$Total_Exp_Month_Per,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$FoodExpenditure_Per/PR27$Total_Exp_Month_Per,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$FoodExpenditure_Per/PR28$Total_Exp_Month_Per,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$FoodExpenditure_Per/PR29$Total_Exp_Month_Per,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$FoodExpenditure_Per/PR30$Total_Exp_Month_Per,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$FoodExpenditure_Per/PU0$Total_Exp_Month_Per,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$FoodExpenditure_Per/PU1$Total_Exp_Month_Per,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$FoodExpenditure_Per/PU2$Total_Exp_Month_Per,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$FoodExpenditure_Per/PU3$Total_Exp_Month_Per,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$FoodExpenditure_Per/PU4$Total_Exp_Month_Per,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$FoodExpenditure_Per/PU5$Total_Exp_Month_Per,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$FoodExpenditure_Per/PU6$Total_Exp_Month_Per,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$FoodExpenditure_Per/PU7$Total_Exp_Month_Per,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$FoodExpenditure_Per/PU8$Total_Exp_Month_Per,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$FoodExpenditure_Per/PU9$Total_Exp_Month_Per,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$FoodExpenditure_Per/PU10$Total_Exp_Month_Per,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$FoodExpenditure_Per/PU11$Total_Exp_Month_Per,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$FoodExpenditure_Per/PU12$Total_Exp_Month_Per,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$FoodExpenditure_Per/PU13$Total_Exp_Month_Per,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$FoodExpenditure_Per/PU14$Total_Exp_Month_Per,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$FoodExpenditure_Per/PU15$Total_Exp_Month_Per,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$FoodExpenditure_Per/PU16$Total_Exp_Month_Per,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$FoodExpenditure_Per/PU17$Total_Exp_Month_Per,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$FoodExpenditure_Per/PU18$Total_Exp_Month_Per,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$FoodExpenditure_Per/PU19$Total_Exp_Month_Per,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$FoodExpenditure_Per/PU20$Total_Exp_Month_Per,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$FoodExpenditure_Per/PU21$Total_Exp_Month_Per,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$FoodExpenditure_Per/PU22$Total_Exp_Month_Per,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$FoodExpenditure_Per/PU23$Total_Exp_Month_Per,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$FoodExpenditure_Per/PU24$Total_Exp_Month_Per,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$FoodExpenditure_Per/PU25$Total_Exp_Month_Per,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$FoodExpenditure_Per/PU26$Total_Exp_Month_Per,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$FoodExpenditure_Per/PU27$Total_Exp_Month_Per,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$FoodExpenditure_Per/PU28$Total_Exp_Month_Per,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$FoodExpenditure_Per/PU29$Total_Exp_Month_Per,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$FoodExpenditure_Per/PU30$Total_Exp_Month_Per,PU30$Weight,na.rm = TRUE)


#Calculate per_Food_Calories for provinces
###Rural
PR0<-subset(MyDataRural, ProvinceCode==0)
weighted.mean(PR0$Per_Daily_Calories,PR0$Weight,na.rm = TRUE)

PR1<-subset(MyDataRural, ProvinceCode==1)
weighted.mean(PR1$Per_Daily_Calories,PR1$Weight,na.rm = TRUE)

PR2<-subset(MyDataRural, ProvinceCode==2)
weighted.mean(PR2$Per_Daily_Calories,PR2$Weight,na.rm = TRUE)

PR3<-subset(MyDataRural, ProvinceCode==3)
weighted.mean(PR3$Per_Daily_Calories,PR3$Weight,na.rm = TRUE)

PR4<-subset(MyDataRural, ProvinceCode==4)
weighted.mean(PR4$Per_Daily_Calories,PR4$Weight,na.rm = TRUE)

PR5<-subset(MyDataRural, ProvinceCode==5)
weighted.mean(PR5$Per_Daily_Calories,PR5$Weight,na.rm = TRUE)

PR6<-subset(MyDataRural, ProvinceCode==6)
weighted.mean(PR6$Per_Daily_Calories,PR6$Weight,na.rm = TRUE)

PR7<-subset(MyDataRural, ProvinceCode==7)
weighted.mean(PR7$Per_Daily_Calories,PR7$Weight,na.rm = TRUE)

PR8<-subset(MyDataRural, ProvinceCode==8)
weighted.mean(PR8$Per_Daily_Calories,PR8$Weight,na.rm = TRUE)

PR9<-subset(MyDataRural, ProvinceCode==9)
weighted.mean(PR9$Per_Daily_Calories,PR9$Weight,na.rm = TRUE)

PR10<-subset(MyDataRural, ProvinceCode==10)
weighted.mean(PR10$Per_Daily_Calories,PR10$Weight,na.rm = TRUE)

PR11<-subset(MyDataRural, ProvinceCode==11)
weighted.mean(PR11$Per_Daily_Calories,PR11$Weight,na.rm = TRUE)

PR12<-subset(MyDataRural, ProvinceCode==12)
weighted.mean(PR12$Per_Daily_Calories,PR12$Weight,na.rm = TRUE)

PR13<-subset(MyDataRural, ProvinceCode==13)
weighted.mean(PR13$Per_Daily_Calories,PR13$Weight,na.rm = TRUE)

PR14<-subset(MyDataRural, ProvinceCode==14)
weighted.mean(PR14$Per_Daily_Calories,PR14$Weight,na.rm = TRUE)

PR15<-subset(MyDataRural, ProvinceCode==15)
weighted.mean(PR15$Per_Daily_Calories,PR15$Weight,na.rm = TRUE)

PR16<-subset(MyDataRural, ProvinceCode==16)
weighted.mean(PR16$Per_Daily_Calories,PR16$Weight,na.rm = TRUE)

PR17<-subset(MyDataRural, ProvinceCode==17)
weighted.mean(PR17$Per_Daily_Calories,PR17$Weight,na.rm = TRUE)

PR18<-subset(MyDataRural, ProvinceCode==18)
weighted.mean(PR18$Per_Daily_Calories,PR18$Weight,na.rm = TRUE)

PR19<-subset(MyDataRural, ProvinceCode==19)
weighted.mean(PR19$Per_Daily_Calories,PR19$Weight,na.rm = TRUE)

PR20<-subset(MyDataRural, ProvinceCode==20)
weighted.mean(PR20$Per_Daily_Calories,PR20$Weight,na.rm = TRUE)

PR21<-subset(MyDataRural, ProvinceCode==21)
weighted.mean(PR21$Per_Daily_Calories,PR21$Weight,na.rm = TRUE)

PR22<-subset(MyDataRural, ProvinceCode==22)
weighted.mean(PR22$Per_Daily_Calories,PR22$Weight,na.rm = TRUE)

PR23<-subset(MyDataRural, ProvinceCode==23)
weighted.mean(PR23$Per_Daily_Calories,PR23$Weight,na.rm = TRUE)

PR24<-subset(MyDataRural, ProvinceCode==24)
weighted.mean(PR24$Per_Daily_Calories,PR24$Weight,na.rm = TRUE)

PR25<-subset(MyDataRural, ProvinceCode==25)
weighted.mean(PR25$Per_Daily_Calories,PR25$Weight,na.rm = TRUE)

PR26<-subset(MyDataRural, ProvinceCode==26)
weighted.mean(PR26$Per_Daily_Calories,PR26$Weight,na.rm = TRUE)

PR27<-subset(MyDataRural, ProvinceCode==27)
weighted.mean(PR27$Per_Daily_Calories,PR27$Weight,na.rm = TRUE)

PR28<-subset(MyDataRural, ProvinceCode==28)
weighted.mean(PR28$Per_Daily_Calories,PR28$Weight,na.rm = TRUE)

PR29<-subset(MyDataRural, ProvinceCode==29)
weighted.mean(PR29$Per_Daily_Calories,PR29$Weight,na.rm = TRUE)

PR30<-subset(MyDataRural, ProvinceCode==30)
weighted.mean(PR30$Per_Daily_Calories,PR30$Weight,na.rm = TRUE)

###Urban
PU0<-subset(MyDataUrban, ProvinceCode==0)
weighted.mean(PU0$Per_Daily_Calories,PU0$Weight,na.rm = TRUE)

PU1<-subset(MyDataUrban, ProvinceCode==1)
weighted.mean(PU1$Per_Daily_Calories,PU1$Weight,na.rm = TRUE)

PU2<-subset(MyDataUrban, ProvinceCode==2)
weighted.mean(PU2$Per_Daily_Calories,PU2$Weight,na.rm = TRUE)

PU3<-subset(MyDataUrban, ProvinceCode==3)
weighted.mean(PU3$Per_Daily_Calories,PU3$Weight,na.rm = TRUE)

PU4<-subset(MyDataUrban, ProvinceCode==4)
weighted.mean(PU4$Per_Daily_Calories,PU4$Weight,na.rm = TRUE)

PU5<-subset(MyDataUrban, ProvinceCode==5)
weighted.mean(PU5$Per_Daily_Calories,PU5$Weight,na.rm = TRUE)

PU6<-subset(MyDataUrban, ProvinceCode==6)
weighted.mean(PU6$Per_Daily_Calories,PU6$Weight,na.rm = TRUE)

PU7<-subset(MyDataUrban, ProvinceCode==7)
weighted.mean(PU7$Per_Daily_Calories,PU7$Weight,na.rm = TRUE)

PU8<-subset(MyDataUrban, ProvinceCode==8)
weighted.mean(PU8$Per_Daily_Calories,PU8$Weight,na.rm = TRUE)

PU9<-subset(MyDataUrban, ProvinceCode==9)
weighted.mean(PU9$Per_Daily_Calories,PU9$Weight,na.rm = TRUE)

PU10<-subset(MyDataUrban, ProvinceCode==10)
weighted.mean(PU10$Per_Daily_Calories,PU10$Weight,na.rm = TRUE)

PU11<-subset(MyDataUrban, ProvinceCode==11)
weighted.mean(PU11$Per_Daily_Calories,PU11$Weight,na.rm = TRUE)

PU12<-subset(MyDataUrban, ProvinceCode==12)
weighted.mean(PU12$Per_Daily_Calories,PU12$Weight,na.rm = TRUE)

PU13<-subset(MyDataUrban, ProvinceCode==13)
weighted.mean(PU13$Per_Daily_Calories,PU13$Weight,na.rm = TRUE)

PU14<-subset(MyDataUrban, ProvinceCode==14)
weighted.mean(PU14$Per_Daily_Calories,PU14$Weight,na.rm = TRUE)

PU15<-subset(MyDataUrban, ProvinceCode==15)
weighted.mean(PU15$Per_Daily_Calories,PU15$Weight,na.rm = TRUE)

PU16<-subset(MyDataUrban, ProvinceCode==16)
weighted.mean(PU16$Per_Daily_Calories,PU16$Weight,na.rm = TRUE)

PU17<-subset(MyDataUrban, ProvinceCode==17)
weighted.mean(PU17$Per_Daily_Calories,PU17$Weight,na.rm = TRUE)

PU18<-subset(MyDataUrban, ProvinceCode==18)
weighted.mean(PU18$Per_Daily_Calories,PU18$Weight,na.rm = TRUE)

PU19<-subset(MyDataUrban, ProvinceCode==19)
weighted.mean(PU19$Per_Daily_Calories,PU19$Weight,na.rm = TRUE)

PU20<-subset(MyDataUrban, ProvinceCode==20)
weighted.mean(PU20$Per_Daily_Calories,PU20$Weight,na.rm = TRUE)

PU21<-subset(MyDataUrban, ProvinceCode==21)
weighted.mean(PU21$Per_Daily_Calories,PU21$Weight,na.rm = TRUE)

PU22<-subset(MyDataUrban, ProvinceCode==22)
weighted.mean(PU22$Per_Daily_Calories,PU22$Weight,na.rm = TRUE)

PU23<-subset(MyDataUrban, ProvinceCode==23)
weighted.mean(PU23$Per_Daily_Calories,PU23$Weight,na.rm = TRUE)

PU24<-subset(MyDataUrban, ProvinceCode==24)
weighted.mean(PU24$Per_Daily_Calories,PU24$Weight,na.rm = TRUE)

PU25<-subset(MyDataUrban, ProvinceCode==25)
weighted.mean(PU25$Per_Daily_Calories,PU25$Weight,na.rm = TRUE)

PU26<-subset(MyDataUrban, ProvinceCode==26)
weighted.mean(PU26$Per_Daily_Calories,PU26$Weight,na.rm = TRUE)

PU27<-subset(MyDataUrban, ProvinceCode==27)
weighted.mean(PU27$Per_Daily_Calories,PU27$Weight,na.rm = TRUE)

PU28<-subset(MyDataUrban, ProvinceCode==28)
weighted.mean(PU28$Per_Daily_Calories,PU28$Weight,na.rm = TRUE)

PU29<-subset(MyDataUrban, ProvinceCode==29)
weighted.mean(PU29$Per_Daily_Calories,PU29$Weight,na.rm = TRUE)

PU30<-subset(MyDataUrban, ProvinceCode==30)
weighted.mean(PU30$Per_Daily_Calories,PU30$Weight,na.rm = TRUE)
