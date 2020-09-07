# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Indices =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)

X1 <- data.table(Year=NA_integer_,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_
                ,Subsidy=NA_real_,Food_Share=NA_real_,
                FoodKCaloriesHH_Per=NA_real_)

X2 <- data.table(Year=NA_integer_,Region=NA_character_
                 ,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_
                 ,Subsidy=NA_real_,Food_Share=NA_real_,
                 FoodKCaloriesHH_Per=NA_real_)

X3 <- data.table(Year=NA_integer_,ProvinceCode=NA_integer_
                 ,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_
                 ,Subsidy=NA_real_,Food_Share=NA_real_,
                 FoodKCaloriesHH_Per=NA_real_)

X4 <- data.table(Year=NA_integer_,Decile=NA_integer_
                 ,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_
                 ,Subsidy=NA_real_,Food_Share=NA_real_,
                 FoodKCaloriesHH_Per=NA_real_)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy)],by="HHID")
  MD[,Decile:=NULL]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  MD<-merge(MD,Deciles,by="HHID")
  
  MD[,All:=G01+G02+G03+G04+G05+G06+G07+G08+G09+G101+
     G102+G103+G104+G105+G11+G12+G13]
  
  for (col in c("ego","bathroom","electricity")) 
    MD[is.na(get(col)), (col) := "True"]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
  P1<-P1[Age<=16 & Age>=6,.(HHID,Age,Literate,Student)]
  P1<-P1[Literate==TRUE | Literate==FALSE]
  P1<-P1[,Lit:=ifelse(Literate==FALSE | Student==FALSE,100,1)]
  P<-P1[,lapply(.SD,sum),by="HHID"]
  P<-P[,Knowledge:=ifelse(Lit>=100,0,1)]
  MD<-merge(MD,P[,.(HHID,Knowledge)],all.x = TRUE)
  
  A1<-MD[,.(HLiterate=weighted.mean(HLiterate==TRUE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='True',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="True",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="True",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="True",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="True",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="True",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel=="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel=="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater=="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE))]
  
  A2<-MD[,.(HLiterate=weighted.mean(HLiterate==TRUE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='True',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="True",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="True",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="True",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="True",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="True",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel=="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel=="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater=="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE)),by=Region]
  
  A3<-MD[,.(HLiterate=weighted.mean(HLiterate==TRUE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='True',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="True",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="True",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="True",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="True",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="True",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel=="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel=="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater=="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE)),by=ProvinceCode]

  A4<-MD[,.(HLiterate=weighted.mean(HLiterate==TRUE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='True',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="True",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="True",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="True",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="True",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="True",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel=="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel=="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater=="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE)),by=Decile]
  
  A1[,Year:=year]
  X1 <- rbind(X1,A1)
  
  A2[,Year:=year]
  X2 <- rbind(X2,A2)
  
  A3[,Year:=year]
  X3 <- rbind(X3,A3)
  
  A4[,Year:=year]
  X4 <- rbind(X4,A4)
  
}

#write.csv(X1,file="X1.csv")
#write.csv(X2,file="X2.csv")
#write.csv(X3,file="X3.csv")
#write.csv(X4,file="X4.csv")


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])