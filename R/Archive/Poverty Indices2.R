# 111-HHBase.R
# Builds the base data.table for households

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Poverty Indices2 =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(foreign)
library(data.table)
library(stringr)
library(ggplot2)

X1 <- data.table(Year=NA_integer_,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_,Energy_Share=NA_real_
                ,Subsidy=NA_real_,Food_Share=NA_real_,Low_Calorie=NA_real_,Low_Protein=NA_real_,
                FoodKCaloriesHH_Per=NA_real_,No_Insurance=NA_real_,Area_Per=NA_real_
                ,skeleton=NA_real_,House_High_Share=NA_real_,Tech_low=NA_real_,Equip_low=NA_real_)

X2 <- data.table(Year=NA_integer_,Region=NA_character_
                 ,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_,Energy_Share=NA_real_
                 ,Subsidy=NA_real_,Food_Share=NA_real_,Low_Calorie=NA_real_,Low_Protein=NA_real_,
                 FoodKCaloriesHH_Per=NA_real_,No_Insurance=NA_real_,Area_Per=NA_real_
                 ,skeleton=NA_real_,House_High_Share=NA_real_,Tech_low=NA_real_,Equip_low=NA_real_)


X3 <- data.table(Year=NA_integer_,Decile=NA_integer_
                 ,HLiterate=NA_real_,Knowledge=NA_real_
                 ,tenure=NA_real_,area=NA_real_,room=NA_real_
                 ,car=NA_real_,oven=NA_real_,frez_refrig=NA_real_
                 ,electricity=NA_real_,bathroom=NA_real_,ego=NA_real_,pipewater=NA_real_
                 ,cookfuel=NA_real_,heatfuel=NA_real_,hotwater=NA_real_
                 ,G0451=NA_real_,G0452=NA_real_,G0453=NA_real_,G044=NA_real_,Energy_Share=NA_real_
                 ,Subsidy=NA_real_,Food_Share=NA_real_,Low_Calorie=NA_real_,Low_Protein=NA_real_,
                 FoodKCaloriesHH_Per=NA_real_,No_Insurance=NA_real_,Area_Per=NA_real_
                 ,skeleton=NA_real_,House_High_Share=NA_real_,Tech_low=NA_real_,Equip_low=NA_real_)

Province<-data.table(Year=NA_integer_,ProvinceCode=NA_integer_,Total_Exp_Month_Per=NA_real_)
  
Y1 <- data.table(Year=NA_integer_,H=NA_real_)

Y2 <- data.table(Year=NA_integer_,H=NA_real_,Region=NA_character_)

Y3 <- data.table(Year=NA_integer_,H=NA_real_,ProvinceCode=NA_integer_)

Z1 <- data.table(Year=NA_integer_,A=NA_real_)

Z2 <- data.table(Year=NA_integer_,A=NA_real_,Region=NA_character_)

Z3 <- data.table(Year=NA_integer_,A=NA_real_,ProvinceCode=NA_integer_)

Q1 <- data.table(Year=NA_integer_,MPI=NA_real_)

Q2 <- data.table(Year=NA_integer_,MPI=NA_real_,Region=NA_character_)

Q3 <- data.table(Year=NA_integer_,MPI=NA_real_,ProvinceCode=NA_integer_)
  
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total2.rda"))
  MD<-merge(MD,Total[,.(HHID,G01,G02,G03,G04,G05,G06,G07,G08,G09,G101,
                        G102,G103,G104,G105,G11,G12,G13,G041,G042,G044,G045,
                        G0451,G0452,G0453,G0454,Subsidy,G125,G1253)],by="HHID")
  MD[,Decile:=NULL]
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles.rda"))
  if (year==98){
    names(Deciles)<-c("HHID","Decile","Percentile")
  }

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

  
  
  MD[,Low_Calorie:=ifelse(FoodKCaloriesHH_Per<2100,1,0)]
  MD[,Low_Protein:=ifelse(FoodProtein_Per<60,1,0)]
  MD[,No_Insurance:=ifelse(G1253==0,1,0)]
  MD[,House_High_Share:=ifelse(((G041+G042)/All>0.3 & (tenure=="Rented" |
                                  tenure=="Mortgage") & Region=="Urban") | 
                                 ((G041+G042)/All>0.2 & (tenure=="Rented" |
                                  tenure=="Mortgage") & Region=="Rural"),1,0)]
  if (year==90){
    MD[,phone:=ifelse(is.na(phone),"True","False")]
    MD[,internet:=ifelse(is.na(internet),"True","False")]   
  }

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
  
  School<-MD[,.(HHID,Knowledge,No_Insurance,bathroom,pipewater)]
  save(School,file=paste0(Settings$HEISProcessedPath,"Y",year,"tahsil.rda"))
  
  A4<-MD[,.(Total_Exp_Month_Per=weighted.mean(Total_Exp_Month_Per,Weight,na.rm=TRUE)),by=ProvinceCode]
  
  A1<-MD[,.(HLiterate=weighted.mean(HLiterate==FALSE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge==0,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='False',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="False",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="False",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="False",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="False",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="False",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel!="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel!="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater!="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Energy_Share=weighted.mean((G044+G045)/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            Low_Calorie=weighted.mean(Low_Calorie,Weight,na.rm=TRUE),
            Low_Protein=weighted.mean(Low_Protein,Weight,na.rm=TRUE),
            No_Insurance=weighted.mean(No_Insurance,Weight,na.rm=TRUE),
            Area_Per=weighted.mean((area/Size)<16,Weight,na.rm=TRUE),
            House_High_Share=weighted.mean(House_High_Share,Weight,na.rm=TRUE),
            Tech_low=weighted.mean(Tech_low,Weight,na.rm=TRUE),
            Equip_low=weighted.mean(Equip_low,Weight,na.rm=TRUE),
            skeleton=1-weighted.mean(skeleton=="metal" | skeleton=="concrete" |
                                     constmat=="BrickSteel_StoneSteel" | 
                                     constmat=="Brickwood_Stonewood" | 
                                     constmat=="CementBlocks" | 
                                     constmat=="AllBrick_Stone" ,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE))]
  
  A2<-MD[,.(HLiterate=weighted.mean(HLiterate==FALSE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge==0,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='False',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="False",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="False",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="False",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="False",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="False",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel!="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel!="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater!="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Energy_Share=weighted.mean((G044+G045)/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            Low_Calorie=weighted.mean(Low_Calorie,Weight,na.rm=TRUE),
            Low_Protein=weighted.mean(Low_Protein,Weight,na.rm=TRUE),
            No_Insurance=weighted.mean(No_Insurance,Weight,na.rm=TRUE),
            Area_Per=weighted.mean((area/Size)<16,Weight,na.rm=TRUE),
            House_High_Share=weighted.mean(House_High_Share,Weight,na.rm=TRUE),
            Tech_low=weighted.mean(Tech_low,Weight,na.rm=TRUE),
            Equip_low=weighted.mean(Equip_low,Weight,na.rm=TRUE),
            skeleton=1-weighted.mean(skeleton=="metal" | skeleton=="concrete" |
                                       constmat=="BrickSteel_StoneSteel" | 
                                       constmat=="Brickwood_Stonewood" | 
                                       constmat=="CementBlocks" | 
                                       constmat=="AllBrick_Stone" ,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE)),by=Region]
  

  A3<-MD[,.(HLiterate=weighted.mean(HLiterate==FALSE,Weight,na.rm=TRUE),
            Knowledge=weighted.mean(Knowledge==0,Weight,na.rm=TRUE),
            tenure=weighted.mean(tenure=="OwnLandandBuilding" | tenure=="Apartment",Weight,na.rm=TRUE),
            area=weighted.mean(area/Size,Weight,na.rm=TRUE),
            room=weighted.mean(room/Size,Weight,na.rm=TRUE),
            car=weighted.mean(car=='False',Weight,na.rm=TRUE),
            frez_refrig=weighted.mean(frez_refrig=="True" | refrigerator=="True",Weight,na.rm=TRUE),
            oven=weighted.mean(oven=="False",Weight,na.rm=TRUE),
            bathroom=weighted.mean(bathroom=="False",Weight,na.rm=TRUE),
            ego=weighted.mean(ego=="False",Weight,na.rm=TRUE),
            electricity=weighted.mean(electricity=="False",Weight,na.rm=TRUE),
            pipewater=weighted.mean(pipewater=="False",Weight,na.rm=TRUE),
            cookfuel=weighted.mean(cookfuel!="pipedgas",Weight,na.rm=TRUE),
            heatfuel=weighted.mean(heatfuel!="pipedgas",Weight,na.rm=TRUE),
            hotwater=weighted.mean(hotwater!="pipedgas",Weight,na.rm=TRUE),
            G0451=weighted.mean(G0451/All,Weight,na.rm=TRUE),
            G0452=weighted.mean(G0452/All,Weight,na.rm=TRUE),
            G0453=weighted.mean((G0453+G0454)/All,Weight,na.rm=TRUE),
            G044=weighted.mean(G044/All,Weight,na.rm=TRUE),
            Energy_Share=weighted.mean((G044+G045)/All,Weight,na.rm=TRUE),
            Subsidy=weighted.mean((Subsidy/12)/All,Weight,na.rm=TRUE),
            Food_Share=weighted.mean(G01/All,Weight,na.rm=TRUE),
            Low_Calorie=weighted.mean(Low_Calorie,Weight,na.rm=TRUE),
            Low_Protein=weighted.mean(Low_Protein,Weight,na.rm=TRUE),
            No_Insurance=weighted.mean(No_Insurance,Weight,na.rm=TRUE),
            Area_Per=weighted.mean((area/Size)<16,Weight,na.rm=TRUE),
            House_High_Share=weighted.mean(House_High_Share,Weight,na.rm=TRUE),
            Tech_low=weighted.mean(Tech_low,Weight,na.rm=TRUE),
            Equip_low=weighted.mean(Equip_low,Weight,na.rm=TRUE),
            skeleton=1-weighted.mean(skeleton=="metal" | skeleton=="concrete" |
                                       constmat=="BrickSteel_StoneSteel" | 
                                       constmat=="Brickwood_Stonewood" | 
                                       constmat=="CementBlocks" | 
                                       constmat=="AllBrick_Stone" ,Weight,na.rm=TRUE),
            FoodKCaloriesHH_Per=weighted.mean(FoodKCaloriesHH_Per,Weight,na.rm=TRUE)),by=Decile]
  
  A1[,Year:=year]
  X1 <- rbind(X1,A1)
  
  A2[,Year:=year]
  X2 <- rbind(X2,A2)
  
  A3[,Year:=year]
  X3 <- rbind(X3,A3)

  A4[,Year:=year]
  Province <- rbind(Province,A4)
  
  MD[,Illiterte:=ifelse(HLiterate==FALSE,1,0)]
  MD[,No_knowledge:=ifelse(Knowledge==1 | is.na(Knowledge),0,1)]
  MD[,High_Energy_Share:=ifelse((G044+G045)/All >0.07 ,1,0)]
  MD[,No_electricity:=ifelse(electricity=="False" ,1,0)]
  MD[,No_cookefuel:=ifelse(cookfuel!="pipedgas",1,0)]
  MD[,No_pipewater:=ifelse(pipewater=="False",1,0)]  
  MD[,Small_House:=ifelse((area/Size)<16,1,0)]
  MD[,No_Skeleton:=1-ifelse(skeleton=="metal" | skeleton=="concrete" |
                            constmat=="BrickSteel_StoneSteel" | 
                            constmat=="Brickwood_Stonewood" | 
                            constmat=="CementBlocks" | 
                            constmat=="AllBrick_Stone",1,0)]
  MD[,No_bathroom:=ifelse(bathroom=="False",1,0)]
  MD[,No_ego:=ifelse(ego=="False",1,0)]
  MD[,No_car:=ifelse(car=="False",1,0)]

  
  W_Edu1<-0.1
  W_Edu2<-0.1
  W_Energy1<-1/15
  W_Energy2<-1/15
  W_Energy3<-1/15
  W_Health1<-1/20
  W_Health2<-1/20
  W_Health3<-1/20
  W_Health4<-1/20
  W_House1<-1/20
  W_House2<-1/20
  W_House3<-1/20
  W_House4<-1/20
  W_Equip1<-0
  W_Equip2<-1/15
  W_Equip3<-1/15
  W_Equip4<-1/15
  
  MD[,Poor_Edu:=ifelse(Illiterte==1 | No_knowledge==1,1,0)]
  a<-MD[,weighted.mean(Poor_Edu,Weight),by=ProvinceCode]
  
  MD[,Poor_Energy:=ifelse(High_Energy_Share==1 | No_electricity==1 |
                            No_cookefuel==1,1,0)]
  a<-MD[,weighted.mean(Poor_Energy,Weight),by=ProvinceCode]
  
  MD[,Poor_Health:=ifelse((Low_Calorie==1 & Low_Protein==1) |
                            No_Insurance==1 | No_pipewater==1,1,0)]
  a<-MD[,weighted.mean(Poor_Health,Weight),by=ProvinceCode]
  
  MD[,Poor_House:=ifelse(Small_House==1 | House_High_Share==1 |
                           No_Skeleton==1 | No_bathroom==1,1,0)]
  a<-MD[,weighted.mean(Poor_House,Weight),by=ProvinceCode]
  
  MD[,Poor_Equip:=ifelse(No_car==1 &
                           (Equip_low==1 | Tech_low==1),1,0)]
  a<-MD[,weighted.mean(Poor_Equip,Weight),by=ProvinceCode]
  
  MD[,Poverty_Score:=W_Edu1*Illiterte+W_Edu2*No_knowledge+
                    W_Energy1*High_Energy_Share+W_Energy2*No_electricity+
                    W_Energy3*No_cookefuel+
                    W_Health1*Low_Calorie+W_Health2*Low_Protein+
                    W_Health3*No_Insurance+W_Health4*No_pipewater+
                    W_House1*Small_House+W_House2*House_High_Share+
                    W_House3*No_Skeleton+W_House4*No_bathroom+
                    W_Equip1*No_ego+W_Equip2*No_car+
                    W_Equip3*Equip_low+W_Equip4*Tech_low]
  
  z1<-MD[,.(Illiterte=weighted.mean(Illiterte,Weight)
  ,No_knowledge=weighted.mean(No_knowledge,Weight)
  ,High_Energy_Share=weighted.mean(High_Energy_Share,Weight)
  ,No_electricity=weighted.mean(No_electricity,Weight)
  ,No_cookefuel=weighted.mean(No_cookefuel,Weight)
  ,Low_Calorie=weighted.mean(Low_Calorie,Weight)
  ,Low_Protein=weighted.mean(Low_Protein,Weight)
  ,No_Insurance=weighted.mean(No_Insurance,Weight)
  ,No_pipewater=weighted.mean(No_pipewater,Weight)
  ,Small_House=weighted.mean(Small_House,Weight)
  ,House_High_Share=weighted.mean(House_High_Share,Weight)
  ,No_Skeleton=weighted.mean(No_Skeleton,Weight)
  ,No_bathroom=weighted.mean(No_bathroom,Weight)
  ,No_ego=weighted.mean(No_ego,Weight)
  ,No_car=weighted.mean(No_car,Weight)
  ,Equip_low=weighted.mean(Equip_low,Weight)
  ,Tech_low=weighted.mean(Tech_low,Weight))]
  
  z2<-MD[,.(Illiterte=weighted.mean(Illiterte,Weight)
            ,No_knowledge=weighted.mean(No_knowledge,Weight)
            ,High_Energy_Share=weighted.mean(High_Energy_Share,Weight)
            ,No_electricity=weighted.mean(No_electricity,Weight)
            ,No_cookefuel=weighted.mean(No_cookefuel,Weight)
            ,Low_Calorie=weighted.mean(Low_Calorie,Weight)
            ,Low_Protein=weighted.mean(Low_Protein,Weight)
            ,No_Insurance=weighted.mean(No_Insurance,Weight)
            ,No_pipewater=weighted.mean(No_pipewater,Weight)
            ,Small_House=weighted.mean(Small_House,Weight)
            ,House_High_Share=weighted.mean(House_High_Share,Weight)
            ,No_Skeleton=weighted.mean(No_Skeleton,Weight)
            ,No_bathroom=weighted.mean(No_bathroom,Weight)
            ,No_ego=weighted.mean(No_ego,Weight)
            ,No_car=weighted.mean(No_car,Weight)
            ,Equip_low=weighted.mean(Equip_low,Weight)
            ,Tech_low=weighted.mean(Tech_low,Weight)),by=Region]
  
  z3<-MD[,.(Illiterte=weighted.mean(Illiterte,Weight)
            ,No_knowledge=weighted.mean(No_knowledge,Weight)
            ,High_Energy_Share=weighted.mean(High_Energy_Share,Weight)
            ,No_electricity=weighted.mean(No_electricity,Weight)
            ,No_cookefuel=weighted.mean(No_cookefuel,Weight)
            ,Low_Calorie=weighted.mean(Low_Calorie,Weight)
            ,Low_Protein=weighted.mean(Low_Protein,Weight)
            ,No_Insurance=weighted.mean(No_Insurance,Weight)
            ,No_pipewater=weighted.mean(No_pipewater,Weight)
            ,Small_House=weighted.mean(Small_House,Weight)
            ,House_High_Share=weighted.mean(House_High_Share,Weight)
            ,No_Skeleton=weighted.mean(No_Skeleton,Weight)
            ,No_bathroom=weighted.mean(No_bathroom,Weight)
            ,No_ego=weighted.mean(No_ego,Weight)
            ,No_car=weighted.mean(No_car,Weight)
            ,Equip_low=weighted.mean(Equip_low,Weight)
            ,Tech_low=weighted.mean(Tech_low,Weight)),by=Decile]
  
  z4<-MD[,.(Illiterte=weighted.mean(Illiterte,Weight)
            ,No_knowledge=weighted.mean(No_knowledge,Weight)
            ,High_Energy_Share=weighted.mean(High_Energy_Share,Weight)
            ,No_electricity=weighted.mean(No_electricity,Weight)
            ,No_cookefuel=weighted.mean(No_cookefuel,Weight)
            ,Low_Calorie=weighted.mean(Low_Calorie,Weight)
            ,Low_Protein=weighted.mean(Low_Protein,Weight)
            ,No_Insurance=weighted.mean(No_Insurance,Weight)
            ,No_pipewater=weighted.mean(No_pipewater,Weight)
            ,Small_House=weighted.mean(Small_House,Weight)
            ,House_High_Share=weighted.mean(House_High_Share,Weight)
            ,No_Skeleton=weighted.mean(No_Skeleton,Weight)
            ,No_bathroom=weighted.mean(No_bathroom,Weight)
            ,No_ego=weighted.mean(No_ego,Weight)
            ,No_car=weighted.mean(No_car,Weight)
            ,Equip_low=weighted.mean(Equip_low,Weight)
            ,Tech_low=weighted.mean(Tech_low,Weight)),by=ProvinceCode]
  
  
  MD[,FinalPoor:=ifelse(Poverty_Score>0.2,1,0)]
  MD[,Poverty_Score_Revised:=ifelse(Poverty_Score>0.2,Poverty_Score,0)]
  B1<-MD[,.(H=weighted.mean(FinalPoor,Weight))]
  B2<-MD[,.(H=weighted.mean(FinalPoor,Weight)),by=Region]
  B3<-MD[,.(H=weighted.mean(FinalPoor,Weight)),by=ProvinceCode]
  MD[,weighted.mean(FinalPoor,Weight),by=Decile]
  
  cat(MD[,weighted.mean(FinalPoor,Weight)],"\n")
  
  B1[,Year:=year]
  Y1 <- rbind(Y1,B1)
  
  B2[,Year:=year]
  Y2 <- rbind(Y2,B2)
  
  B3[,Year:=year]
  Y3 <- rbind(Y3,B3)
  #write.csv(Y3,file = "Y3.csv")
  
  C1<-MD[,.(A=sum(Poverty_Score_Revised)/sum(FinalPoor))]
  C2<-MD[,.(A=sum(Poverty_Score_Revised)/sum(FinalPoor)),by=Region]
  C3<-MD[,.(A=sum(Poverty_Score_Revised)/sum(FinalPoor)),by=ProvinceCode]
  
  C1[,Year:=year]
  Z1 <- rbind(Z1,C1)
  
  C2[,Year:=year]
  Z2 <- rbind(Z2,C2)
  
  C3[,Year:=year]
  Z3 <- rbind(Z3,C3)
  
  D1<-MD[,.(MPI=weighted.mean(FinalPoor,Weight)*(sum(Poverty_Score_Revised)/sum(FinalPoor)))]
  D2<-MD[,.(MPI=weighted.mean(FinalPoor,Weight)*(sum(Poverty_Score_Revised)/sum(FinalPoor))),by=Region]
  D3<-MD[,.(MPI=weighted.mean(FinalPoor,Weight)*(sum(Poverty_Score_Revised)/sum(FinalPoor))),by=ProvinceCode]
  
  D1[,Year:=year]
  Q1 <- rbind(Q1,D1)
  
  D2[,Year:=year]
  Q2 <- rbind(Q2,D2)
  
  D3[,Year:=year]
  Q3 <- rbind(Q3,D3)

}



load(file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
D1<-P1[,.(HHID,Age,Literate,Student,Sex)]
D1<-merge(D1,MD[,.(HHID,ProvinceCode,Decile,Weight,Region)],all.x = TRUE)
D1[Age>6,weighted.mean(Literate=="FALSE",Weight,na.rm = TRUE),by="Region"]
x1<-D1[Age>6,weighted.mean(Literate=="FALSE",Weight,na.rm = TRUE),by="ProvinceCode"]
x2<-D1[Age>7,weighted.mean(Literate=="FALSE",Weight,na.rm = TRUE),by="Decile"]

a1<-MD[,weighted.mean(HLiterate==FALSE,Weight),by="ProvinceCode"][order(ProvinceCode)]
a2<-MD[,weighted.mean(Knowledge==0,Weight,na.rm = TRUE),by="ProvinceCode"][order(ProvinceCode)]


X1<-X1[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]
X2<-X2[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]
X3<-X3[Year==90 | Year==91 | Year==92 | Year==93 | Year==94 | 
         Year==95 | Year==96 | Year==97 | Year==98]


png(file="C:/IRHEIS/R/1.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(HLiterate), y=HLiterate, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 22)
dev.off()

png(file="C:/IRHEIS/R/2.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=HLiterate, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/3.png",width=1200, height=600)
ggplot(X3)+
geom_line(mapping = aes(x=Year,y=HLiterate,col=factor(Decile),
linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()

###############################################################################
png(file="C:/IRHEIS/R/4.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Knowledge), y=Knowledge, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/5.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Knowledge, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/6.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Knowledge,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
###############################################################################
###############################################################################
png(file="C:/IRHEIS/R/7.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Energy_Share), y=Energy_Share, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/8.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Energy_Share, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/9.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Energy_Share,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/10.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(electricity), y=electricity, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/11.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=electricity, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/12.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=electricity,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/13.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(cookfuel), y=cookfuel, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/14.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=cookfuel, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/15.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=cookfuel,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
###############################################################################
###############################################################################
png(file="C:/IRHEIS/R/16.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Low_Calorie), y=Low_Calorie, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/17.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Low_Calorie, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/18.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Low_Calorie,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/19.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Low_Protein), y=Low_Protein, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/20.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Low_Protein, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/21.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Low_Protein,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/22.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(No_Insurance), y=No_Insurance, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/23.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=No_Insurance, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/24.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=No_Insurance,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/25.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(pipewater), y=pipewater, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/26.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=pipewater, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/27.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=pipewater,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
###############################################################################
###############################################################################
png(file="C:/IRHEIS/R/28.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Area_Per), y=Area_Per, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/29.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Area_Per, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/30.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Area_Per,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/31.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(skeleton), y=skeleton, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/32.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=skeleton, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/33.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=skeleton,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/34.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(House_High_Share), y=House_High_Share, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/35.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=House_High_Share, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/36.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=House_High_Share,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
###############################################################################
###############################################################################
png(file="C:/IRHEIS/R/37.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(bathroom), y=bathroom, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/38.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=bathroom, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/39.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=bathroom,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/40.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(bathroom), y=ego, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/41.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=ego, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/42.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=ego,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/43.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(car), y=car, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/44.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=car, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/45.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=car,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/46.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Tech_low), y=Tech_low, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/47.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Tech_low, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/48.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Tech_low,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()
###############################################################################
png(file="C:/IRHEIS/R/49.png",width=1200, height=600)
ggplot(X1, aes(fill=factor(Equip_low), y=Equip_low, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
dev.off()

png(file="C:/IRHEIS/R/50.png",width=1200, height=600)
ggplot(X2, aes(fill=factor(Region), y=Equip_low, x=factor(Year))) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + theme_grey(base_size = 25)
#+ geom_text(aes(label=HLiterate), position=position_dodge(width=0.9), vjust=-0.25)
dev.off()

png(file="C:/IRHEIS/R/51.png",width=1200, height=600)
ggplot(X3)+
  geom_line(mapping = aes(x=Year,y=Equip_low,col=factor(Decile),
                          linetype=factor(Decile)))+ theme_grey(base_size = 25)
dev.off()




write.csv(X1,file="X1.csv")
write.csv(X2,file="X2.csv")
write.csv(X3,file="X3.csv")
write.csv(X4,file="X4.csv")
#write.csv(Province,file="Province.csv")

endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat((endtime-starttime)[3])