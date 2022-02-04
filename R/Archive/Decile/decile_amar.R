rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Decile Data =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)
library(compare)
library(acid)

MDq <- data.table()
g1 <- data.table()
g1_u <- data.table()
g1_r <- data.table()
year <-98
for(year in (Settings$startyear:Settings$endyear)){

    cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  #load Demos+FoodPrices+Weights
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHBase.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"lactating.rda"))
  HHBase<-merge(HHBase,lactating,by="HHID")
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Calorie_Need.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHI.rda"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"BigFoodPrice.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Value.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Esghat_Value.rda"))
  
  load(file=paste0(Settings$HEISWeightsPath,Settings$HEISWeightFileName,year,".rda"))
  HHWeights<- as.data.table(HHWeights)
  HHWeights<-HHWeights[,HHID:=as.numeric(HHID)]
  HHWeights[,Year:=NULL]
  
  Value<-merge(Value,HHHouseProperties)
  Value<-merge(Value,HHWeights)
  Value[,Year:=year]
  Value<-merge(Value,Esghat_Value,by="Year")
  Value[,Year:=NULL]
  
  
  A1<- Value[`71111`+`71112`+`71116`+`71117`>0,
             weighted.mean(`71111`+`71112`+`71116`+
                             `71117`,Weight)]
  A2<-Value[`91128`+`91129`>0,weighted.mean(`91128`+`91129`,Weight)]
  A3<-Value[`53112`>0,weighted.mean(`53112`,Weight)]
  A4<-Value[`53116`>0,weighted.mean(`53116`,Weight)]
  A5<-Value[`53113`>0,weighted.mean(`53113`,Weight)]
  A6<-Value[`82113`>0,weighted.mean(`82113`,Weight)]
  A7<-Value[`53125`>0,weighted.mean(`53125`,Weight)]
  A8<-Value[`91311`>0,weighted.mean(`91311`,Weight)]
  A9<-Value[`72111`>0,weighted.mean(`72111`,Weight)]
  if (year!=92){
    A10<-Value[`72118`>0,weighted.mean(`72118`,Weight)]
  }
  A11<-Value[`72319`>0,weighted.mean(`72319`,Weight)]
  
  Value[car=="True",Added1:=A1-Auto_Sale] ### We use 0.05 instead of 0.1
  Value[tvcr=="True",Added2:=A2-TV_Sale]
  Value[freezer=="True" | frez_refrig=="True" | refrigerator=="True",
        Added3:=A3-yakhchal_Sale]
  Value[oven=="True",Added4:=A4-ojaghgaz_Sale]
  Value[washer=="True",Added5:=A5-lebasshooyi_Sale]
  Value[cellphone=="True",Added6:=A6-Mobile_Sale]
  Value[cooler_gas=="True",Added7:=A7-Coolergazi_Sale]
  Value[computer=="True",Added8:=A8-PC_Sale]
  Value[car=="True",Added9:=A9-lastik_Sale]
  if (year!=92){
    Value[car=="True",Added10:=A10]
  }
  Value[car=="True",Added11:=A11]
  
  
  
  
  
  Value[,Weight:=NULL]
  if (year!=92){
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111", "72118","72319")
  }
  if (year==92){
    dep <- c( "71111", "71112","71116", "71117",
              "91128", "91129","53112", "53116",
              "53113", "82113","53125", "91311",
              "72111","72319")
  }
  
  Value[, Total_Depreciated_Durable := Reduce(`+`, .SD), .SDcols=dep]
  #Value[is.na(Value)] <- 0
  
  if (year!=92){
    Value[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
            Added7+Added8+Added9+Added10+Added11]
  }
  if (year==92){
    Value[,Added:=Added1+Added2+Added3+Added4+Added5+Added6+
            Added7+Added8+Added9+Added11]
  }
  #load Expenditures
  
  for(G in c("Foods","Cigars","Cloths","Amusements","Communications",
             "Durables", "Education", "Furnitures","HotelRestaurants",
             "HouseandEnergys","House", "Medicals","Hygienes","Transportations","Others",
             "Resturants"
             ,"Durablele_Detail"
  )){
    load(file=paste0(Settings$HEISProcessedPath,"Y",year,G,".rda"))
    
  }
  
  # load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Added_Food.rda")) 
  
  #load Calories
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Food_Calories.rda"))
  FData[,Region:=NULL]
  #for (col in c("FoodKCaloriesHH")) FData[is.na(get(col)), (col) := 0]
  FData <- FData[FoodKCaloriesHH>0]
  
  #merge groups
  MD<-merge(HHBase,HHI ,by =c("HHID"),all=TRUE)
  FData[,Size:=NULL]
  MD<-merge(MD,FData ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HHWeights ,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FoodData,by =c("HHID"),all=TRUE)
  # MD<-merge(MD,Added_Food,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CigarData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ClothData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,AmusementData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,CommunicationData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,EducData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseandEnergyData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HouseData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,FurnitureData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HotelRestaurantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,HygieneData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,TransportationData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BenzinData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,GazData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,BarghData,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,NaftSefidData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,OtherData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,MedicalData,by =c("HHID"),all=TRUE)
  # MD<-merge(MD,NonFreeDurableData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,ResturantData,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Durablele_Detail,by =c("HHID"),all=TRUE)
  MD<-merge(MD,Value,by =c("HHID"),all=TRUE)
  #MD<-merge(MD,InvestmentData,by =c("HHID"),all=TRUE)
  for (col in c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp", "Cloth_Exp", "Amusement_Exp", 
                "Communication_Exp", "Education_Exp", "HouseandEnergy_Exp", 
                "Furniture_Exp", "HotelRestaurant_Exp", "Hygiene_Exp", "Transportation_Exp",
                "Other_Exp", "Medical_Exp", 
                "Resturant_Exp","Durable_Exp", "Durable_Pure_Exp"
                ,"Add_to_NonDurable","Durable_Dep",
                "Durable_NoDep","Durable_Emergency","Total_Depreciated_Durable"
  )) 
    MD[is.na(get(col)), (col) := 0]
  #  MD<-MD[,Yaraneh:=416000*Size]
  
  MD<-merge(MD,Calorie_Need)
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Durablele_Detail.rda"))
  #MD<-merge(MD,Durablele_Detail)
  
  
  
  
  #  "", ,"ServiceExp"
  #,"","Durable_Dep",  "","",""
  
  
  #Calculate Monthly Total Expenditures 
  d <- c("OriginalFoodExpenditure",
          "FoodOtherExpenditure", 
          "Cigar_Exp", "Cloth_Exp",
          "HouseandEnergy_Exp", "Furniture_Exp", "Hygiene_Exp", "Medical_Exp", "Transportation_Exp",
          "Communication_Exp", "Amusement_Exp",  "HotelRestaurant_Exp", "Other_Exp"
          #,"Added"
          )
  nd <- c("Durable_Pure_Exp"
         ,"Education_Exp"  
         )
  f <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp")
  
  
  h <- c("HouseandEnergy_Exp")
  furn <- c("Furniture_Exp")
  b<- c( "Hygiene_Exp","Medical_Exp")
  zirs <- c("Transportation_Exp","Communication_Exp")
           #"",         # "", "Amusement_Exp",  "HotelRestaurant_Exp", "Other_Exp"
          #,"Added"

  # w <- c(nw, "Medical_Exp", "NonFreeDurable_Exp")
  #w <- c(nw, "Medical_Exp", "Durable_Pure_Exp")
  # pw <- c(nw, "Added_Food_Exp_Month")
  #Lw <- c(pw,  "Medical_Exp", "Durable_Exp")
  
  MD[, Total_Exp_Month_ND := Reduce(`+`, .SD), .SDcols=nd]
  MD[, Total_Exp_Month_durable := Reduce(`+`, .SD), .SDcols=d]
  MD[, Total_Exp_Month_food := Reduce(`+`, .SD), .SDcols=f]
  MD[, Total_Exp_Month_house := Reduce(`+`, .SD), .SDcols=h]
  MD[, Total_Exp_Month_furniture := Reduce(`+`, .SD), .SDcols=furn]
  MD[, Total_Exp_Month_behdasht := Reduce(`+`, .SD), .SDcols=b]
  MD[, Total_Exp_Month_zirs := Reduce(`+`, .SD), .SDcols=zirs]
  #cat(MD[,weighted.mean(Total_Exp_Month,Weight)])
#  cat(MD[,weighted.mean(Total_Consumption_Month,Weight)])
  MD<-merge(MD,HHWeights)
  MD1 <- MD[,.(HHID,Total_Exp_Month_ND,Total_Exp_Month_durable,Weight,Region,Size, Durable_Exp,
                Total_Exp_Month_ND ,Total_Exp_Month_durable, Total_Exp_Month_food , Total_Exp_Month_house,
                Total_Exp_Month_furniture , Total_Exp_Month_behdasht, Total_Exp_Month_zirs
                ,NewArea,NewArea_Name, Total_Exp_Month_food,Total_Exp_Month_house)]
  MD1 <- MD1[,Texp:=(Total_Exp_Month_ND+Total_Exp_Month_durable)*12]
  MD1 <- MD1[,TFexp:=Total_Exp_Month_food*12]
  
    MD1 <- MD1[order(Region, Texp)]  # I removed Region from ordering, deciling is not divided 
    #into rural/urban (M.E. 5/11/2020)
    
  
  MD1 <- MD1[,crw:=cumsum(Weight)/sum(Weight),by=Region]  # Cumulative Relative Weight
  MD1 <- MD1[,crexp:=cumsum(Texp*Weight)/sum(Texp*Weight),by=Region]  # Cumulative Relative Weight
  MD1 <- MD1[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  MD1 <- MD1[,Decile_amar:=Decile]
  MD_amar <- MD1[,.(HHID,Region,Decile_amar,Texp)]
  cat(MD1[,sum(Weight*Size)])
  save(MD_amar,file=paste0(Settings$HEISProcessedPath,"Y",year,"Decile_amar.rda"))
  
  IncomeTable <- data.table()
  IncomeTable<- MD1[]
  IncomeTable<-IncomeTable[!is.na(Weight)]
  g <- data.table()
  g <- IncomeTable[,weighted.gini(Texp,Weight)]
  g <- g[,Year:=year]
  g_r <- data.table()
  g_r <- IncomeTable[Region=="Rural",weighted.gini(Texp,Weight)]
  g_r <- g_r[,Region:="Rural"]
  g_r <- g_r[,Year:=year]
  g_u <- data.table()
  g_u <- IncomeTable[Region=="Urban",weighted.gini(Texp,Weight)]
  g_u <- g_u[,Region:="Urban"]
  g_u <- g_u[,Year:=year]
  g1 <- rbind(g1,g)
  g1_u <- rbind(g1_u,g_u)
  g1_r <- rbind(g1_r,g_r)
  MD1 <- MD1[,Exp_Reg:=weighted.mean(Texp,Weight)/10^6, by=c("Region")]
  MD1 <- MD1[,Exp_h_Reg:=weighted.mean(Total_Exp_Month_house,Weight)*12/10^6, by=c("Region")]
  MD1 <- MD1[order(Region, NewArea_Name)]
  MD1 <- MD1[,Exp_f_Reg:=weighted.mean(TFexp,Weight)/10^6, by=c("Region")]
  MD1 <- MD1[,food_ratio_Reg:=Exp_f_Reg/Exp_Reg]
  MD1 <- MD1[,hous_ratio_Reg:=Exp_h_Reg/Exp_Reg]
  MD1 <- MD1[,Exp_decile:=weighted.mean(Texp ,Weight) , by=c("Region", "Decile") ]
  MD1 <- MD1[,Exp_f_decile:=weighted.mean(TFexp ,Weight) , by=c("Region", "Decile") ]
  MD1 <- MD1[,Exp_h_decile:=weighted.mean(Total_Exp_Month_house ,Weight) , by=c("Region", "Decile") ]
  MD2 <- MD1[,.(Exp_decile, Decile,Region,Exp_f_decile,Exp_h_decile,food_ratio_Reg,hous_ratio_Reg)]
  MD2 <- unique(MD2)
  MD2 <- MD2[,food_ratio:=Exp_f_decile/Exp_decile]
  MD2 <- MD2[,hous_ratio:=Exp_h_decile/Exp_decile*12]
  MD2 <- MD2[, Year:=year]
  MDq <- rbind(MD2,MDq)
  #MDq <- merge(MDq,g,by=c("Year"))
  #MD2 <- MD2[,gini1:=weighted.gini(Exp_decile,Weight )]
  #MD1 <- MD1[,Exp_decile:=weighted.mean(Texp,Weight), by=c(Region,Decile)]
  
  #cat(MD1[Region=="Urban",weighted.mean(Exp_f_Reg,Weight,na.rm = TRUE)],"\t")
   # cat(MD1[Region=="Urban",weighted.mean(Exp_Reg,Weight,na.rm = TRUE)],"\n")
  
  
  #cat(MD[,weighted.mean(Added1,Weight,na.rm = TRUE)],"\n")
    

  }

colnames(g1_u)[colnames(g1_u) == 'Gini'] <- 'Gini_u'
colnames(g1_r)[colnames(g1_r) == 'Gini'] <- 'Gini_r'
Final <- data.table()
Final <- merge(MDq,g1[,.(Gini,Year)])
Final <- merge(Final,g1_u[,.(Gini_u,Year,Region)],by=c("Year","Region"),all =TRUE)
Final <- merge(Final,g1_r[,.(Gini_r,Year,Region)],by=c("Year","Region"),all =TRUE)

library(writexl)
write_xlsx(Final,"E:/decile_exp.xlsx")

