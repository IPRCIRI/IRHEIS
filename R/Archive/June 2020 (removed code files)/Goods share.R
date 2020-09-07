# Goods share.R
# 
# Copyright © 2019: Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Goods share =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)

PoorsShareResults <- data.table(Year=NA_integer_,Food_Share=NA_real_,Cigar_Share=NA_real_,
                                cloth_Share=NA_real_,House_Share=NA_real_,Furniture_Share=NA_real_,
                                Hygiene_Share=NA_real_,Transportation_Share=NA_real_,
                                Communication_Share=NA_real_,Amusement_Share=NA_real_,
                                HotelRestaurant_Share=NA_real_,Other_Share=NA_real_,
                                Durable_Share=NA_real_,Meter_Price=NA_real_,
                                cluster3=NA_integer_)[0]

ShareResults <- data.table(Year=NA_integer_,Share=NA_real_,Type=NA_character_,
                           cluster3=NA_integer_)[0]

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  MD<-MD[FinalPoor==0]
  #MD<-MD[Region=="Rural" & NewArea_Name=="Sistan"]
  
 

  ################Cluster##################
  X1 <- MD[,.(Food_Share=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
              Cigar_Share=weighted.mean(Cigar_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              cloth_Share=weighted.mean(Cloth_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              House_Share=weighted.mean(HouseandEnergy_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Meter_Price=weighted.mean(MetrPrice,Weight,na.rm = TRUE),
              Furniture_Share=weighted.mean(Furniture_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Hygiene_Share=weighted.mean(Hygiene_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Transportation_Share=weighted.mean(Transportation_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Communication_Share=weighted.mean(Communication_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Amusement_Share=weighted.mean(Amusement_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              HotelRestaurant_Share=weighted.mean(HotelRestaurant_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Other_Share=weighted.mean(Other_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Durable_Share=weighted.mean(Durable_Exp/Total_Exp_Month,Weight,na.rm = TRUE)),by=cluster3]

  X1[,Year:=year]

  PoorsShareResults <- rbind(PoorsShareResults,X1)
  
  

  X2 <- MD[,.(Share=weighted.mean(FoodExpenditure/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Food"),by=cluster3]
  X2[,Year:=year]
  ShareResults <- rbind(ShareResults,X2)
  
  X3 <- MD[,.(Share=weighted.mean(Cigar_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Cigar"),by=cluster3]
  X3[,Year:=year]
  ShareResults <- rbind(ShareResults,X3)
  
  X4 <- MD[,.(Share=weighted.mean(Cloth_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Cloth"),by=cluster3]
  X4[,Year:=year]
  ShareResults <- rbind(ShareResults,X4)
  
  X5 <- MD[,.(Share=weighted.mean(HouseandEnergy_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="HouseandEnergy"),by=cluster3]
  X5[,Year:=year]
  ShareResults <- rbind(ShareResults,X5)
  
  X6 <- MD[,.(Share=weighted.mean(Furniture_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Furniture"),by=cluster3]
  X6[,Year:=year]
  ShareResults <- rbind(ShareResults,X6)
  
  X7 <- MD[,.(Share=weighted.mean(Transportation_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Transportation"),by=cluster3]
  X7[,Year:=year]
  ShareResults <- rbind(ShareResults,X7)
  
  X8 <- MD[,.(Share=weighted.mean(Communication_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Communication"),by=cluster3]
  X8[,Year:=year]
  ShareResults <- rbind(ShareResults,X8)
  
  X9 <- MD[,.(Share=weighted.mean(Amusement_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Amusement"),by=cluster3]
  X9[,Year:=year]
  ShareResults <- rbind(ShareResults,X9)
  
  X10 <- MD[,.(Share=weighted.mean(Hygiene_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="Hygiene"),by=cluster3]
  X10[,Year:=year]
  ShareResults <- rbind(ShareResults,X10)
  
  X11 <- MD[,.(Share=weighted.mean(HotelRestaurant_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
              Type="HotelRestaurant"),by=cluster3]
  X11[,Year:=year]
  ShareResults <- rbind(ShareResults,X11)
  
  X12 <- MD[,.(Share=weighted.mean(Other_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
               Type="Other"),by=cluster3]
  X12[,Year:=year]
  ShareResults <- rbind(ShareResults,X12)
  
  X13 <- MD[,.(Share=weighted.mean(Durable_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
               Type="Durable"),by=cluster3]
  X13[,Year:=year]
  ShareResults <- rbind(ShareResults,X13)
  
  X14 <- MD[,.(Share=weighted.mean(Medical_Exp/Total_Exp_Month,Weight,na.rm = TRUE),
               Type="Medical"),by=cluster3]
  X14[,Year:=year]
  ShareResults <- rbind(ShareResults,X14)
}

save(PoorsShareResults,file=paste0(Settings$HEISProcessedPath,"PoorsShareResults.rda"))
save(ShareResults,file=paste0(Settings$HEISProcessedPath,"ShareResults.rda"))

ggplot(PoorsShareResults)+
  geom_line(mapping = aes(x=Year,y=Meter_Price,col=factor(cluster3)))

ggplot(ShareResults, aes(fill=Type, y=Share, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

ShareResults7<-ShareResults[cluster3==13]

ggplot(ShareResults7, aes(fill=Type, y=Share, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Goods shares in clusters")

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")