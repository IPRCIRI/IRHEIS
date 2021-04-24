
rm(list=ls())

Dcile <- data.table()

starttime <- proc.time()
cat("\n\n================ Deciling for normal calculations =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

#library(readxl)
library(data.table)
library(ggplot2)
Dcile<-data.table()
year <- 98
for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  
  # load data --------------------------------------
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  
  ###Nominal- Country
  MD<- MD[order(Region, Total_Exp_Month_Per_nondurable)]  #Deciling in Countrym region(Nominal)
  MD <- MD[,crw2:=cumsum(Weight*Size)/sum(Weight*Size),by=Region]  # Cumulative Relative Weight
  
  MD[,Decile:=cut(crw2,breaks = seq(0,1,.1),labels = 1:10),by=Region]
  MD[,Percentile:=cut(crw2,breaks=seq(0,1,.01),labels=1:100),by=Region]
  MD1 <- MD[,.(Region,crw2,Total_Exp_Month_Per_nondurable,Weight,Size,Decile)]
  MD1 <- MD1[,.(Texp_per_Decile=sum(Total_Exp_Month_Per_nondurable*Weight*Size)/sum(Weight*Size)),by=c("Decile","Region")]
  MD1 <- MD1[, Year:=year]
  Dcile <- rbind(Dcile,MD1)
  
  NRMD <- MD[,.(HHID,Decile,Percentile,Region)]
  colnames(NRMD)[colnames(NRMD) == 'Decile'] <- 'Decile_NonReal'
  cat(MD[,sum(Weight*Size)])
  save(NRMD,file=paste0(Settings$HEISProcessedPath,"Y",year,"Deciles_Nonreal.rda"))
}

NRMD <- NRMD[,t:="??"]

library(writexl)
write_xlsx(Dcile[Region=="Urban",],"E:/decile_exp_96-98U.xlsx")
write_xlsx(Dcile[Region=="Rural",],"E:/decile_exp_96-98R.xlsx")
write_xlsx(NRMD,"E:/NRMD.xlsx")
