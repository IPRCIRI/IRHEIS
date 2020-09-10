
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ POoRS =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)
library(stats)
library(spatstat)
library(readxl)
library(sm)
Eduyears<- data.table(Year=NA_integer_,cluster3=NA_integer_,H_lit_np=NA_integer_ ,
                      H_lit_p=NA_integer_,Region=NA_integer_,
                      ED_Exp_p=NA_integer_,ED_Exp_np=NA_integer_)


for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  #year=97
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))
  
  Edu_k<- MD[NKids>0,.(.N,ED_Exp=weighted.mean(Education_Exp,Weight)),by=.(Region,cluster3)]
  Edu_poor<- MD[FinalPoor==1&NKids>0,.(.N,H_lit_p=weighted.mean(HLiterate,Weight),
                               ED_Exp_p=weighted.mean(Education_Exp/Total_Exp_Month,Weight)),by=.(Region,cluster3)]
  Edu_Npoor<- MD[FinalPoor==0&NKids>0,.(.N,H_lit_np=weighted.mean(HLiterate,Weight),
                                ED_Exp_np=weighted.mean(Education_Exp/Total_Exp_Month,Weight)),by=.(Region,cluster3)]
  Edu <-merge(Edu_poor,Edu_Npoor)
  Edu <-merge(Edu,Edu_k)
  Eduy <- Edu
  Eduy <- Eduy[,.(H_lit_p,H_lit_np,ED_Exp_np,ED_Exp_p,Region,cluster3)]
  Eduy <- Eduy[,Year:=year]
  
  Eduyears <- rbind(Eduyears,Eduy)
  
  }
ggplot(Eduyears)+
  geom_line(mapping = aes(x=Year,y=H_lit_p,col=factor(cluster3)))

ggplot(Eduyears)+
  geom_line(mapping = aes(x=Year,y=ED_Exp_np,col=factor(cluster3)))
pol...
ggplot(Eduyears)+
  geom_line(mapping = aes(x=Year,y=ED_Exp_p,col=factor(cluster3)))
