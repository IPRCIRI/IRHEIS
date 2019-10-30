#Hist
# Licence: GPL-3
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Hist =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(plotrix)
library(Hmisc)


years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
  P1<-P1[Age<20]
  
  out <- histbackback(split(P1$Age, P1$Sex),ylim(), main="83")
  barplot(-out$left, col="orange" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  barplot(out$right, col="purple", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  
  
}